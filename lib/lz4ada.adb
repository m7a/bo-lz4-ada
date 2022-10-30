with Ada.Text_IO; -- TODO DEBUG ONLY
with XXHash;

package body LZ4Ada is

	-- Stream_Element_Array based functions --

	function Init(Input: in Ada.Streams.Stream_Element_Array;
			Num_Consumed: out Integer) return Decompressor is
		Input_Conv: Octets(0 .. Input'Length - 1);
		for Input_Conv'Address use Input'Address;
	begin
		return Init(Input_Conv, Num_Consumed);
	end Init;

	function Update(Ctx: in out Decompressor;
				Input: in Ada.Streams.Stream_Element_Array;
				Num_Consumed: out Integer;
				Output: out Ada.Streams.Stream_Element_Array;
				Num_Produced: out Integer) return Boolean is
		Input_Conv: Octets(0 .. Input'Length - 1);
		for Input_Conv'Address use Input'Address;
		Output_Conv: Octets(0 .. Output'Length - 1);
		for Output_Conv'Address use Output'Address;
	begin
		return Ctx.Update(Input_Conv, Num_Consumed, Output_Conv,
								Num_Produced);
	end Update;

	-- Octets based functions --

	function Init(Input: in Octets; Num_Consumed: out Integer)
							return Decompressor is
		Magic_NB:              constant U32 := Load_32(Input(Input'First
							.. Input'First + 3));
		FLG:                   constant U8 := Input(Input'First + 4);
		BD:                    constant U8 := Input(Input'First + 5);
		FLG_Block_Checksum:    constant Boolean := ((FLG and 16) /= 0);
		FLG_Content_Size:      constant Boolean := ((FLG and  8) /= 0);
		FLG_Content_Checksum:  constant Boolean := ((FLG and  4) /= 0);
		FLG_Reserved:          constant Boolean := ((FLG and  2) /= 0);
		FLG_Dictionary_ID:     constant Boolean := ((FLG and  1) /= 0);
		FLG_Version:           constant U8      := Shift_Right(FLG and
								16#c0#, 6);
		Block_Checksum_Size:   constant Integer :=
					(if FLG_Block_Checksum   then 4 else 0);
		Content_Checksum_Size: constant Integer :=
					(if FLG_Content_Checksum then 4 else 0);

		BD_Block_Max_Size: constant U8 := Shift_Right(BD and 16#70#, 4);
		BD_Has_Reserved:   constant Boolean := ((BD and 16#8f#) /= 0);

		Cursor:          Integer := Input'First + 6;
		Block_Max_Size:  Integer;
		Detected_Format: Format;
		HC:              U8; -- header checksum byte
		Computed_HC:     U8;
	begin
		case Magic_NB is
		when 16#184d2204# => Detected_Format := Modern;
		-- TODO EFFECTIVELY UNSUPPORTED AT THE MOMENT!
		--when 16#184c2102# => Detected_Format := Legacy;
		when others       => raise Constraint_Error with
					"Invalid or unsupported magic: " &
					U32'Image(Magic_NB);
		end case;
		if FLG_Version /= 1 then
			raise Constraint_Error with
				"Only LZ4 frame format version 01 supported. " &
				"Detected " & U8'Image(FLG_Version) &
				" instead.";
		end if;
		if FLG_Reserved or BD_Has_Reserved then
			raise Constraint_Error with
				"Found reserved bits /= 0. Data might be too " &
				"new to be processed by this implementation!";
		end if;
		case BD_Block_Max_Size is
		when 4 => Block_Max_Size :=       64 * 1024; -- 64 KiB
		when 5 => Block_Max_Size :=      256 * 1024; -- 256 KiB
		when 6 => Block_Max_Size :=     1024 * 1024; -- 1 MiB
		when 7 => Block_Max_Size := 4 * 1024 * 1024; -- 4 MiB
		-- TODO LEGACY FORMAT HAS 8MIB
		when others => raise Constraint_Error with
					"Unknown maximum block size flag: " &
					U8'Image(BD_Block_Max_Size);
		end case;

		-- skip over content size
		if FLG_Content_Size then
			Cursor := Cursor + 8;
		end if;

		-- skip over dictionary ID
		if FLG_Dictionary_ID then
			Cursor := Cursor + 4;
		end if;

		HC := Input(Cursor);
		Computed_HC := Shift_Right(U8(XXHash.XXH32(Input(Input'First ..
						Cursor - 1), 0) and 16#ff#), 8);
		if HC /= Computed_HC then
			raise Checksum_Error with "Computed Header Checksum " &
				U8'Image(Computed_HC) & " does not match " &
				"expected Header Checksum " & U8'Image(HC);
		end if;
		Num_Consumed := Cursor - Input'First + 1;
		return (
			Is_Format               => Detected_Format,
			-- Per spec, the declared size excludes block size +
			-- optional checksum fields. Hence add their sizes here!
			Size_Last               => Block_Max_Size +
							Block_Checksum_Size +
							Block_Size_Bytes - 1,
			Block_Checksum_Length   => Block_Checksum_Size,
			Content_Checksum_Length => Content_Checksum_Size,
			Is_At_End_Mark          => False,
			Input_Buffer            => (others => 0),
			Input_Buffer_Filled     => 0,
			Input_Length            => -1,
			Is_Compressed           => (Detected_Format = Legacy),
			History                 => (others => 0),
			History_Pos             => 0
		);
	end Init;

	function Update(Ctx: in out Decompressor;
			Input:  in  Octets; Num_Consumed: out Integer;
			Output: out Octets; Num_Produced: out Integer)
			return Boolean is
	begin
		Num_Consumed := 0;
		Num_Produced := 0;
		if Ctx.Is_At_End_Mark then
			return Ctx.Check_End_Mark(Input, Num_Consumed);
		end if;
		if Ctx.Input_Length = -1 then
			Ctx.Try_Detect_Input_Length(Input, Num_Consumed);

			if Ctx.Is_At_End_Mark then
				return Ctx.Check_End_Mark(Input, Num_Consumed);
			end if;

			if Ctx.Input_Length /= -1 then
				Ctx.Handle_Newly_Known_Input_Length(
					Input, Num_Consumed,
					Output, Num_Produced
				);
			end if;
		else
			Ctx.Cache_Data_And_Process_If_Full(Input, Num_Consumed,
							Output, Num_Produced);
		end if;
		return False;
	end Update;

	function Check_End_Mark(Ctx: in out Decompressor; Input: in Octets;
				Num_Consumed: in out Integer) return Boolean is
		Provided_Input_Length: constant Integer :=
			Input'Length - Num_Consumed;
		Required_Input_Length: constant Integer :=
			Ctx.Content_Checksum_Length - Ctx.Input_Buffer_Filled;
	begin
		if Ctx.Content_Checksum_Length = 0 or
						Required_Input_Length <= 0 then
			return True;
		elsif Provided_Input_Length >= Required_Input_Length then
			declare
				Checksum: constant U32 := Load_32(
					Ctx.Input_Buffer(0 ..
						Ctx.Input_Buffer_Filled - 1) &
					Input(Num_Consumed .. Num_Consumed +
						Required_Input_Length - 1)
				);
			begin
				Num_Consumed := Num_Consumed +
							Required_Input_Length;
				-- TODO CHECK THE CHECKSUM HERE
				Ada.Text_IO.Put_Line("TODO CANNOT CHECK: CONTENT CHECKSUM = " & U32'Image(Checksum));
			end;
			return True;
		else
			Ctx.Input_Buffer(Ctx.Input_Buffer_Filled ..
					Ctx.Input_Buffer_Filled +
					Provided_Input_Length - 1) :=
				Input(Input'First + Num_Consumed .. Input'Last);
			Ctx.Input_Buffer_Filled := Ctx.Input_Buffer_Filled +
							Provided_Input_Length;
			Num_Consumed := Num_Consumed + Provided_Input_Length;
			return False;
		end if;
	end Check_End_Mark;

	procedure Try_Detect_Input_Length(Ctx: in out Decompressor;
			Input: in Octets; Num_Consumed: in out Integer) is
		Additional_Length: constant Integer :=
				Block_Size_Bytes + Ctx.Block_Checksum_Length;
		Length_Word: U32;
	begin
		Num_Consumed := Min(Block_Size_Bytes - Ctx.Input_Buffer_Filled,
					Input'Length);
		Ctx.Input_Buffer(Ctx.Input_Buffer_Filled ..
			Ctx.Input_Buffer_Filled + Num_Consumed - 1) :=
			Input(Input'First .. Input'First + Num_Consumed - 1);
		Ctx.Input_Buffer_Filled :=
			Ctx.Input_Buffer_Filled + Num_Consumed;

		if Ctx.Input_Buffer_Filled = Block_Size_Bytes then
			Length_Word := Load_32(Ctx.Input_Buffer(0 ..
							Block_Size_Bytes - 1));
			if Ctx.Is_Format = Modern and Length_Word = 0 then
				Ctx.Is_At_End_Mark      := True;
				Ctx.Input_Buffer_Filled := 0;
				return;
			end if;
			if Ctx.Is_Format = Modern then
				Ctx.Is_Compressed := (
					(Length_Word and 16#80000000#) /= 0);
				Length_Word := Length_Word and 16#7ffffff#;
			end if;
			Ctx.Input_Length := Integer(Length_Word);
			if (Ctx.Input_Length + Additional_Length) >
						Ctx.Input_Buffer'Length then
				Ctx.Input_Length := -1;
				raise Constraint_Error with
					"Declared maximum data length " &
					"exceeded. Buffer has " &
					Integer'Image(Ctx.Input_Buffer'Length) &
					" bytes, current block requires " &
					Integer'Image(Ctx.Input_Length) &
					" bytes + " &
					Integer'Image(Additional_Length) &
					" bytes for metadata.";
			end if;
		end if;
	end Try_Detect_Input_Length;

	procedure Handle_Newly_Known_Input_Length(Ctx: in out Decompressor;
			Input: in Octets; Num_Consumed: in out Integer;
			Output: out Octets; Num_Produced: in out Integer) is
		Total_Length: constant Integer :=
				Ctx.Input_Length + Ctx.Block_Checksum_Length;
	begin
		if (Input'Length - Num_Consumed) >= Total_Length then
			-- Supplied input is large enough to process right away
			-- => no copying
			Ctx.Decode_Full_Block_With_Trailer(Input(Input'First +
				Num_Consumed .. Input'First + Num_Consumed +
						Total_Length - 1),
				Output,
				Num_Produced
			);
			Num_Consumed := Num_Consumed + Ctx.Input_Length +
						Ctx.Block_Checksum_Length;
			Ctx.Input_Buffer_Filled := 0;
			Ctx.Input_Length        := -1;

			-- if output has entire block of free space consider
			-- re-running. optionally: else consider reading ahead
			-- length + re-running on smaller if available. possibly
			-- not a good idea as this may not improve performance
			-- in any meaningful way and callers need to do an
			-- outer loop anyways?
		else
			Ctx.Cache_Data_And_Process_If_Full(Input, Num_Consumed,
						Output, Num_Produced);
		end if;
	end Handle_Newly_Known_Input_Length;

	procedure Cache_Data_And_Process_If_Full(Ctx: in out Decompressor;
			Input: in Octets; Num_Consumed: in out Integer;
			Output: out Octets; Num_Produced: in out Integer) is
		Input_Avail: constant Integer := Input'Length - Num_Consumed;
		Input_Want: constant Integer := Ctx.Input_Length +
						Ctx.Block_Checksum_Length -
						Ctx.Input_Buffer_Filled ;
	begin
		if Input_Want > Input_Avail then
			Ctx.Input_Buffer(Ctx.Input_Buffer_Filled ..
				Ctx.Input_Buffer_Filled + Input_Avail - 1) :=
				Input(Input'First + Num_Consumed .. Input'Last);
			Num_Consumed := Num_Consumed + Input_Avail;
		else
			Ctx.Decode_Full_Block_With_Trailer(
				Ctx.Input_Buffer(0 ..
					Ctx.Input_Buffer_Filled - 1) &
				Input(Input'First + Num_Consumed ..
					Input'First + Num_Consumed +
					Input_Want - 1),
				Output, Num_Produced
			);
			Num_Consumed := Num_Consumed + Input_Want;
			Ctx.Input_Buffer_Filled := 0;
			Ctx.Input_Length := -1;
		end if;
	end Cache_Data_And_Process_If_Full;

	procedure Decode_Full_Block_With_Trailer(Ctx: in out Decompressor;
			Input_Block: in Octets; Output: out Octets;
			Num_Produced: in out Integer) is
		Raw_Data: constant Octets := Input_Block(Input_Block'First ..
				Input_Block'Last - Ctx.Block_Checksum_Length);
	begin
		if Ctx.Block_Checksum_Length > 0 then
			Check_Checksum(Raw_Data, Load_32(Input_Block(
				Input_Block'Last - Ctx.Block_Checksum_Length + 1
				.. Input_Block'Last)));
		end if;

		if Ctx.Is_Compressed then
			Ctx.Decompress_Full_Block(Raw_Data, Output,
								Num_Produced);
		else
			Output(Output'First + Num_Produced .. Output'First +
				Num_Produced + Raw_Data'Length - 1) := Raw_Data;
			Num_Produced := Num_Produced + Raw_Data'Length;
		end if;
	end Decode_Full_Block_With_Trailer;

	procedure Check_Checksum(Data: in Octets; Expect_Checksum: in U32) is
		Compute_Checksum: constant U32 := XXHash.XXH32(Data, 0);
	begin
		if Compute_Checksum /= Expect_Checksum then
			raise Checksum_Error with
				"Declared checksum is " & U32'Image(
				Expect_Checksum) & ", but computed one is " &
				U32'Image(Compute_Checksum) & ".";
		end if;
	end Check_Checksum;

	procedure Decompress_Full_Block(Ctx: in out Decompressor;
					Raw_Data: in Octets; Output: out Octets;
					Num_Produced: in out Integer) is
		Has_Reached_End: Boolean := False;
		Idx:             Integer := Raw_Data'First;

		procedure Process_Variable_Length(Var: in out Integer) is
			Tmp: U8;
		begin
			if Var = 15 then
				loop
					Tmp := Raw_Data(Idx);
					Var := Var + Integer(Tmp);
					Idx := Idx + 1;
					exit when Tmp /= 255;
				end loop;
			end if;
		end Process_Variable_Length;

		procedure Decompress_Sequence is
			Token: constant U8 := Raw_Data(Idx);
			Num_Literals: Integer := Integer(Shift_Right(Token and
								16#f0#, 4));
			Match_Length: Integer := Integer(Token and 16#0f#);
			Offset: Integer;
		begin
			Idx := Idx + 1;
			-- Literals
			Process_Variable_Length(Num_Literals);
			if Num_Literals > 0 then
				Ctx.Write_Output(Output, Num_Produced, Raw_Data(
						Idx .. Idx + Num_Literals - 1));
				Idx := Idx + Num_Literals;
			end if;
			if Idx > Raw_Data'Last then
				Has_Reached_End := True;
				return;
			end if;
			-- Match Copy
			Offset := Integer(U32(Raw_Data(Idx)) or
					Shift_Left(U32(Raw_Data(Idx + 1)), 8));
			Idx := Idx + 2;
			if Offset = 0 then
				raise Constraint_Error with
					"Corrupted Block: Offset = 0 detected.";
			end if;
			Process_Variable_Length(Match_Length);
			-- +4 for minmatch
			Ctx.Output_With_History(Output, Num_Produced, Offset,
							Match_Length + 4);
		end Decompress_Sequence;
	begin
		while Idx <= Raw_Data'Last and not Has_Reached_End loop
			Decompress_Sequence;
		end loop;
		if (Idx > Raw_Data'Last) /= Has_Reached_End then
			raise Constraint_Error with
				"Invalid block end: May not have processed " &
				"all data: Idx = " & Integer'Image(Idx) &
				", Last = " & Integer'Image(Raw_Data'Last) &
				", Has_Reached_End = " &
				Boolean'Image(Has_Reached_End);
		end if;
	end Decompress_Full_Block;

	procedure Write_Output(Ctx: in out Decompressor; Output: in out Octets;
			Num_Produced: in out Integer; Data: in Octets) is
	begin
		Output(Output'First + Num_Produced ..
			Output'First + Num_Produced + Data'Length - 1) := Data;
		Num_Produced := Num_Produced + Data'Length;
		Ctx.Historize(Data);
	end Write_Output;

	procedure Historize(Ctx: in out Decompressor; Data: in Octets) is
	begin
		if Data'Length >= Ctx.History'Length then
			-- Replace entire history
			Ctx.History := Data(Data'Last - Ctx.History'Length + 1
								.. Data'Last);
			Ctx.History_Pos := 0;
		elsif Ctx.History_Pos + Data'Length - 1 > Ctx.History'Last then
			-- Two slices necessary
			declare
				Space_In_Hist: constant Integer :=
					Ctx.History'Last - Ctx.History_Pos + 1;
				Second_End_Idx: constant Integer :=
					Data'Length - Space_In_Hist - 1;
			begin
				Ctx.History(Ctx.History_Pos .. Ctx.History'Last)
					:= Data(Data'First ..
						Data'First + Space_In_Hist - 1);
				Ctx.History(0 .. Second_End_Idx)
					:= Data(Data'First + Space_In_Hist ..
						Data'Last);
				Ctx.History_Pos := Second_End_Idx + 1;
			end;
		else
			-- One slice sufficient
			Ctx.History(Ctx.History_Pos .. Ctx.History_Pos +
						Data'Length - 1) := Data;
			Ctx.History_Pos := Ctx.History_Pos + Data'Length;
		end if;
	end Historize;

	procedure Output_With_History(Ctx: in out Decompressor;
			Output: out Octets; Num_Produced: in out Integer;
			Offset: in Integer; Match_Length: in Integer) is
	begin
		if Match_Length <= Offset then
			Ctx.Output_With_History_No_Overlap(Output, Num_Produced,
							Offset, Match_Length);
		else
			Ctx.Output_With_History_No_Overlap(Output, Num_Produced,
							Offset, Offset);
			-- tail recursion
			Ctx.Output_With_History(Output, Num_Produced, Offset,
							Match_Length - Offset);
		end if;
	end Output_With_History;

	procedure Output_With_History_No_Overlap(Ctx: in out Decompressor;
			Output: out Octets; Num_Produced: in out Integer;
			Offset: in Integer; Match_Length: in Integer) is
		Offset_First:  constant Integer := (Ctx.History_Pos - Offset)
						mod Ctx.History'Length;
		Offset_Last:   constant Integer := (Ctx.History_Pos - Offset +
						Match_Length - 1)
						mod Ctx.History'Length;
	begin
		Ctx.Write_Output(Output, Num_Produced, (
			if   (Offset_Last < Offset_First) 
			then (Ctx.History(Offset_First .. Ctx.History'Last) &
						Ctx.History(0 .. Offset_Last))
			else (Ctx.HIstory(Offset_First .. Offset_Last))
		));
	end Output_With_History_No_Overlap;

end LZ4Ada;
