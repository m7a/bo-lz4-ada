with Ada.Assertions;

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
		function "*"(Flag: in Boolean; Num: in Integer)
				return Integer is (if Flag then Num else 0);

		Magic_NB: constant U32 := Load_32(Input(Input'First
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
		BD_Block_Max_Size: constant U8 := Shift_Right(BD and 16#70#, 4);
		BD_Has_Reserved:   constant Boolean := ((BD and 16#8f#) /= 0);

		Block_Checksum_Size:   Integer := FLG_Block_Checksum   * 4;
		Content_Checksum_Size: Integer := FLG_Content_Checksum * 4;
		Cursor:                Integer := Input'First + 6;
		Content_Size:          U64     := 0;

		Declared_Format: Format;
		Block_Max_Size:  Integer;
	begin
		case Magic_NB is
		when Magic_Modern =>
			Declared_Format := Modern;
			Check_Flag_Validity(FLG_Version, FLG_Reserved or
							BD_Has_Reserved);
			Block_Max_Size := Block_Size_Table(BD_Block_Max_Size);

			-- record content size if available
			if FLG_Content_Size then
				Content_Size := Load_64(Input(Cursor ..
								Cursor + 7));
				Cursor := Cursor + 8;
			end if;

			-- skip over dictionary ID
			Cursor := Cursor + FLG_Dictionary_ID * 4;

			Check_Header_Checksum(Input(Input'First .. Cursor - 1),
						Input(Cursor));
			Num_Consumed := Cursor - Input'First + 1;
		when Magic_Legacy =>
			Declared_Format        := Legacy;
			Block_Max_Size         := 8 * 1024 * 1024;
			Block_Checksum_Size    := 0;
			Content_Checksum_Size  := 0;
			Num_Consumed           := 4;
		when 16#184d2a50# .. 16#184d2a5f# =>
			Declared_Format        := Skippable;
			Block_Max_Size         := 0;
			Block_Checksum_Size    := 0;
			Content_Checksum_Size  := 0;
			Num_Consumed           := 8;
			Content_Size           := U64(Load_32(
							Input(Input'First + 4 ..
							Input'First + 7)));
		when others =>
			raise Not_Supported with
					"Invalid or unsupported magic: " &
					U32'Image(Magic_NB);
		end case;
		return (
			Is_Format               => Declared_Format,
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
			-- No need to set this for skippable, since it is always
			-- assumed that Content_Size_Remaining contains a value
			-- for Skippable frames
			Has_Content_Size        => (Declared_Format = Modern
							and FLG_Content_Size),
			Content_Size_Remaining  => Content_Size,
			Is_Compressed           => (Declared_Format = Legacy),
			History                 => (others => 0),
			History_Pos             => 0,
			Hash_All_Data           => LZ4Ada.XXHash32.Init
		);
	end Init;

	procedure Check_Flag_Validity(FLG_Version: in U8;
							Reserved: in Boolean) is
	begin
		if FLG_Version /= 1 then
			raise Not_Supported with
				"Only LZ4 frame format version 01 supported. " &
				"Detected " & U8'Image(FLG_Version) &
				" instead.";
		end if;
		if Reserved then
			raise Not_Supported with
				"Found reserved bits /= 0. Data might be too " &
				"new to be processed by this implementation!";
		end if;
	end Check_Flag_Validity;

	function Block_Size_Table(BD_Block_Max_Size: in U8) return Integer is
	begin
		case BD_Block_Max_Size is
		when 4      => return       64 * 1024; -- 64 KiB
		when 5      => return      256 * 1024; -- 256 KiB
		when 6      => return     1024 * 1024; -- 1 MiB
		when 7      => return 4 * 1024 * 1024; -- 4 MiB
		when others => raise Not_Supported with
					"Unknown maximum block size flag: " &
					U8'Image(BD_Block_Max_Size);
		end case;
	end Block_Size_Table;

	procedure Check_Header_Checksum(Data: in Octets; HC: in U8) is
		Computed_HC: constant U8 := Shift_Right(U8(LZ4Ada.XXHash32.Hash(
							Data) and 16#ff#), 8);
	begin
		if HC /= Computed_HC then
			raise Checksum_Error with
				"Computed Header Checksum " &
				U8'Image(Computed_HC) & " does not match " &
				"expected Header Checksum " & U8'Image(HC);
		end if;
	end Check_Header_Checksum;

	function Load_64(Data: in Octets) return U64 is
		Ret: U64;
		for Ret'Address use Data'Address;
	begin
		return Ret;
	end Load_64;

	function Update(Ctx: in out Decompressor;
			Input:  in  Octets; Num_Consumed: out Integer;
			Output: out Octets; Num_Produced: out Integer)
			return Boolean is
	begin
		Num_Consumed := 0;
		Num_Produced := 0;
		if Ctx.Is_Format = Skippable then
			return Ctx.Skip(Input, Num_Consumed);
		end if;
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

	function Skip(Ctx: in out Decompressor; Input: in Octets;
				Num_Consumed: in out Integer) return Boolean is
		Remain: constant Integer := Integer(Ctx.Content_Size_Remaining);
	begin
		Num_Consumed := Min(Input'Length, Remain);
		Ctx.Content_Size_Remaining := U64(Remain - Num_Consumed);
		return Ctx.Content_Size_Remaining = 0;
	end Skip;

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
				Compare: constant U32 :=
							Ctx.Hash_All_Data.Final;
			begin
				Num_Consumed := Num_Consumed +
							Required_Input_Length;
				if Checksum /= Compare then
					raise Checksum_Error with
						"Computed content checksum " &
						U32'Image(Compare) & " does " &
						"not match declared content " &
						"checksum " &
						U32'Image(Compare) & ".";
				end if;
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
			elsif Ctx.Is_Format = Legacy and
						Length_Word = Magic_Legacy then
				Ctx.Is_At_End_Mark      := True;
				-- may not strictly be necessary:
				Ctx.Input_Buffer_Filled := 4;
				Ctx.Input_Buffer(0 .. Block_Size_Bytes - 1)
							:= (others => 0);
				Ada.Assertions.Assert(Num_Consumed >=
							Block_Size_Bytes);
				-- unconsume this part from the next frame!
				Num_Consumed := Num_Consumed - Block_Size_Bytes;
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
				raise Data_Corruption with
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
			Num_Consumed            := Num_Consumed + Input_Want;
			Ctx.Input_Buffer_Filled := 0;
			Ctx.Input_Length        := -1;
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
			Ctx.Write_Output(Output, Num_Produced, Raw_Data);
		end if;
	end Decode_Full_Block_With_Trailer;

	procedure Check_Checksum(Data: in Octets; Expect_Checksum: in U32) is
		Compute_Checksum: constant U32 := LZ4Ada.XXHash32.Hash(Data);
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
				raise Data_Corruption with
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
			raise Data_Corruption with
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
		if Ctx.Content_Checksum_Length /= 0 then
			Ctx.Hash_All_Data.Update(Data);
		end if;
		if Ctx.Has_Content_Size then
			if Ctx.Content_Size_Remaining < Data'Length then
				raise Data_Corruption with
					"Produced content size exceeds " &
					"declared content size. The supplied " &
					"data is inconsistent.";
			end if;
			Ctx.Content_Size_Remaining :=
				Ctx.Content_Size_Remaining - Data'Length;
		end if;
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
			else (Ctx.History(Offset_First .. Offset_Last))
		));
	end Output_With_History_No_Overlap;

	package body XXHash32 is

		function Init(Seed: in U32) return Hasher is
		begin
			return (State_0      => Seed + Prime_1 + Prime_2,
				State_1      => Seed + Prime_2,
				State_2      => Seed,
				State_3      => Seed - Prime_1,
				Buffer       => (others => 0),
				Buffer_Size  => 0,
				Total_Length => 0);
		end Init;

		procedure Update(Ctx: in out Hasher; Input: in Octets) is
			Data:     Integer := Input'First;
			Load_New: Integer;
		begin
			Ctx.Total_Length := Ctx.Total_Length + Input'Length;
			if Ctx.Buffer_Size + Input'Length < Max_Buffer_Size then
				Ctx.Buffer(Ctx.Buffer_Size .. Ctx.Buffer_Size +
						Input'Length - 1) := Input;
				return;
			end if;
			if Ctx.Buffer_Size > 0 then
				Load_New := Ctx.Buffer'Last - Ctx.Buffer_Size;
				Ctx.Buffer(Ctx.Buffer_Size .. Ctx.Buffer'Last)
					:= Input(Data .. Data + Load_New);
				Data := Data + Load_New + 1;
				Ctx.Process(Ctx.Buffer);
			end if;
			while Data <= Input'Last - Max_Buffer_Size + 1 loop
				Ctx.Process(Input(Data ..
						Data + Max_Buffer_Size - 1));
				Data := Data + Max_Buffer_Size;
			end loop;
			Ctx.Buffer_Size := Input'Last - Data + 1;
			Ctx.Buffer(0 .. Input'Last - Data) :=
						Input(Data .. Input'Last);
		end Update;

		-- efficient data parallel notation
		procedure Process(Ctx: in out Hasher; Data: in Octets) is
			D0: constant Integer := Data'First;
			B0: constant U32 := Load_32(Data(D0      .. D0 +  3));
			B1: constant U32 := Load_32(Data(D0 +  4 .. D0 +  7));
			B2: constant U32 := Load_32(Data(D0 +  8 .. D0 + 11));
			B3: constant U32 := Load_32(Data(D0 + 12 .. D0 + 15));

			procedure Rot_Mul(S: in out U32; B: in U32) is
			begin
				S := Rotate_Left(S + B * Prime_2, 13) * Prime_1;
			end Rot_Mul;
		begin
			Rot_Mul(Ctx.State_0, B0);
			Rot_Mul(Ctx.State_1, B1);
			Rot_Mul(Ctx.State_2, B2);
			Rot_Mul(Ctx.State_3, B3);
		end Process;

		function Final(Ctx: in Hasher) return U32 is
			Ret: U32 := U32(Ctx.Total_Length and 16#ffffffff#) +
				(if   (Ctx.Total_Length >= U64(Max_Buffer_Size))
				then  ( Rotate_Left(Ctx.State_0,  1) +
					Rotate_Left(Ctx.State_1,  7) +
					Rotate_Left(Ctx.State_2, 12) +
					Rotate_Left(Ctx.State_3, 18))
				else  (Ctx.State_2 + Prime_5));
			Data: Integer := 0;
		begin
			while (Data + 3) < Ctx.Buffer_Size loop
				Ret := Rotate_Left(Ret + Load_32(
						Ctx.Buffer(Data .. Data + 3)) *
						Prime_3, 17) * Prime_4;
				Data := Data + 4;
			end loop;
			while Data < Ctx.Buffer_Size loop
				Ret := Rotate_Left(Ret + U32(Ctx.Buffer(Data)) *
							Prime_5, 11) * Prime_1;
				Data := Data + 1;
			end loop;

			Ret := (Ret xor Shift_Right(Ret, 15)) * Prime_2;
			Ret := (Ret xor Shift_Right(Ret, 13)) * Prime_3;
			return Ret xor Shift_Right(Ret, 16);
		end Final;

		function Hash(Input: in Octets) return U32 is
			Ctx: Hasher := Init;
		begin
			Ctx.Update(Input);
			return Ctx.Final;
		end Hash;

	end XXHash32;

end LZ4Ada;
