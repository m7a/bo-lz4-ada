-- LZ4 Extractor Library in Ada 1.0.0,
-- (c) 2022, 2023 Ma_Sys.ma <info@masysma.net>.
--
-- Available under Expat License, see lz4ada.ads for full license and copyright.

with Ada.Assertions;

package body LZ4Ada is

	------------------------------------------------------------------------
	------------------------------------------------------------------------
	-------------------------------------  STREAM_ELEMENT_ARRAY HANDLING  --
	------------------------------------------------------------------------
	------------------------------------------------------------------------

	function Init(Min_Buffer_Size:      out Stream_Element_Offset;
			Reservation:        in  Memory_Reservation := For_All)
			return Decompressor is
			(Init(Integer(Min_Buffer_Size), Reservation));

	-- Essentially performs unchecked conversions between the types on
	-- the assumption that they are just different names for the same
	-- thing. Use with care.
	procedure Update(Ctx:          in out Decompressor;
			 Input:        in     Stream_Element_Array;
			 Num_Consumed: out    Stream_Element_Offset;
			 Buffer:       in out Stream_Element_Array;
			 Output_First: out    Stream_Element_Offset;
			 Output_Last:  out    Stream_Element_Offset)
	is
		Input_Cnv:  Octets(0 .. Input'Length - 1);
				for Input_Cnv'Address use Input'Address;
		Buffer_Cnv: Octets(0 .. Buffer'Length - 1);
				for Buffer_Cnv'Address use Buffer'Address;
		First_Cnv:  Integer;
		Last_Cnv:   Integer;
	begin
		Ctx.Update(Input_Cnv, Integer(Num_Consumed), Buffer_Cnv,
							First_Cnv, Last_Cnv);
		Output_First := Buffer'First + Stream_Element_Offset(First_Cnv);
		Output_Last  := Buffer'First + Stream_Element_Offset(Last_Cnv);
	end Update;

	------------------------------------------------------------------------
	------------------------------------------------------------------------
	--------------------------------------------------------------  INIT  --
	------------------------------------------------------------------------
	------------------------------------------------------------------------

	function Init(Min_Buffer_Size:   out    Integer;
			Reservation:     in     Memory_Reservation := For_All)
			return Decompressor is
		Block_Max_Size: constant Integer := Get_Block_Size(Reservation);
	begin
		-- +8 for overcopy optimization
		Min_Buffer_Size := Block_Max_Size + History_Size + 8;
		return (M => (Memory_Reservation => Reservation, others => <>),
			-- Per spec, the declared size excludes block size +
			-- optional checksum fields. Hence add their sizes here!
			-- Since we do not know if there will be a block
			-- checksum appearing, always reserve 4 bytes for it.
			In_Last => Block_Max_Size + 4 + Block_Size_Bytes - 1,
			Hash_All_Data => LZ4Ada.XXHash32.Init,
			others => <>);
	end Init;

	function Get_Block_Size(R: in Memory_Reservation) return Integer is
		KiB: constant Integer := 1024;
		MiB: constant Integer := 1024 * KiB;
		LUT: constant array (Memory_Reservation) of Integer := (
				SZ_64_KiB  => 64  * KiB,
				SZ_256_KiB => 256 * KiB,
				SZ_1_MiB   => 1   * MiB,
				SZ_4_MiB   => 4   * MiB,
				SZ_8_MiB   => 8   * MiB
			);
	begin
		return LUT(R);
	end Get_Block_Size;

	function Init_With_Header(Input: in  Octets;
			Num_Consumed:    out Integer;
			Min_Buffer_Size: out Integer;
			Reservation:     in  Flexible_Memory_Reservation
								:= Single_Frame)
			return Decompressor is
		Header_Buffer:  Octets(0 .. 19); -- 20 byte buffer = large enough
		Input_Pos:      Integer := Input'First;
		-- If we were to use Reservation directly here, it would not
		-- tell us a max block size since MT.Memory_Reservation would
		-- still be a `Single_Frame`. Work around this by initially
		-- pretending that we want to `Use_First` when the API user
		-- supplied `Single_Frame`. Then, replace the detected setting
		-- by `Single_Frame` setting later.
		MT:             Decompressor_Meta := (Memory_Reservation =>
					(if Reservation = Single_Frame
					then Use_First else Reservation),
					others => <>);
		Consumed_Inner: Integer;
		Block_Max_Size: Integer;
		In_Last_Comp:   Integer;
	begin
		Num_Consumed := 0;
		while MT.Header_Parsing /= Header_Complete loop
			if Input_Pos > Input'Last then
				raise Too_Few_Header_Bytes with
					"Expected at least " &
					U64'Image(MT.Size_Remaining) &
					" more bytes but header input has " &
					"already ended.";
			end if;
			Process_Header_Bytes(MT, Header_Buffer,
						Input(Input_Pos .. Input'Last),
						Consumed_Inner);
			Input_Pos    := Input_Pos    + Consumed_Inner;
			Num_Consumed := Num_Consumed + Consumed_Inner;
		end loop;
		Block_Max_Size := Get_Block_Size(MT.Memory_Reservation);
		In_Last_Comp   := Block_Max_Size + MT.Block_Checksum_Length +
							Block_Size_Bytes - 1;
		Min_Buffer_Size := Block_Max_Size + History_Size + 8;
		if Reservation = Single_Frame then
			MT.Memory_Reservation := Single_Frame;
		end if;
		return (M => MT, In_Last => In_Last_Comp,
			Hash_All_Data => LZ4Ada.XXHash32.Init, others => <>);
	end Init_With_Header;

	------------------------------------------------------------------------
	------------------------------------------------------------------------
	------------------------------------------------------------  HEADER  --
	------------------------------------------------------------------------
	------------------------------------------------------------------------

	procedure Process_Header_Bytes(M: in out Decompressor_Meta;
				Input_Buffer: in out Octets; Input: in Octets;
				Num_Consumed: out Integer) is
		Copy_Length: constant Integer := Integer'Min(Input'Length,
						Integer(M.Size_Remaining));
	begin
		Ada.Assertions.Assert(Copy_Length > 0);
		Input_Buffer(M.Input_Buffer_Filled ..
			M.Input_Buffer_Filled + Copy_Length - 1) :=
			Input(Input'First .. Input'First + Copy_Length - 1);
		M.Input_Buffer_Filled := M.Input_Buffer_Filled + Copy_Length;
		M.Size_Remaining      := M.Size_Remaining - U64(Copy_Length);
		Num_Consumed          := Copy_Length;
		if M.Size_Remaining = 0 then
			case M.Header_Parsing is
			when Need_Magic =>
				Process_Header_Magic(M, Input_Buffer);
			when Need_Flags =>
				Process_Header_Flags(M, Input_Buffer);
			when Need_Modern =>
				Process_Modern_End_Of_Header(M, Input_Buffer);
			when Need_Skippable_Length =>
				M.Memory_Reservation := SZ_64_KiB;
				M.Header_Parsing := Header_Complete;
				M.Size_Remaining := U64(Load_32(Input_Buffer
								(4 .. 7)));
				M.Status_EOF := (if M.Size_Remaining = 0
							then Yes else No);
				M.Input_Buffer_Filled := 0;
			when Header_Complete =>
				raise Constraint_Error with
					"Header_Complete case must not be " &
					"reached while processing header " &
					"bytes. Library bug detected.";
			end case;
		end if;
	end Process_Header_Bytes;

	procedure Process_Header_Magic(M: in out Decompressor_Meta;
						Input_Buffer: in Octets) is
	begin
		Process_Header_Magic(M, Load_32(Input_Buffer(0 .. 3)));
	end Process_Header_Magic;

	procedure Process_Header_Magic(M: in out Decompressor_Meta;
							Magic_NB: in U32) is
	begin
		case Magic_NB is
		when Magic_Modern =>
			M.Is_Format               := Modern;
			M.Header_Parsing          := Need_Flags;
			M.Size_Remaining          := 2;
		when Magic_Legacy =>
			Process_Legacy_End_Of_Header(M);
		when Magic_Skippable =>
			M.Is_Format               := Skippable;
			M.Header_Parsing          := Need_Skippable_Length;
			M.Size_Remaining          := 4;
			M.Block_Checksum_Length   := 0;
			M.Content_Checksum_Length := 0;
			-- No need to set this for skippable, since it is always
			-- assumed that Size_Remaining contains a value
			-- for Skippable frames
			-- M.Has_Content_Size:=False; M.Is_Compressed:=False;
		when others => 
			raise Not_Supported with "Invalid or unsupported " &
						"magic: 0x" & To_Hex(Magic_NB);
		end case;
	end Process_Header_Magic;

	procedure Process_Legacy_End_Of_Header(M: in out Decompressor_Meta) is
		Effective_Reservation: Memory_Reservation := For_Legacy;
	begin
		M.Input_Buffer_Filled     := 0;
		M.Is_Format               := Legacy;
		M.Header_Parsing          := Header_Complete;
		M.Size_Remaining          := 0;
		M.Status_EOF              := Maybe;
		M.Block_Checksum_Length   := 0;
		M.Content_Checksum_Length := 0;
		M.Has_Content_Size        := False;
		M.Size_Remaining          := 0;
		M.Is_Compressed           := True;
		Check_Reservation(M.Memory_Reservation, Effective_Reservation);
		M.Memory_Reservation := Effective_Reservation;
	end Process_Legacy_End_Of_Header;

	procedure Check_Reservation(Requested: in Flexible_Memory_Reservation;
					Effective: in out Memory_Reservation) is
	begin
		if Requested in Memory_Reservation then
			if Effective > Requested then
				raise Too_Little_Memory with
					"LZ4 header requres reservation " &
					Memory_Reservation'Image(Effective) &
					", but API call requested that only " &
					Flexible_Memory_Reservation'Image(
					Requested) & " be used. This frame " &
					"cannot be processed under the given " &
					"constraints.";
			else
				-- Upgrade to the provided parameter
				-- (or do nothing of they match)
				Effective := Requested;
			end if;
		end if;
	end Check_Reservation;

	procedure Process_Header_Flags(M: in out Decompressor_Meta;
						Input_Buffer: in Octets) is
		function "*"(Flag: in Boolean; Num: in Integer)
				return Integer is (if Flag then Num else 0);

		FLG:              constant U8      := Input_Buffer(4);
		BD:               constant U8      := Input_Buffer(5);
		FLG_Block_Chck:   constant Boolean := ((FLG and 16) /= 0);
		FLG_Content_Chck: constant Boolean := ((FLG and  4) /= 0);
		FLG_Reserved:     constant Boolean := ((FLG and  2) /= 0);
		FLG_Dictionary:   constant Boolean := ((FLG and  1) /= 0);
		FLG_Version:      constant U8 := Shift_Right(FLG and 16#c0#, 6);
		BD_Block_Max_SZ:  constant U8 := Shift_Right(BD  and 16#70#, 4);
		BD_Has_Reserved:  constant Boolean := ((BD and 16#8f#) /= 0);
		Required_Reservation:      Memory_Reservation;
	begin
		Check_Flag_Validity(FLG_Version, FLG_Reserved or
							BD_Has_Reserved);

		-- Even if data size is 0, this frame can still contain data.
		-- Process such data by setting Status_EOF := No unconditionally
		M.Status_EOF              := No;
		Required_Reservation      := Get_Block_Size_Reservation(
							BD_Block_Max_SZ);
		M.Block_Checksum_Length   := FLG_Block_Chck   * 4;
		M.Content_Checksum_Length := FLG_Content_Chck * 4;
		M.Has_Content_Size        := ((FLG and 8) /= 0);
		M.Header_Parsing          := Need_Modern;
		-- for now still tracks the header bytes to come
		M.Size_Remaining          := U64(1 + M.Has_Content_Size * 8 +
							FLG_Dictionary * 4);

		Check_Reservation(M.Memory_Reservation, Required_Reservation);
		if M.Memory_Reservation /= Single_Frame then
			M.Memory_Reservation := Required_Reservation;
		end if;
	end Process_Header_Flags;

	procedure Check_Flag_Validity(FLG_Version: in U8;
							Reserved: in Boolean) is
	begin
		if FLG_Version /= 1 then
			raise Not_Supported with
				"Only LZ4 frame format version 01 supported. " &
				"Detected 0x" & To_Hex(FLG_Version) &
				" instead.";
		end if;
		if Reserved then
			raise Not_Supported with
				"Found reserved bits /= 0. Data might be too " &
				"new to be processed by this implementation!";
		end if;
	end Check_Flag_Validity;

	function Get_Block_Size_Reservation(BD_Block_Max_SZ: in U8)
						return Memory_Reservation is
	begin
		case BD_Block_Max_SZ is
		when 4      => return SZ_64_KiB;
		when 5      => return SZ_256_KiB;
		when 6      => return SZ_1_MiB;
		when 7      => return SZ_4_MiB;
		when others => raise Not_Supported with
					"Unknown maximum block size flag: 0x" &
					To_Hex(BD_Block_Max_SZ);
		end case;
	end Get_Block_Size_Reservation;

	procedure Process_Modern_End_Of_Header(M: in out Decompressor_Meta;
						Input_Buffer: in Octets) is
		Checksum_Byte: constant U8 :=
				Input_Buffer(M.Input_Buffer_Filled - 1);
	begin
		if M.Has_Content_Size then
			M.Size_Remaining := Load_64(Input_Buffer(6 .. 13));
		end if;
		-- offset 4 since FrameDescriptor excludes magic per spec.
		Check_Header_Checksum(Input_Buffer(4 ..
				M.Input_Buffer_Filled - 2), Checksum_Byte);
		M.Header_Parsing      := Header_Complete;
		M.Input_Buffer_Filled := 0;
	end Process_Modern_End_Of_Header;

	function Load_64(Data: in Octets) return U64 is
		Ret: U64; for Ret'Address use Data'Address;
	begin
		return Ret;
	end Load_64;

	procedure Check_Header_Checksum(Data: in Octets; HC: in U8) is
		Computed_HC: constant U8 := U8(Shift_Right(LZ4Ada.XXHash32.Hash(
							Data), 8) and 16#ff#);
	begin
		if HC /= Computed_HC then
			raise Checksum_Error with
				"Computed Header Checksum 0x" &
				To_Hex(Computed_HC) & " does not match " &
				"expected Header Checksum 0x" & To_Hex(HC);
		end if;
	end Check_Header_Checksum;

	function To_Hex(Num: in U8) return String is
		Hex_Tbl: constant String := "0123456789abcdef";
	begin
		return (Hex_Tbl(Integer(Shift_Right(Num, 4)) + 1),
			Hex_Tbl(Integer(Num and 16#0f#)      + 1));
	end To_Hex;

	function To_Hex(Num: in U32) return String is
		Conv: Octets(0 .. 3); for Conv'Address use Num'Address;
	begin
		return To_Hex(Conv(3)) & To_Hex(Conv(2)) & To_Hex(Conv(1)) &
								To_Hex(Conv(0));
	end To_Hex;

	------------------------------------------------------------------------
	------------------------------------------------------------------------
	------------------------------------------------------------  UPDATE  --
	------------------------------------------------------------------------
	------------------------------------------------------------------------

	procedure Update(Ctx:          in out Decompressor;
			 Input:        in     Octets;
			 Num_Consumed: out    Integer;
			 Buffer:       in out Octets;
			 Output_First: out    Integer;
			 Output_Last:  out    Integer) is
	begin
		Num_Consumed := 0;
		Output_First := 1;
		Output_Last  := 0;
		if Ctx.M.Header_Parsing /= Header_Complete then
			Process_Header_Bytes(Ctx.M, Ctx.Input_Buffer, Input,
								Num_Consumed);
		elsif Ctx.M.Is_Format = Skippable then
			Ctx.Skip(Input, Num_Consumed);
		else
			Ada.Assertions.Assert(Ctx.M.Is_Format /= TBD);
			if Ctx.Is_At_End_Mark then
				Ctx.Check_End_Mark(Input, Num_Consumed);
			elsif Ctx.Input_Length /= -1 then
				Ctx.Cache_Data_And_Process_If_Full(Input,
					Num_Consumed, Buffer, Output_First,
					Output_Last);
			else
				Ctx.Try_Detect_Input_Length(Input,
								Num_Consumed);
				if Ctx.Is_At_End_Mark then
					Ctx.Check_End_Mark(Input, Num_Consumed);
				elsif Ctx.Input_Length /= -1 then
					Ctx.Handle_Newly_Known_Input_Length(
						Input, Num_Consumed, Buffer,
						Output_First, Output_Last);
				end if;
			end if;
		end if;
	end Update;

	procedure Skip(Ctx: in out Decompressor; Input: in Octets;
						Num_Consumed: out Integer) is
		Remain:   constant U64 := Ctx.M.Size_Remaining;
		Consumed: constant U64 := U64'Min(U64(Input'Length), Remain);
	begin
		if Ctx.M.Status_EOF = Yes and Consumed = 0 then
			Ctx.Reset_For_Next_Frame(Input, Num_Consumed);
		else
			Num_Consumed         := Integer(Consumed);
			Ctx.M.Size_Remaining := Remain - Consumed;
			Ctx.M.Status_EOF     := (if Ctx.M.Size_Remaining = 0
							then Yes else No);
		end if;
	end Skip;

	procedure Reset_For_Next_Frame(Ctx: in out Decompressor;
				Input: in Octets; Num_Consumed: out Integer) is
	begin
		if Ctx.M.Memory_Reservation = Single_Frame then
			raise Data_Corruption with -- TODO OR NO_PROGRESS
				"Requested Single_Frame operation but data " &
				"was provided after End of Frame was detected";
		end if;
		Ctx.M.Status_EOF     := No;
		Ctx.M.Header_Parsing := Need_Magic;
		Ctx.M.Size_Remaining := 4;
		Ctx.Reset_Outer_For_Next_Frame;
		Process_Header_Bytes(Ctx.M, Ctx.Input_Buffer, Input,
								Num_Consumed);
	end Reset_For_Next_Frame;

	procedure Reset_Outer_For_Next_Frame(Ctx: in out Decompressor) is
	begin
		Ctx.Is_At_End_Mark     := False;
		Ctx.Input_Length       := -1;
		-- Unclear if this is needed but it would seem to improve
		-- reliability by avoiding any means to backreference data from
		-- the preceding frame!
		Ctx.Output_Pos         := 0;
		Ctx.Output_Pos_History := 0;
		Ctx.Hash_All_Data.Reset;
	end Reset_Outer_For_Next_Frame;

	procedure Check_End_Mark(Ctx: in out Decompressor; Input: in Octets;
						Num_Consumed: in out Integer) is
		procedure Set_Frame_Has_Ended is
		begin
			Ctx.M.Status_EOF := Yes;
			Ctx.M.Input_Buffer_Filled := 0;
			if Ctx.M.Has_Content_Size and
						Ctx.M.Size_Remaining /= 0 then
				raise Data_Corruption with
					"Frame has ended, but according to " &
					"content size, there should be " &
					U64'Image(Ctx.M.Size_Remaining) &
					" bytes left to output.";
			end if;
		end Set_Frame_Has_Ended;

		Provided_Len: constant Integer := Input'Length - Num_Consumed;
		Required_Len: constant Integer := Ctx.M.Content_Checksum_Length
						- Ctx.M.Input_Buffer_Filled;
	begin
		if Ctx.M.Content_Checksum_Length = 0 or Ctx.M.Status_EOF = Yes
						or Required_Len <= 0 then
			if Ctx.M.Status_EOF = Yes then
				Ada.Assertions.Assert(Num_Consumed = 0);
				Ctx.Reset_For_Next_Frame(Input, Num_Consumed);
			else
				Set_Frame_Has_Ended;
			end if;
		elsif Provided_Len >= Required_Len then
			declare
				Checksum: constant U32 := Load_32(
					Ctx.Input_Buffer(0 ..
						Ctx.M.Input_Buffer_Filled - 1) &
					Input(Input'First + Num_Consumed ..
						Input'First + Num_Consumed +
						Required_Len - 1)
				);
				Compare: constant U32 :=
							Ctx.Hash_All_Data.Final;
			begin
				Num_Consumed := Num_Consumed + Required_Len;
				if Checksum /= Compare then
					raise Checksum_Error with
						"Computed content checksum 0x" &
						To_Hex(Compare) & " does " &
						"not match declared content " &
						"checksum 0x" &
						To_Hex(Checksum) & ".";
				end if;
			end;
			Set_Frame_Has_Ended;
		else
			Ctx.Input_Buffer(Ctx.M.Input_Buffer_Filled ..
					Ctx.M.Input_Buffer_Filled +
					Provided_Len - 1) :=
				Input(Input'First + Num_Consumed .. Input'Last);
			Ctx.M.Input_Buffer_Filled := Ctx.M.Input_Buffer_Filled +
							Provided_Len;
			Num_Consumed := Num_Consumed + Provided_Len;
		end if;
	end Check_End_Mark;

	procedure Try_Detect_Input_Length(Ctx: in out Decompressor;
			Input: in Octets; Num_Consumed: in out Integer) is
		Additional_Length: constant Integer :=
				Block_Size_Bytes + Ctx.M.Block_Checksum_Length;
		Length_Word: U32;

		procedure Detect_Modern is
		begin
			if Ctx.M.Is_Format = Modern then
				-- Bit = 1 aka. set   means uncompressed.
				-- Bit = 0 aka. unset means compressed.
				Ctx.M.Is_Compressed := (
					(Length_Word and 16#80000000#) = 0);
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
					U32'Image(Length_Word) &
					" bytes + " &
					Integer'Image(Additional_Length) &
					" bytes for metadata.";
			end if;
		end Detect_Modern;
	begin
		Num_Consumed := Integer'Min(Block_Size_Bytes -
				Ctx.M.Input_Buffer_Filled, Input'Length);
		Ctx.Input_Buffer(Ctx.M.Input_Buffer_Filled ..
			Ctx.M.Input_Buffer_Filled + Num_Consumed - 1) :=
			Input(Input'First .. Input'First + Num_Consumed - 1);
		Ctx.M.Input_Buffer_Filled :=
			Ctx.M.Input_Buffer_Filled + Num_Consumed;

		if Ctx.M.Input_Buffer_Filled = Block_Size_Bytes then
			Length_Word := Load_32(Ctx.Input_Buffer(0 ..
							Block_Size_Bytes - 1));
			if Ctx.M.Is_Format = Modern and Length_Word = 0 then
				Ctx.Is_At_End_Mark        := True;
				Ctx.M.Input_Buffer_Filled := 0;
			elsif Ctx.M.Is_Format = Legacy and
					Is_Any_Magic_Number(Length_Word) then
				if Ctx.M.Memory_Reservation = Single_Frame then
					raise Data_Corruption with
						"Requested Single_Frame " &
						"operation but data provided " &
						"what looks like the " &
						"beginning of another frame.";
				end if;
				Ctx.Reset_Outer_For_Next_Frame;
				Process_Header_Magic(Ctx.M, Length_Word);
			else
				Detect_Modern;
			end if;
		end if;
	end Try_Detect_Input_Length;

	function Is_Any_Magic_Number(Candidate: in U32) return Boolean is
	begin
		case Candidate is
		when Magic_Modern|Magic_Legacy|Magic_Skippable => return True;
		when others                                    => return False;
		end case;
	end Is_Any_Magic_Number;

	procedure Handle_Newly_Known_Input_Length(Ctx: in out Decompressor;
				Input: in Octets; Num_Consumed: in out Integer;
				Buffer: in out Octets;
				Output_First: in out Integer;
				Output_Last: in out Integer) is
		Total_Length: constant Integer := Ctx.Input_Length +
						Ctx.M.Block_Checksum_Length;
	begin
		if (Input'Length - Num_Consumed) >= Total_Length then
			-- Supplied input is large enough to process right away
			-- => no copying
			declare
				Offset:    constant Integer := Input'First +
								Num_Consumed;
				Use_Input: constant Octets  := Input(Offset ..
						Offset + Total_Length - 1);
			begin
				Num_Consumed := Num_Consumed + Total_Length;
				Ctx.M.Input_Buffer_Filled := 0;
				Ctx.Input_Length          := -1;
				Ctx.Decode_Full_Block_With_Trailer(Use_Input,
					Buffer, Output_First, Output_Last);
			end;
			-- if output has entire block of free space consider
			-- re-running. optionally: else consider reading ahead
			-- length + re-running on smaller if available. possibly
			-- not a good idea as this may not improve performance
			-- in any meaningful way and callers need to do an
			-- outer loop anyways?
		else
			Ctx.Cache_Data_And_Process_If_Full(Input, Num_Consumed,
					Buffer, Output_First, Output_Last);
		end if;
	end Handle_Newly_Known_Input_Length;

	procedure Cache_Data_And_Process_If_Full(Ctx: in out Decompressor;
			Input: in Octets; Num_Consumed: in out Integer;
			Buffer: in out Octets; Output_First: in out Integer;
			Output_Last: in out Integer) is
		Avail:  constant Integer := Input'Length - Num_Consumed;
		Want:   constant Integer := Ctx.Input_Length +
						Ctx.M.Block_Checksum_Length -
						Ctx.M.Input_Buffer_Filled +
						Block_Size_Bytes;
		Fill:   constant Integer := Ctx.M.Input_Buffer_Filled;
		Offset: constant Integer := Input'First + Num_Consumed;
	begin
		if Want > Avail then
			Ctx.Input_Buffer(Fill .. Fill + Avail - 1) :=
						Input(Offset .. Input'Last);
			Ctx.M.Input_Buffer_Filled :=
					Ctx.M.Input_Buffer_Filled + Avail;
			Num_Consumed := Num_Consumed + Avail;
		else
			Num_Consumed := Num_Consumed + Want;
			Ctx.M.Input_Buffer_Filled := 0;
			Ctx.Input_Length := -1;
			Ctx.Decode_Full_Block_With_Trailer(
				Ctx.Input_Buffer(Block_Size_Bytes .. Fill - 1) &
				Input(Offset .. Offset + Want - 1),
				Buffer, Output_First, Output_Last
			);
		end if;
	end Cache_Data_And_Process_If_Full;

	procedure Decode_Full_Block_With_Trailer(
				Ctx:          in out Decompressor;
				Input_Block:  in     Octets;
				Buffer:       in out Octets;
				Output_First: out    Integer;
				Output_Last:  out    Integer) is
		Last:     constant Integer := Input_Block'Last -
						Ctx.M.Block_Checksum_Length;
		Raw_Data: constant Octets  := Input_Block(Input_Block'First ..
									Last);
	begin
		if Ctx.M.Block_Checksum_Length > 0 then
			Check_Checksum(Raw_Data, Load_32(Input_Block(
				Input_Block'Last - Ctx.M.Block_Checksum_Length
				+ 1 .. Input_Block'Last)));
		end if;

		if Ctx.Output_Pos >= History_Size then
			Ctx.Output_Pos := 0;
		end if;

		if Ctx.M.Is_Compressed then
			Ctx.Decompress_Full_Block(Raw_Data, Buffer,
						Output_First, Output_Last);
		else
			Ctx.Write_Output(Input_Block, Input_Block'First, Last,
									Buffer);
			if Ctx.Output_Pos >= History_Size then
				Ctx.Output_Pos_History := Ctx.Output_Pos;
			end if;
			Output_First := Ctx.Output_Pos - Raw_Data'Length;
			Output_Last  := Ctx.Output_Pos - 1;
			Ctx.Update_Checksum(Buffer(Output_First ..
								Output_Last));
		end if;
	end Decode_Full_Block_With_Trailer;

	procedure Check_Checksum(Data: in Octets; Expect_Checksum: in U32) is
		Compute_Checksum: constant U32 := LZ4Ada.XXHash32.Hash(Data);
	begin
		if Compute_Checksum /= Expect_Checksum then
			raise Checksum_Error with
				"Declared checksum is 0x" & To_Hex(
				Expect_Checksum) & ", but computed one is 0x" &
				To_Hex(Compute_Checksum) & ".";
		end if;
	end Check_Checksum;

	procedure Update_Checksum(Ctx: in out Decompressor; Outp: in Octets) is
	begin
		if Ctx.M.Content_Checksum_Length /= 0 then
			Ctx.Hash_All_Data.Update(Outp);
		end if;
	end Update_Checksum;

	procedure Decompress_Full_Block(Ctx:          in out Decompressor;
					Raw_Data:     in     Octets;
					Buffer:       in out Octets;
					Output_First: out    Integer;
					Output_Last:  out    Integer) is

		Idx: Integer := Raw_Data'First;

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
			Token:    constant U8 := Raw_Data(Idx);
			Num_Literals: Integer := Integer(Shift_Right(Token and
								16#f0#, 4));
			Match_Length: Integer := Integer(Token and 16#0f#);
			Offset:       Integer;
		begin
			Idx := Idx + 1;
			-- Literals
			Process_Variable_Length(Num_Literals);
			if Num_Literals > 0 then
				Ctx.Write_Output(Raw_Data, Idx, Idx +
						Num_Literals - 1, Buffer);
				Idx := Idx + Num_Literals;
			end if;
			if Idx > Raw_Data'Last then
				if Match_Length /= 0 then
					raise Data_Corruption with
						"Match_Length=" & Integer'Image(
						Match_Length) & " suggests " &
						"compressed data but this " &
						"sequence already ends after " &
						"the literals. This might " &
						"also happen with an " &
						"untypical encoder?";
				end if;
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
			Ctx.Output_With_History(Offset, Match_Length + 4,
									Buffer);
		end Decompress_Sequence;
	begin
		Output_First := Ctx.Output_Pos;
		while Idx <= Raw_Data'Last loop -- and not Has_Reached_End loop
			Decompress_Sequence;
		end loop;
		Output_Last := Ctx.Output_Pos - 1;
		Ctx.Update_Checksum(Buffer(Output_First .. Output_Last));
		if Ctx.Output_Pos >= History_Size then
			Ctx.Output_Pos_History := Ctx.Output_Pos;
		end if;
	end Decompress_Full_Block;

	procedure Write_Output(Ctx: in out Decompressor; Data: in Octets;
					First: in Integer; Last: in Integer;
					Buffer: in out Octets) is
		-- Suppressing these checks gives a rather significant
		-- performance boost. Feel free to enable them if safety is
		-- hugely more important compared to performance for you.
		-- On my test system, commenting out these "pragma Suppress"
		-- statements gives a performance drop of at least -100 MiB/s...
		pragma Suppress(Length_Check);
		pragma Suppress(Overflow_Check);
		pragma Suppress(Index_Check);
		pragma Suppress(Range_Check);

		Num_El:     constant Integer := Last - First + 1;
		Cursor_Out:          Integer := Ctx.Output_Pos;
		Cursor_In:           Integer := First;
	begin
		-- "Wild copy" optimization.
		-- Prefer to copy multiples of 8 bytes, then fix up the issues
		-- afterwards. When we want to copy 3 bytes, it is more
		-- efficient to copy 8 at once rather than 3 explicitly.
		while (Data'Last - Cursor_In + 1) >= 8 and
							Cursor_In <= Last loop
			Buffer(Cursor_Out .. Cursor_Out + 7) :=
					Data(Cursor_In .. Cursor_In + 7);
			Cursor_Out := Cursor_Out + 8;
			Cursor_In  := Cursor_In  + 8;
		end loop;
		if Cursor_In <= Last then
			Buffer(Cursor_Out .. Cursor_Out + Last - Cursor_In) :=
							Data(Cursor_In .. Last);
		end if;
		Ctx.Output_Pos := Ctx.Output_Pos + Num_El;
		Ctx.Decrease_Data_Size_Remaining(U64(Num_El));
	end Write_Output;

	procedure Decrease_Data_Size_Remaining(Ctx: in out Decompressor;
							Data_Length: in U64) is
	begin
		if Ctx.M.Has_Content_Size then
			if Ctx.M.Size_Remaining < Data_Length then
				raise Data_Corruption with
					"Produced content size exceeds " &
					"declared content size. The supplied " &
					"data is inconsistent.";
			end if;
			Ctx.M.Size_Remaining := Ctx.M.Size_Remaining -
								Data_Length;
		end if;
	end Decrease_Data_Size_Remaining;

	-- While this procedure looks a little convoluted, it is significantly
	-- faster compared to a naive solution where each byte is evaluated
	-- again as to whether a “wrap around” nees to be considered or not
	-- (even if this choice is hidden in a modulus operation!)
	procedure Output_With_History(Ctx: in out Decompressor;
				Offset: in Integer; Match_Length: in Integer;
				Buffer: in out Octets) is
		Raw_Offset:      constant Integer := Ctx.Output_Pos - Offset;
		Remaining_Match: Integer := Match_Length;
		H_Offset:        Integer;
		H_Length:        Integer;
		I_Offset:        Integer;
		I_Length:        Integer;
		R_Start_Idx:     Integer;
		R_Length:        Integer;
		R_Processed:     Integer := 0;
	begin
		if Raw_Offset >= 0 then
			-- Start with intermediate part right away
			I_Offset := Raw_Offset;
			I_Length := Integer'Min(Match_Length, Offset);
		else
			-- Have some part to replay from history
			H_Offset := Raw_Offset + Ctx.Output_Pos_History;
			H_Length := Integer'Min(Match_Length,
						Offset - Ctx.Output_Pos);
			if H_Offset < 0 then
				raise Data_Corruption with
					"Backreference location out of range." &
					" Read from offset " &
					Integer'Image(H_Offset) &
					" not possible (earliest available " &
					"index is 0).";
			end if;
			if H_Length > 0 then
				Ctx.Write_Output(Buffer, H_Offset,
					H_Offset + H_Length - 1, Buffer);
				Remaining_Match := Match_Length - H_Length;
			end if;
			I_Offset := 0;
			I_Length := Integer'Min(Remaining_Match,
								Ctx.Output_Pos);
		end if;

		-- Intermediate part
		if I_Length > 0 then
			Ctx.Write_Output(Buffer, I_Offset,
					I_Offset + I_Length - 1, Buffer);
			Remaining_Match := Remaining_Match - I_Length;
		end if;

		-- Have repeating part to handle
		if Remaining_Match > 0 then
			R_Start_Idx := Ctx.Output_Pos - Offset;
			while R_Processed < Remaining_Match loop
				R_Length := Integer'Min(Ctx.Output_Pos -
						R_Start_Idx,
						Remaining_Match - R_Processed);
				Ctx.Write_Output(Buffer, R_Start_Idx,
					R_Start_Idx + R_Length - 1, Buffer);
				R_Processed := R_Processed + R_Length;
			end loop;
		end if;
	end Output_With_History;

	function Is_End_Of_Frame(Ctx: in Decompressor) return End_Of_Frame is
			(if Ctx.M.Is_Format = Legacy and Ctx.Is_At_End_Mark
			then Maybe else Ctx.M.Status_EOF);

	------------------------------------------------------------------------
	------------------------------------------------------------------------
	---------------------------------------------------------  XXHASH 32  --
	------------------------------------------------------------------------
	------------------------------------------------------------------------

	package body XXHash32 is

		function Init(Seed: in U32 := 0) return Hasher is
		begin
			return Ctx: Hasher do
				Ctx.Reset;
			end return;
		end Init;

		procedure Reset(Ctx: in out Hasher; Seed: in U32 := 0) is
		begin
			Ctx.State_0      := Seed + Prime_1 + Prime_2;
			Ctx.State_1      := Seed + Prime_2;
			Ctx.State_2      := Seed;
			Ctx.State_3      := Seed - Prime_1;
			Ctx.Buffer_Size  := 0;
			Ctx.Total_Length := 0;
		end Reset;

		procedure Update(Ctx: in out Hasher; Input: in Octets) is
			Fast:  Boolean := (Ctx.Buffer_Size = 0);
			Start: Integer := Input'First;
			Len:   Integer;
		begin
			-- Processing is much faster if data need not be
			-- copied to the internal buffer first. This loop does
			-- this for the minimum number of thimes and uses
			-- to a “fast” mode without copying as much as possible
			while Start <= Input'Last loop
				Len := Integer'Min(16, Input'Last - Start + 1);
				if Len = 16 and Fast then
					Ctx.Total_Length :=
							Ctx.Total_Length + 16;
					Ctx.Process(Input(Start .. Start + 15));
					Start := Start + 16;
				else
					Fast  := Ctx.Update1(Input(Start));
					Start := Start + 1;
				end if;
			end loop;
		end Update;

		function Update1(Ctx: in out Hasher; Input: in U8)
							return Boolean is
		begin
			Ctx.Buffer(Ctx.Buffer_Size) := Input;
			Ctx.Total_Length            := Ctx.Total_Length + 1;
			Ctx.Buffer_Size             := Ctx.Buffer_Size  + 1;
			if Ctx.Buffer_Size = Max_Buffer_Size then
				Ctx.Buffer_Size := 0;
				Ctx.Process(Ctx.Buffer);
				return True;
			end if;
			return False;
		end Update1;

		procedure Process(Ctx: in out Hasher; Data: in Octets) is
			B: Array(0..3) of U32; for B'Address use Data'Address;

			procedure Rot_Mul(S: in out U32; B: in U32) is
			begin
				S := Rotate_Left(S + B * Prime_2, 13) * Prime_1;
			end Rot_Mul;
		begin
			Rot_Mul(Ctx.State_0, B(0));
			Rot_Mul(Ctx.State_1, B(1));
			Rot_Mul(Ctx.State_2, B(2));
			Rot_Mul(Ctx.State_3, B(3));
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
