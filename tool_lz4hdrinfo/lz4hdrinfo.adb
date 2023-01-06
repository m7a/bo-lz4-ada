with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Streams;
use  Ada.Streams;
with Interfaces;
use  Interfaces;

procedure LZ4HdrInfo is

	subtype U8  is Interfaces.Unsigned_8;
	subtype U32 is Interfaces.Unsigned_32;
	subtype U64 is Interfaces.Unsigned_64;
	type Octets is array (Integer range <>) of U8;

	function To_Hex(Num: in U8) return String is
		Hex_Tbl: constant String := "0123456789abcdef";
	begin
		return (Hex_Tbl(Integer(Shift_Right(Num, 4)) + 1),
			Hex_Tbl(Integer(Num and 16#0f#)      + 1));
	end To_Hex;

	function To_Hex(Num: in U32) return String is
		Conv: Octets(0 .. 3);
		for Conv'Address use Num'Address;
	begin
		return To_Hex(Conv(3)) & To_Hex(Conv(2)) & To_Hex(Conv(1)) &
								To_Hex(Conv(0));
	end To_Hex;

	function Load_32(Src: in Octets) return U32
				is (U32(Src(Src'First)) or
				Shift_Left(U32(Src(Src'First + 1)), 8) or
				Shift_Left(U32(Src(Src'First + 2)), 16) or
				Shift_Left(U32(Src(Src'First + 3)), 24))
				with Pre => (Src'Length = 4);

	function Load_64(Data: in Octets) return U64 is
		Ret: U64;
		for Ret'Address use Data'Address;
	begin
		return Ret;
	end Load_64;

	Magic_Modern: constant U32 := 16#184d2204#; -- lz4ada.ads
	Magic_Legacy: constant U32 := 16#184c2102#;

	Stdin: constant access Root_Stream_Type'Class :=
		Ada.Text_IO.Text_Streams.Stream(Ada.Text_IO.Standard_Input);

	Input_Raw: Stream_Element_Array(0 .. 63);
	Input:     Octets(0 .. 63);
	for Input'Address use Input_Raw'Address;

	Last_Raw: Stream_Element_Offset;
	Last:     Integer;
	for Last'Address use Last_Raw'Address;

	procedure Block_Size_Table(BD_Block_Max_Size: in U8) is
	begin
		case BD_Block_Max_Size is
		when 4      => Ada.Text_IO.Put("64 KiB");
		when 5      => Ada.Text_IO.Put("256 KiB");
		when 6      => Ada.Text_IO.Put("1 MiB");
		when 7      => Ada.Text_IO.Put("4 MiB");
		when others => Ada.Text_IO.Put("INVALID");
		end case;
		Ada.Text_IO.Put_Line(" (" & To_Hex(BD_Block_Max_Size) & ")");
	end Block_Size_Table;

	procedure Decode_Header is -- largely copied from lz4ada.adb

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
		Cursor:            Integer := Input'First + 6;
		Content_Size:      U64     := 0;
	begin
		case Magic_NB is
		when Magic_Modern =>
			Ada.Text_IO.Put_Line("Declared Format        = " &
					To_Hex(Magic_Modern) & " (modern)");
			Ada.Text_IO.Put_Line("FLG                    = " &
					To_Hex(FLG));
			Ada.Text_IO.Put_Line("    Version:64|128     = " &
					To_Hex(FLG_Version));
			Ada.Text_IO.Put_Line("    Block_Checksum:16  = " &
					Boolean'Image(FLG_Block_Checksum));
			Ada.Text_IO.Put_Line("    Content_Size:8     = " &
					Boolean'Image(FLG_Content_Size));
			Ada.Text_IO.Put_Line("    Content_Checksum:4 = " &
					Boolean'Image(FLG_Content_Checksum));
			Ada.Text_IO.Put_Line("    Reserved:2         = " &
					Boolean'Image(FLG_Reserved));
			Ada.Text_IO.Put_Line("    Dictionary_ID:1    = " &
					Boolean'Image(FLG_Dictionary_ID));

			Ada.Text_IO.Put_Line("BD                     = " &
					To_Hex(BD));
			Ada.Text_IO.Put_Line("    Has_Reserved       = " &
					Boolean'Image(BD_Has_Reserved));
			Ada.Text_IO.Put("    Block_Max_Size     = ");
			Block_Size_Table(BD_Block_Max_Size);

			if FLG_Content_Size then
				Content_Size := Load_64(Input(Cursor ..
								Cursor + 7));
				Ada.Text_IO.Put_Line(
						"Content_Size           = " &
						U64'Image(Content_Size));
				Cursor := Cursor + 8;
			end if;

			-- skip over dictionary ID
			Cursor := Cursor + FLG_Dictionary_ID * 4;

			Ada.Text_IO.Put_Line("Header_Checksum        = " &
							To_Hex(Input(Cursor)));
		when Magic_Legacy =>
			Ada.Text_IO.Put_Line("Declared Format        = " &
					To_Hex(Magic_Legacy) & " (legacy)");
		when 16#184d2a50# .. 16#184d2a5f# =>
			Ada.Text_IO.Put_Line("Declared Format        = " &
					To_Hex(Magic_NB) & " (skippable)");
			Content_Size := U64(Load_32(Input(Input'First + 4 ..
							Input'First + 7)));
			Ada.Text_IO.Put_Line("Content_Size           = " &
					U64'Image(Content_Size));
		when others =>
			Ada.Text_IO.Put_Line("Declared Format        = " &
					To_Hex(Magic_NB) & " (UNSUPPORTED)");
		end case;
	end Decode_Header;

begin
	Ada.Text_IO.Put_Line("Ma_Sys.ma LZ4 Header Info 1.0.0, " &
				"(c) 2023 Ma_Sys.ma <info@masysma.net>");
	Ada.Text_IO.Put_Line("");

	Read(Stdin.all, Input_Raw, Last_Raw);
	if Last < 6 then
		raise Constraint_Error with "Partial frame detected. " &
						"Unable to process all data";
	end if;

	Decode_Header;

end LZ4HdrInfo;
