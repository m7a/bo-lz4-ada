with Ada.Assertions;
use  Ada.Assertions;
with Ada.Text_IO;
use  Ada.Text_IO;
with Ada.Exceptions;
use  Ada.Exceptions;
with Ada.Streams;
use  Ada.Streams;
with Ada.Streams.Stream_IO;
use  Ada.Streams.Stream_IO;
with Ada.Directories;
use  Ada.Directories;
with LZ4Ada;
use  LZ4Ada;

-- Auxiliary program to perform Black-Box tests of the LZ4 decompression library
procedure LZ4Test is

	pragma Assertion_Policy(Pre => Check, Post => Check);

	Test_Failure: exception;

	--
	-- Good Cases
	--

	procedure Test_Good_Case_Inner(LZS, BNS:
				in out Ada.Streams.Stream_IO.File_Type) is
		Buf_Input:    Stream_Element_Array(0 .. 4095);
		OSZ:          Stream_Element_Offset;
		Ctx:          LZ4Ada.Decompressor := LZ4Ada.Init(OSZ);
		O_Buf, C_Buf: Stream_Element_Array(1 .. OSZ);

		Last:                 Stream_Element_Offset := -1;
		Total_Consumed:       Stream_Element_Offset := 0;
		Consumed, C_Got, Len, Result_First, Result_Last:
							Stream_Element_Offset;
		EOF_Status: End_Of_Frame := Ctx.Is_End_Of_Frame;
	begin
		loop
			if Total_Consumed > Last then
				Read(LZS, Buf_Input, Last);
				exit when Last < 0;
				Total_Consumed := 0;
			end if;
			Ctx.Update(Buf_Input(Total_Consumed .. Last), Consumed,
					O_Buf, Result_First, Result_Last);
			Len := (Result_Last - Result_First + 1);
			if Len > 0 then
				Read(BNS, C_Buf(Result_First .. Result_Last),
									C_Got);
				if C_Got /= Result_Last then
					raise Test_Failure with
						Stream_Element_Offset'Image(
						C_Got) & " /= " &
						Stream_Element_Offset'Image(
						Result_Last);
				end if;
				if C_Buf(Result_First .. Result_Last) /=
						O_Buf(Result_First ..
						Result_Last) then
					raise Test_Failure with "Data mismatch";
				end if;
			end if;
			Total_Consumed := Total_Consumed + Consumed;
			EOF_Status     := Ctx.Is_End_Of_Frame;
		end loop;
		if EOF_Status = No then
			raise Test_Failure with "Mismatching EOF status";
		end if;
		Read(BNS, C_Buf, C_Got);
		if C_Got > 0 then
			raise Test_Failure with
				"More comparison data (delta >= " &
				Stream_Element_Offset'Image(C_Got) &
				") than produced by decompressor.";
		end if;
	end Test_Good_Case_Inner;

	procedure Open_Stream_Reading(S: in out Ada.Streams.Stream_IO.File_Type;
					FN: in String) is
	begin
		Open(S, In_File, FN);
	end Open_Stream_Reading;

	generic
		type BNT is limited private;
		Ext: in String;
		with procedure Run(LZS: in out Ada.Streams.Stream_IO.File_Type;
							BNS: in out BNT);
		with procedure Open_BNT(BNS: in out BNT; BN: in String);
		with procedure Close_BNT(BNS: in out BNT);
	procedure Generic_Test_Case(En: in Directory_Entry_Type);

	procedure Generic_Test_Case(En: in Directory_Entry_Type) is
		LZ: constant String := Full_Name(En);
		BN: constant String := LZ(LZ'First .. LZ'Last - 3) & Ext;
		LZS: Ada.Streams.Stream_IO.File_Type;
		BNS: BNT;
	begin
		if Exists(LZ) and Exists(BN) then
			Open_Stream_Reading(LZS, LZ);
			Open_BNT(BNS, BN);
			begin
				Run(LZS, BNS);
				Put_Line("[ OK ] Test " & Simple_Name(En));
			exception
			when Ex: others =>
				-- seems to include a NL already
				Put("[FAIL] Test " & Simple_Name(En) &
					" -- " & Exception_Information(Ex));
			end;
			Close_BNT(BNS);
			Close(LZS);
		else
			Put_Line("[WARN] One of " & LZ & " and " & BN &
						" is missing. Not testing...");
		end if;
	end Generic_Test_Case;

	procedure Test_Good_Hash_Individual_Bytes is
		use type LZ4Ada.U32;
		TC: constant Octets := (16#1a#, 16#1a#, 16#1a#, 16#1a#,
					16#1a#, 16#1a#, 16#1a#, 16#1a#,
					16#1a#, 16#1a#, 16#1a#, 16#1a#,
					16#1a#, 16#1a#, 16#11#, 16#10#);
		Ctx: XXHash32.Hasher := XXHash32.Init;
	begin
		for I in TC'Range loop
			Ctx.Update(TC(I .. I));
		end loop;
		if Ctx.Final = 16#f994ef8a# then
			Put_Line("[ OK ] Test_Good_Hash_Individual_Bytes");
		else
			Put_Line("[FAIL] Test_Good_Hash_Individual_Bytes -- " &
						"expected 0xf994ef8a, got " &
						To_Hex(Ctx.Final));
		end if;
	end Test_Good_Hash_Individual_Bytes;

	procedure Test_Good_Decompress_Individual_Bytes is
		ENOOUT: constant String :=
			"[FAIL] Test_Good_Decompress_Individual_Bytes - " &
			"no output produced but expected";
		TC: constant Octets(0 .. 77) := (
		     16#02#, 16#21#, 16#4c#, 16#18#, 16#30#, 16#00#, 16#00#,
		     16#00#, 16#f0#, 16#1f#, 16#3c#, 16#3f#, 16#78#, 16#6d#,
		     16#6c#, 16#20#, 16#76#, 16#65#, 16#72#, 16#73#, 16#69#,
		     16#6f#, 16#6e#, 16#3d#, 16#22#, 16#31#, 16#2e#, 16#30#,
		     16#22#, 16#20#, 16#65#, 16#6e#, 16#63#, 16#6f#, 16#64#,
		     16#69#, 16#6e#, 16#67#, 16#3d#, 16#22#, 16#55#, 16#54#,
		     16#46#, 16#2d#, 16#38#, 16#22#, 16#3f#, 16#3e#, 16#3c#,
		     16#74#, 16#65#, 16#73#, 16#74#, 16#2f#, 16#3e#, 16#0a#,
		     16#02#, 16#21#, 16#4c#, 16#18#, 16#0e#, 16#00#, 16#00#,
		     16#00#, 16#d0#, 16#48#, 16#65#, 16#6c#, 16#6c#, 16#6f#,
		     16#20#, 16#77#, 16#6f#, 16#72#, 16#6c#, 16#64#, 16#2e#,
		     16#0a#
		);
		Expect_Str: constant String :=
			"<?xml version=""1.0"" encoding=""UTF-8""?><test/>" &
			Character'Val(16#0a#) & "Hello world." &
			Character'Val(16#0a#);
		RS_Expect: Octets(0 .. Expect_Str'Length - 1);
		for RS_Expect'Address use Expect_Str'Address;

		RS_Have: Octets(RS_Expect'Range);
		Initially_Consumed, Min_Buffer_Size: Integer;
		Ctx: Decompressor := Init_With_Header(TC, Initially_Consumed,
						Min_Buffer_Size, For_All);
		O_Buf: Octets(0 .. Min_Buffer_Size - 1);
		Num_Consumed, Output_First, Output_Last: Integer;
		RS_Idx: Integer := RS_Have'First;
	begin
		for I in Initially_Consumed .. TC'Last loop
			Num_Consumed := 0;
			while Num_Consumed = 0 loop
				Ctx.Update(TC(I .. I), Num_Consumed, O_Buf,
						Output_First, Output_Last);
				if Num_Consumed = 0 and Output_Last <
							Output_First then
					Put_Line(ENOOUT);
					return;
				end if;
				if Output_Last >= Output_First then
					RS_Have(RS_Idx .. RS_Idx + Output_Last -
						Output_First) := O_Buf(
						Output_First .. Output_Last);
					RS_Idx := RS_Idx +
						Output_Last - Output_First + 1;
				end if;
			end loop;
		end loop;
		if RS_Idx /= RS_Expect'Length then
			Put_Line("[FAIL] Test_Good_Decompress_Individual_Bytes"
				& " - too little output: " &
				Integer'Image(RS_Idx) & "/" &
				Integer'Image(RS_Expect'Length) &
				" bytes produced.");
		elsif RS_Have /= RS_Expect then
			Put_Line("[FAIL] Test_Good_Decompress_Individual_Bytes"
				& " - wrong output produced.");
		else
			Put_Line("[ OK ] " &
				"Test_Good_Decompress_Individual_Bytes");
		end if;
	end Test_Good_Decompress_Individual_Bytes;

	procedure Test_Good_Cases is
		procedure Test_Good_Case is new Generic_Test_Case(
				BNT       => Ada.Streams.Stream_IO.File_Type,
				Ext       => "bin", Run => Test_Good_Case_Inner,
				Open_BNT  => Open_Stream_Reading,
				Close_BNT => Close);
	begin
		Search("../test_vectors_lz4", "*.lz4",
				(Ordinary_File => True, others => False),
				Test_Good_Case'Access);
		Test_Good_Hash_Individual_Bytes;
		Test_Good_Decompress_Individual_Bytes;
	end Test_Good_Cases;

	--
	-- Error Cases
	--

	procedure Error_Test_Case_Process(Buf_Input: in Stream_Element_Array) is
		In_Conv: Octets(0 .. Buf_Input'Length - 1);
		for In_Conv'Address use Buf_Input'Address;
		Total_Consumed, Required_Buffer_Size: Integer;
		Ctx: LZ4Ada.Decompressor := LZ4Ada.Init_With_Header(In_Conv,
			Total_Consumed, Required_Buffer_Size, Single_Frame);
		BO: Stream_Element_Array(0 .. Stream_Element_Offset(
						Required_Buffer_Size - 1));
		Consumed, RF, RL: Stream_Element_Offset;
	begin
		while Total_Consumed < Buf_Input'Length loop
			Ctx.Update(Buf_Input(Stream_Element_Offset(
					Total_Consumed) .. Buf_Input'Last),
					Consumed, BO, RF, RL);
			if Consumed = 0 then
				raise Test_Failure with
					"No more data accepted but no " &
					"exception signalled. Regular " &
					"end should not be reached for " &
					"error test case.";
			end if;
			Total_Consumed := Total_Consumed + Integer(Consumed);
		end loop;
		raise Test_Failure with
			"All data processed but no exception raised. " &
			"Error test cases are expected to cause errors, but " &
			"none has appeared so far. This indicates a " &
			"defective code or test case.";
	end Error_Test_Case_Process;

	procedure Detail_Check_Error(Ex: in Exception_Occurrence;
							Declared: in String) is
		EI:  constant String := Exception_Information(Ex);
		EIS: constant String := EI(EI'First .. EI'Last - 1);
	begin
		Assert(EI(EI'Last) = Character'Val(16#0a#));
		if Declared /= EIS then
			Ada.Text_IO.Put_Line("[ II ] Expected = <" &
								Declared & ">");
			Ada.Text_IO.Put_Line("[ II ] Got      = <" & EIS & ">");
			raise Test_Failure with "Error test outcome not " &
				"matching predefined error message. See above.";
		end if;
	end Detail_Check_Error;

	procedure Test_Error_Case_Inner(
			LZS: in out Ada.Streams.Stream_IO.File_Type;
			Error_Description_FD: in out Ada.Text_IO.File_Type) is
		Buf_Input: Stream_Element_Array(0 .. 10_000);
		Last:      Stream_Element_Offset;
		Declared:  constant String :=
				Ada.Text_IO.Get_Line(Error_Description_FD);
	begin
		Read(LZS, Buf_Input, Last);
		begin
			Error_Test_Case_Process(Buf_Input(Buf_Input'First ..
									Last));
		exception
		-- propagate test failures
		when Test_Failure =>
			raise;
		-- declared exceptions are expected
		when Ex: Checksum_Error|Data_Corruption|Not_Supported|
								No_Progress =>
			Detail_Check_Error(Ex, Declared);
		-- there should not be any constraint errors or such
		when Ex: others =>
			raise Test_Failure with
				"Unexpected Exception during error testing: " &
				Exception_Information(Ex);
		end;
	end Test_Error_Case_Inner;

	procedure Test_Error_Cases is
		procedure Open_Text_Reading(FD: in out Ada.Text_IO.File_Type;
							FN: in String) is
		begin
			Open(FD, In_File, FN);
		end Open_Text_Reading;

		procedure Test_Error_Case is new Generic_Test_Case(
				BNT => Ada.Text_IO.File_Type,
				Ext => "eds", Run => Test_Error_Case_Inner,
				Open_BNT => Open_Text_Reading,
				Close_BNT => Ada.Text_IO.Close);
	begin
		Search("../test_vectors_lz4", "*.err",
			(Ordinary_File => True, others => False),
			Test_Error_Case'Access);
	end Test_Error_Cases;

begin

	Test_Good_Cases;
	Test_Error_Cases;

end LZ4Test;
