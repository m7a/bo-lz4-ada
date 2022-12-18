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

procedure LZ4Test is

	pragma Assertion_Policy(Pre => Check, Post => Check);

	Test_Failure: exception;

	procedure Test_Inner(
		Ctx:                  in out LZ4Ada.Decompressor;
		Required_Buffer_Size: in     Stream_Element_Offset;
		Buf_Input:            in out Stream_Element_Array;
		Total_Consumed, Last: in out Stream_Element_Offset;
		LZS, BNS:             in out Ada.Streams.Stream_IO.File_Type
	) is
		O_Buf, C_Buf: Stream_Element_Array(1 .. Required_Buffer_Size);
		Consumed, C_Got, Len, Result_First, Result_Last:
							Stream_Element_Offset;
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
		end loop;
		if Ctx.Is_End_Of_Frame = No then
			raise Test_Failure with "Input data ended mid-frame";
		end if;
		Read(BNS, C_Buf, C_Got);
		if C_Got > 0 then
			raise Test_Failure with "Not all data decompressed";
		end if;
	end Test_Inner;

	procedure Test_Good_Case(LZS, BNS:
				in out Ada.Streams.Stream_IO.File_Type) is
		Buf_Input: Stream_Element_Array(0 .. 4095);
		Last, Total_Consumed, Required_Buffer_Size:
							Stream_Element_Offset;
	begin
		Read(LZS, Buf_Input, Last);
		declare
			Ctx: LZ4Ada.Decompressor := LZ4Ada.Init(
					Buf_Input(0 .. Last), Total_Consumed,
					Required_Buffer_Size);
		begin
			Test_Inner(Ctx, Required_Buffer_Size, Buf_Input,
					Total_Consumed, Last, LZS, BNS);
		end;
	end Test_Good_Case;

	procedure Test_Good_Case(En: in Directory_Entry_Type) is
		LZ: constant String := Full_Name(En);
		BN: constant String := LZ(LZ'First .. LZ'Last - 3) & "bin";
		LZS, BNS: Ada.Streams.STream_IO.File_Type;
	begin
		if Exists(LZ) and Exists(BN) then
			Open(LZS, In_File, LZ);
			Open(BNS, In_File, BN);
			begin
				Test_Good_Case(LZS, BNS);
				Put_Line("[ OK ] Test " & Simple_Name(En));
			exception
			when Ex: others =>
				-- seems to include a NL already
				Put("[FAIL] Test " & Simple_Name(En) &
					" -- " & Exception_Information(Ex));
			end;
			Close(BNS);
			Close(LZS);
		else
			Put_Line("[WARN] One of " & LZ & " and " & BN &
						" is missing. Not testing...");
		end if;
	end Test_Good_Case;

	procedure Test_Good_Cases is
	begin
		Search("../test_vectors_lz4", "*.lz4",
			(Ordinary_File => True, others => False),
			Test_Good_Case'Access);
	end Test_Good_Cases;

begin

	Test_Good_Cases;

end LZ4Test;
