with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Streams;
use  Ada.Streams;
with LZ4Ada;

procedure UnLZ4Ada_Singleframe is

	Stdin:  constant access Root_Stream_Type'Class :=
		Ada.Text_IO.Text_Streams.Stream(Ada.Text_IO.Standard_Input);
	Stdout: constant access Root_Stream_Type'Class :=
		Ada.Text_IO.Text_Streams.Stream(Ada.Text_IO.Standard_Output);
	
	Buf_Input: Stream_Element_Array(0 .. 4095); -- 4k buffer
	Last:      Stream_Element_Offset;

	procedure Get_Input(Min_Bytes_Expected: in Stream_Element_Offset) is
	begin
		Read(Stdin.all, Buf_Input, Last);
		if Last < (Min_Bytes_Expected - 1) then
			raise Constraint_Error with
					"Input too short: Expected at least " &
					Stream_Element_Offset'Image(
					Min_Bytes_Expected) & " byte/s.";
		end if;
	end Get_Input;

	procedure Process_Frame is
		Total_Consumed:  Stream_Element_Offset;
		Req_Buffer_Size: Stream_Element_Offset;
		Ctx:             LZ4Ada.Decompressor := LZ4Ada.Init(
					Buf_Input(0 .. Last),
					Total_Consumed, Req_Buffer_Size);
		Buf:             Stream_Element_Array(1 .. Req_Buffer_Size);
		End_Of_Frame:    Boolean := False;
		Consumed:        Stream_Element_Offset;
		Result_First:    Stream_Element_Offset;
		Result_Last:     Stream_Element_Offset;
	begin
		while not End_Of_Frame loop
			if Total_Consumed > Last then
				Get_Input(Min_Bytes_Expected => 1);
				Total_Consumed := 0;
			end if;
			Ctx.Update(Buf_Input(Total_Consumed .. Last), Consumed,
				Buf, Result_First, Result_Last, End_Of_Frame);
			if (Result_Last - Result_First) >= 0 then
				Write(Stdout.all,
					Buf(Result_First .. Result_Last));
			end if;
			Total_Consumed := Total_Consumed + Consumed;
		end loop;
	end Process_Frame;
begin
	Get_Input(Min_Bytes_Expected => 7);
	Process_Frame;
end UnLZ4Ada_Singleframe;
