with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Streams;
use  Ada.Streams;
with LZ4Ada;

procedure UnLZ4Ada_Simple is
	Stdin:  constant access Root_Stream_Type'Class :=
		Ada.Text_IO.Text_Streams.Stream(Ada.Text_IO.Standard_Input);
	Stdout: constant access Root_Stream_Type'Class :=
		Ada.Text_IO.Text_Streams.Stream(Ada.Text_IO.Standard_Output);

	Buf_In: Stream_Element_Array(0 .. 4095); -- 4k buffer
	Buf_Sz: Stream_Element_Offset;
	Ctx:    LZ4Ada.Decompressor := LZ4Ada.Init(Buf_Sz);

	Last:           Stream_Element_Offset := -1;
	Total_Consumed: Stream_Element_Offset := 0;
	Output_Buffer:  Stream_Element_Array(1 .. Buf_Sz);

	Consumed, Output_First, Output_Last: Stream_Element_Offset;
begin
	loop
		if Total_Consumed > Last then
			Read(Stdin.all, Buf_In, Last);
			exit when Last < 0;
			Total_Consumed := 0;
		end if;
		Ctx.Update(Buf_In(Total_Consumed .. Last), Consumed,
				Output_Buffer, Output_First, Output_Last);
		Write(Stdout.all, Output_Buffer(Output_First .. Output_Last));
		Total_Consumed := Total_Consumed + Consumed;
	end loop;
	if LZ4Ada."="(Ctx.Is_End_Of_Frame, LZ4Ada.No) then
		raise Constraint_Error with "Input ended mid-frame.";
	end if;
end UnLZ4Ada_Simple;
