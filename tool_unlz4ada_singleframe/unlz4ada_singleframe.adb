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
	Buf_In: Stream_Element_Array(0 .. 4095); -- 4k buffer
	Last:   Stream_Element_Offset;
begin
	Read(Stdin.all, Buf_In, Last);
	if Last < 6 then
		raise Constraint_Error with "LZ4 Frame Header too short.";
	end if;
	declare
		Total_Consumed, Req_Buffer_Size: Stream_Element_Offset;
		Ctx: LZ4Ada.Decompressor := LZ4Ada.Init(Buf_In(0 .. Last),
					Total_Consumed, Req_Buffer_Size);
		Buf: Stream_Element_Array(1 .. Req_Buffer_Size);
		Consumed, Output_First, Output_Last: Stream_Element_Offset;
	begin
		loop
			if Total_Consumed > Last then
				Read(Stdin.all, Buf_In, Last);
				exit when Last < 0;
				Total_Consumed := 0;
			end if;
			Ctx.Update(Buf_In(Total_Consumed .. Last), Consumed,
						Buf, Output_First, Output_Last);
			Write(Stdout.all, Buf(Output_First .. Output_Last));
			Total_Consumed := Total_Consumed + Consumed;
		end loop;
		if LZ4Ada."="(Ctx.Is_End_Of_Frame, LZ4Ada.No) then
			raise Constraint_Error with "Input ended mid-frame.";
		end if;
	end;
end UnLZ4Ada_Singleframe;
