with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Streams;
use  Ada.Streams;
with LZ4Ada;

procedure UnLZ4Ada is

	Stdin:  constant access Root_Stream_Type'Class :=
		Ada.Text_IO.Text_Streams.Stream(Ada.Text_IO.Standard_Input);
	Stdout: constant access Root_Stream_Type'Class :=
		Ada.Text_IO.Text_Streams.Stream(Ada.Text_IO.Standard_Output);
	
	Buf_Input: Stream_Element_Array(0 .. 4095); -- 4k buffer
	Last:      Stream_Element_Offset;
	Consumed:  Stream_Element_Offset;

	procedure Process_Frame is
		Total_Consumed: Stream_Element_Offset;
		Ctx:            LZ4Ada.Decompressor := LZ4Ada.Init(
					Buf_Input(0 .. Last), Total_Consumed);
		Buf_Output:     Stream_Element_Array(0 ..
					Stream_Element_Offset(Ctx.
					Get_Minimum_Output_Buffer_Size - 1));
		Produced:       Stream_Element_Offset := 0;
		End_Of_Frame:   Boolean := False;
	begin
		loop
			-- Loop over input until something produced.
			-- When something was produced output it to free
			-- up the buffer since the contract of Update requires
			-- us to provide at least the minimum output buffer
			-- size of free space upon each invocation.
			while not End_Of_Frame and Total_Consumed <= Last and
							Produced = 0 loop
				End_Of_Frame := Ctx.Update(Buf_Input(
					Total_Consumed .. Last), Consumed,
					Buf_Output, Produced);
				Total_Consumed := Total_Consumed + Consumed;
			end loop;
			if Produced > 0 then
				Write(Stdout.all, Buf_Output(0 .. Produced - 1));
				Produced := 0;
			end if;

			-- Prefer to rely on detected end of frame conditions.
			-- When EOF occurs (Last < 0) but no end of frame
			-- was detected this hints towards a data corruption.
			exit when End_Of_Frame;
			if Total_Consumed > Last then
				Read(Stdin.all, Buf_Input, Last);
				if Last < 0 then
					raise Constraint_Error with
						"End not signalled by library" &
						". Unable to process all data";
				end if;
				Total_Consumed := 0;
			end if;
		end loop;
	end Process_Frame;

begin
	loop
		Read(Stdin.all, Buf_Input, Last);
		exit when Last < 0;
		if Last < 7 then
			raise Constraint_Error with "Partial frame detected. " &
						"Unable to process all data";
		end if;
		Process_Frame;
	end loop;
end UnLZ4Ada;
