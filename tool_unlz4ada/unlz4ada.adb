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
	
	Buf_Input:      Stream_Element_Array(0 .. 4095); -- 4k buffer
	Last:           Stream_Element_Offset;
	Consumed:       Stream_Element_Offset;
	End_Of_Frame:   Boolean;
	Total_Consumed: Stream_Element_Offset;

	procedure Process_Inner(Ctx: in out LZ4Ada.Decompressor) is
		Produced: constant Stream_Element_Array := Ctx.Update(Buf_Input(
			Total_Consumed .. Last), Consumed, End_Of_Frame);
	begin
		Total_Consumed := Total_Consumed + Consumed;

		-- Loop over input until something produced.
		-- When something was produced output it to free
		-- up the buffer since the contract of Update requires
		-- us to provide at least the minimum output buffer
		-- size of free space upon each invocation.
		if End_Of_Frame or Total_Consumed > Last or
							Produced'Length > 0 then
			Write(Stdout.all, Produced);

			-- Prefer to rely on detected end of frame conditions.
			-- When EOF occurs (Last < 0) but no end of frame
			-- was detected this hints towards a data corruption.
			if not End_Of_Frame and Total_Consumed > Last then
				Read(Stdin.all, Buf_Input, Last);
				if Last < 0 then
					raise Constraint_Error with
						"End not signalled by library" &
						". Unable to process all data";
				end if;
				Total_Consumed := 0;
			end if;
		end if;
	end Process_Inner;
begin
	loop
		Read(Stdin.all, Buf_Input, Last);
		exit when Last < 0;
		if Last < 7 then
			raise Constraint_Error with "Partial frame detected. " &
						"Unable to process all data";
		end if;
		End_Of_Frame := False;
		declare
			Ctx: LZ4Ada.Decompressor := LZ4Ada.Init(
					Buf_Input(0 .. Last), Total_Consumed);
		begin
			while not End_Of_Frame loop
				Process_Inner(Ctx);
			end loop;
		end;
	end loop;
end UnLZ4Ada;
