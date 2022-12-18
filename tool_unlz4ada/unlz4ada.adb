with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Streams;
use  Ada.Streams;
with LZ4Ada;

procedure UnLZ4Ada is

	use type LZ4Ada.End_Of_Frame;

	Stdin:  constant access Root_Stream_Type'Class :=
		Ada.Text_IO.Text_Streams.Stream(Ada.Text_IO.Standard_Input);
	Stdout: constant access Root_Stream_Type'Class :=
		Ada.Text_IO.Text_Streams.Stream(Ada.Text_IO.Standard_Output);
	
	Buf_Input:      Stream_Element_Array(0 .. 4095);
	Last:           Stream_Element_Offset;
	Consumed:       Stream_Element_Offset;
	Total_Consumed: Stream_Element_Offset;
	End_Of_Input:   Boolean := False;
	EOF_Status:     LZ4Ada.End_Of_Frame := LZ4Ada.Maybe;

	procedure Process_Inner(Ctx: in out LZ4Ada.Decompressor;
				Buf: in out Stream_Element_Array) is
		Result_First: Stream_Element_Offset;
		Result_Last:  Stream_Element_Offset;
	begin
		Ctx.Update(Buf_Input(Total_Consumed .. Last), Consumed,
						Buf, Result_First, Result_Last);
		Total_Consumed := Total_Consumed + Consumed;
		EOF_Status     := Ctx.Is_End_Of_Frame;

		-- Loop over input until something produced. When something was
		-- produced output it to free up the buffer since next call
		-- might overwrite its contents.
		if EOF_Status = LZ4Ada.Yes or Total_Consumed > Last or
					(Result_Last - Result_First) >= 0 then
			Write(Stdout.all, Buf(Result_First .. Result_Last));

			-- Prefer to rely on detected end of frame conditions.
			-- When EOF occurs (Last < 0) but no end of frame
			-- was detected this hints towards a data corruption.
			if EOF_Status /= LZ4Ada.Yes and
						Total_Consumed > Last then
				Read(Stdin.all, Buf_Input, Last);
				if Last < 0 then
					End_Of_Input := True;
					if EOF_Status = LZ4Ada.No then
						raise Constraint_Error with
						"End not signalled by library" &
						". Unable to process all data";
					end if;
				end if;
				Total_Consumed := 0;
			end if;
		end if;
	end Process_Inner;
begin
	while not End_Of_Input loop
		Read(Stdin.all, Buf_Input, Last);
		exit when Last < 0;
		if Last < 6 then
			raise Constraint_Error with "Partial frame detected. " &
						"Unable to process all data";
		end if;
		declare
			Required_Buffer_Size: Stream_Element_Offset;
			Ctx: LZ4Ada.Decompressor := LZ4Ada.Init(
					Buf_Input(0 .. Last), Total_Consumed,
					Required_Buffer_Size);
			Buf: Stream_Element_Array(1 .. Required_Buffer_Size);
		begin
			while EOF_Status /= LZ4Ada.Yes and not End_Of_Input loop
				Process_Inner(Ctx, Buf);
			end loop;
		end;
	end loop;
end UnLZ4Ada;
