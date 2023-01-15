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
	Buf_Input_Conv: LZ4Ada.Octets(0 .. Buf_Input'Length - 1);
			for Buf_Input_Conv'Address use Buf_Input'Address;
	Last:           Integer := -1;
	Consumed:       Stream_Element_Offset;
	Total_Consumed: Integer := 0;
	End_Of_Input:   Boolean := False;
	EOF_Status:     LZ4Ada.End_Of_Frame;

	procedure Process_Inner(Ctx: in out LZ4Ada.Decompressor;
				Buf: in out Stream_Element_Array) is
		Result_First: Stream_Element_Offset;
		Result_Last:  Stream_Element_Offset;
	begin
		Ctx.Update(Buf_Input(Stream_Element_Offset(Total_Consumed) ..
					Stream_Element_Offset(Last)), Consumed,
					Buf, Result_First, Result_Last);
		Total_Consumed := Total_Consumed + Integer(Consumed);
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
				Read(Stdin.all, Buf_Input,
						Stream_Element_Offset(Last));
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
		if Last - Total_Consumed < 6 then
			Buf_Input_Conv(0 .. Last - Total_Consumed) :=
					Buf_Input_Conv(Total_Consumed .. Last);
			Read(Stdin.all, Buf_Input(Stream_Element_Offset(
				Last - Total_Consumed + 1) ..
				Stream_Element_Offset(Buf_Input'Last)),
				Stream_Element_Offset(Last));
			exit when Last < 0;
			if Last < 6 then
				raise Constraint_Error with
						"Partial frame detected. " &
						"Unable to process all data";
			end if;
			Total_Consumed := 0;
		end if;
		EOF_Status := LZ4Ada.No;
		declare
			Required_Buffer_Size: Integer;
			Consumed_Conv: Integer;
			Ctx: LZ4Ada.Decompressor := LZ4Ada.Init_With_Header(
					Buf_Input_Conv(Total_Consumed .. Last),
					Consumed_Conv, Required_Buffer_Size,
					LZ4Ada.Single_Frame);
			Buf: Stream_Element_Array(1 .. Stream_Element_Offset(
							Required_Buffer_Size));
		begin
			Total_Consumed := Total_Consumed + Consumed_Conv;
			-- Check for = No here because we need to assume that
			-- each potential end of frame is treated like an
			-- actual end of frame given that after the current
			-- legacy frame there might be a modern frame that
			-- needs different decoder settings and hence a
			-- re-iteration of `init`. This is slower than
			-- checking for `EOF_Status /= LZ4Ada.Yes` which you
			-- might prefer in case you do not need to deal with
			-- mixed legacy/modern frame inputs.
			while EOF_Status = LZ4Ada.No and not End_Of_Input loop
				Process_Inner(Ctx, Buf);
			end loop;
		end;
	end loop;
end UnLZ4Ada;
