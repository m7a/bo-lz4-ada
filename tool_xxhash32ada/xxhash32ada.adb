with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Streams;
use  Ada.Streams;
with LZ4Ada;
use  LZ4Ada;

procedure XXHash32Ada is
	Stdin:     constant access Root_Stream_Type'Class :=
	           Ada.Text_IO.Text_Streams.Stream(Ada.Text_IO.Standard_Input);
	Buf_Input: Stream_Element_Array(0 .. 4095);
	Last:      Stream_Element_Offset;
	Ctx:       XXHash32.Hasher := XXHash32.Init;
begin
	loop
		Read(Stdin.all, Buf_Input, Last);
		exit when Last < 0;
	
		declare
			Conv_Octets: Octets(0 .. Integer(Last));
			for Conv_Octets'Address use Buf_Input'Address;
		begin
			Ctx.Update(Conv_Octets);
		end;
	end loop;
	Ada.Text_IO.Put_Line("xxhash32(0, stdin) = 0x" & To_Hex(Ctx.Final));
end XXHash32Ada;
