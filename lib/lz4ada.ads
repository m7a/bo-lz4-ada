with Ada.Streams;
with Interfaces;

-- API Rationale TODO CSTAT UPDATE THE IMPLEMENTATION TO REFLECT THIS NEW THINKING!
--
-- Num_Consumed: Specifies how many of the input bytes have been processed.
--               Next `Update` invocation is expected to present the input
--               data starting from index Num_Consumed (if 0-indexed).
--
-- Get_Minimum_Output_Buffer_Size: A single block size is the minimum output
--               buffer size. If the output buffer were possible to be chosen
--               even smaller, internal computation would be much more
--               complicated since it would be necessary to pause and resume
--               output mid-buffer. Allowing the routines to assume that there
--               is enough space for at least _one_ buffer, makes the handling
--               less complicated without impacting performance.
--
-- A small input may lead to a large output. To avoid using unbounded memory
-- amounts one must limit the output buffer size. This implementation ensures
-- this by passing a target buffer as an `out` parameter. A supplied input's
-- decompressed size may exceed the output buffer capacity. In order to allow
-- output for the remainder of the input to be generated, it may become
-- necessary to supply part of the same input again. This is achieved by
-- signallying the number of consumed bytes back to the caller.
--
-- There is no need for a "Final" function as LZ4 clearly indicates when the
-- data has arrived at the end of the frame. Function Update reports this by
-- returning True back to the caller. If the caller has more data to be
-- processed, a new Context needs to be initialized after Final has returned
-- true. (More input data provided will not be consumed, and Num_Consumed will
-- then be 0 all the time for this old context).
package LZ4Ada is

	pragma Assertion_Policy(Pre => Check, Post => Check);

	Checksum_Error: exception;

	type U8     is mod 2**8;
	type U32    is mod 2**32;
	type Octets is array (Integer range <>) of U8;

	type Decompressor(Size_Last: Integer) is tagged limited private;

	Empty: constant Octets(1 .. 0) := (others => 0);

	-- Decompressor
	function Init(Input: in Octets; Num_Consumed: out Integer)
				return Decompressor
				with Pre => Input'Length >= 7;
	function Init(Input: in Ada.Streams.Stream_Element_Array;
				Num_Consumed: out Integer) return Decompressor
				with Pre => Input'Length >= 7;

	function Get_Minimum_Output_Buffer_Size(Ctx: in Decompressor)
								return Integer;

	function Update(Ctx: in out Decompressor; Input: in Octets;
			Num_Consumed: out Integer; Output: out Octets;
			Num_Produced: out Integer) return Boolean
			with Pre => Output'Length >=
					Ctx.Get_Minimum_Output_Buffer_Size;
	function Update(Ctx: in out Decompressor;
			Input: in Ada.Streams.Stream_Element_Array;
			Num_Consumed: out Integer;
			Output: out Ada.Streams.Stream_Element_Array;
			Num_Produced: out Integer) return Boolean
			with Pre => Output'Length >=
					Ctx.Get_Minimum_Output_Buffer_Size;

	-- TODO z CURRENTLY ONLY EXPORTED FOR USE IN xxhash
	function Shift_Right(Value: in U32; Amount: in Natural) return U32 is
		(U32(Interfaces.Shift_Right(Interfaces.Unsigned_32(Value),
		Amount)));

private

	History_Size:     constant Integer := 64 * 1024;
	Block_Size_Bytes: constant Integer := 4;

	-- Forward declarations
	function Check_End_Mark(Ctx: in out Decompressor; Input: in Octets;
				Num_Consumed: in out Integer) return Boolean;
	-- TODO z Num_Consumed = 0 predondition can be lifted by improving the
	--        implementation code.
	procedure Try_Detect_Input_Length(Ctx: in out Decompressor;
			Input: in Octets; Num_Consumed: in out Integer)
			with pre => Ctx.Input_Buffer_Filled < Block_Size_Bytes
					and Num_Consumed = 0;
	procedure Handle_Newly_Known_Input_Length(Ctx: in out Decompressor;
			Input: in Octets; Num_Consumed: in out Integer;
			Output: out Octets; Num_Produced: in out Integer);
	procedure Cache_Data_And_Process_If_Full(Ctx: in out Decompressor;
			Input: in Octets; Num_Consumed: in out Integer;
			Output: out Octets; Num_Produced: in out Integer);
	procedure Decode_Full_Block_With_Trailer(Ctx: in out Decompressor;
			Input_Block: in Octets; Output: out Octets;
			Num_Produced: in out Integer);
	procedure Check_Checksum(Data: in Octets; Expect_Checksum: in U32);
	procedure Decompress_Full_Block(Ctx: in out Decompressor;
					Raw_Data: in Octets; Output: out Octets;
					Num_Produced: in out Integer);
	procedure Write_Output(Ctx: in out Decompressor; Output: in out Octets;
			Num_Produced: in out Integer; Data: in Octets);
	procedure Historize(Ctx: in out Decompressor; Data: in Octets);
	procedure Output_With_History(Ctx: in out Decompressor;
			Output: out Octets; Num_Produced: in out Integer;
			Offset: in Integer; Match_Length: in Integer);
	procedure Output_With_History_No_Overlap(Ctx: in out Decompressor;
			Output: out Octets; Num_Produced: in out Integer;
			Offset: in Integer; Match_Length: in Integer)
			with Pre => Match_Length <= Offset;

	type Format is (Legacy, Modern);

	type Decompressor(Size_Last: Integer) is tagged limited record
		Is_Format:               Format;
		Content_Checksum_Length: Integer; -- 0 or 4
		Block_Checksum_Length:   Integer; -- 0 or 4
		Is_At_End_Mark:          Boolean;
		Input_Buffer:            Octets(0 .. Size_Last);
		Input_Buffer_Filled:     Integer; -- how much data is in there
		Input_Length:            Integer; -- Declared current block len
		Is_Compressed:           Boolean; -- current block compressed YN
		History:                 Octets(0 .. History_Size - 1);
		History_Pos:             Integer;
	end record;

	function Get_Minimum_Output_Buffer_Size(Ctx: in Decompressor)
				return Integer is (Ctx.Input_Buffer'Length);
	function Shift_Right(Value: in U8; Amount: in Natural) return U8 is
		(U8(Interfaces.Shift_Right(Interfaces.Unsigned_8(Value),
		Amount))) with Pre => Amount <= 7;
	function Shift_Left(Value: in U32; Amount: in Natural) return U32 is
		(U32(Interfaces.Shift_Left(Interfaces.Unsigned_32(Value),
		Amount)));
	function Load_32(Src: in Octets) return U32
				is (U32(Src(Src'First)) or
				Shift_Left(U32(Src(Src'First + 1)), 8) or
				Shift_Left(U32(Src(Src'First + 2)), 16) or
				Shift_Left(U32(Src(Src'First + 3)), 24))
				with Pre => (Src'Length = 4);
	function Min(A, B: in Integer) return Integer is
						(if A < B then A else B);

end LZ4Ada;
