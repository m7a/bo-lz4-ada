with Ada.Streams;
with Interfaces;
use  Interfaces;

-- API Rationale
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

	subtype U8  is Interfaces.Unsigned_8;
	subtype U32 is Interfaces.Unsigned_32;
	type Octets is array (Integer range <>) of U8;

	Checksum_Error:  exception;
	Data_Corruption: exception;
	Not_Supported:   exception;

	type Decompressor(Size_Last: Integer) is tagged limited private;

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

	-- XXHash32 Implementation based on the following C++ implementation:
	-- https://github.com/stbrumme/xxhash/blob/master/xxhash32.h
	-- +--------------------------------------------------------------------
	-- | THIS IS AN ALTERED SOURCE VERSION                                 |
	-- +--------------------------------------------------------------------
	-- The C++ implementation has the following details:
	-- +-------------------------------------------------------------------+
	-- | xxhash32.h                                                        |
	-- | Copyright (c) 2016 Stephan Brumme. All rights reserved.           |
	-- | see http://create.stephan-brumme.com/disclaimer.html              |
	-- |                                                                   |
	-- | Unless otherwise noted, all source code published on              |
	-- | http://create.stephan-brumme.com and its sub-pages is licensed    |
	-- | similar to the zlib license:                                      |
	-- |                                                                   |
	-- | This software is provided 'as-is', without any express or implied |
	-- | warranty. In no event will the author be held liable for any      |
	-- damages arising from the use of this software.                      |
	-- |                                                                   |
	-- | Permission is granted to anyone to use this software for any      |
	-- | purpose, including commercial applications, and to alter it and   |
	-- | redistribute it freely, subject to the following restrictions:    |
	-- |                                                                   |
	-- | * The origin of this software must not be misrepresented; you     |
	-- |   must not claim that you wrote the original software.            |
	-- | * If you use this software in a product, an acknowledgment in the |
	-- |   product documentation would be appreciated but is not required. |
	-- | * Altered source versions must be plainly marked as such, and     |
	-- |   must not be misrepresented as being the original software.      |
	-- |                                                                   |
	-- | If you like / hate / ignore my software, send me an email or,     |
	-- | even better, a nice postcard. Thank you ! ☺                       |
	-- +-------------------------------------------------------------------+
	package XXHash32 is
		type Hasher is tagged limited private;
		function  Init return Hasher;
		function  Init(Seed: in U32) return Hasher;
		procedure Update(Ctx: in out Hasher; Input: in Octets);
		function  Final(Ctx: in Hasher) return U32;
		function  Hash(Input: in Octets) return U32; -- One-Stop Call
	private
		Prime_1: constant U32 := 2654435761;
		Prime_2: constant U32 := 2246822519;
		Prime_3: constant U32 := 3266489917;
		Prime_4: constant U32 :=  668265263;
		Prime_5: constant U32 :=  374761393;
		Max_Buffer_Size: constant Integer := 16;

		subtype U64 is Interfaces.Unsigned_64;
		procedure Process(Ctx: in out Hasher; Data: in Octets)
						with Pre => Data'Length = 16;

		type Hasher is tagged limited record
			-- No need to do an array if we always access by
			-- constant index. Allow the compiler to optimize here.
			State_0:      U32;
			State_1:      U32;
			State_2:      U32;
			State_3:      U32;
			Buffer:       Octets(0 .. Max_Buffer_Size - 1);
			Buffer_Size:  Integer;
			Total_Length: U64;
		end record;

		function Init return Hasher is (Init(0));
	end XXHash32;

private

	Magic_Modern: constant U32 := 16#184d2204#;
	Magic_Legacy: constant U32 := 16#184c2102#;

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
		Hash_All_Data:           LZ4Ada.XXHash32.Hasher;
	end record;

	function Get_Minimum_Output_Buffer_Size(Ctx: in Decompressor)
				return Integer is (Ctx.Input_Buffer'Length -
				Block_Size_Bytes - Ctx.Block_Checksum_Length);

	-- TODO z might want to make this more efficient?
	function Load_32(Src: in Octets) return U32
				is (U32(Src(Src'First)) or
				Shift_Left(U32(Src(Src'First + 1)), 8) or
				Shift_Left(U32(Src(Src'First + 2)), 16) or
				Shift_Left(U32(Src(Src'First + 3)), 24))
				with Pre => (Src'Length = 4);
	function Min(A, B: in Integer) return Integer is
						(if A < B then A else B);

end LZ4Ada;
