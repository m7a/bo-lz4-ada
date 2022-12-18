-- LZ4 Extractor Library in Ada 1.0.0, (c) 2022 Ma_Sys.ma <info@masysma.net>.
--
-- This implementation has been created according to the LZ4 Block and Frame
-- format specifications taking some loose inspriation also from the
-- implemtnation of other decompressors. The XXHash32 implementation has been
-- directly ported from an existing C++ implementation
-- (c) 2018 Stephan Brumme
-- <https://github.com/stbrumme/xxhash/blob/master/xxhash32.h>
--
-- MIT License
--
-- Copyright (c) 2022 Ma_Sys.ma <info@masysma.net>
-- Copyright (c) 2018 Stephan Brumme
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"),
-- to deal in the Software without restriction, including without limitation
-- the rights to use, copy, modify, merge, publish, distribute, sublicense,
-- and/or sell copies of the Software, and to permit persons to whom the
-- Software is furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included
-- in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

with Ada.Streams;
with Interfaces;
use  Interfaces;

-- This API supports using Stream_Element_Array/Stream_Element_Offset types such
-- that no “custom” datatypes need to be used. Internally, everything is
-- computed using the custom datatypes, though. To allow efficient interfacing
-- in cases where origin data is not a Stream_Element_Array/_Offset you can of
-- course directly use the functions with the “custom” data types, saving any
-- potential overhead of conversion.
--
-- WARNING About Portability:
-- This code assumes that Stream_Element_Array consists of Bytes and that code
-- runs on a little endian arhcitecture. If your implementation has different
-- value types for Stream_Element_Array, use the `Octets`-based API.
-- If your architecture is big endian, this implementation is not expected to
-- work out-of-the box for you -- feel free to submit patches for this, though!
package LZ4Ada is

	-- The performance penalties of enabling these checks were found to be
	-- acceptable.
	pragma Assertion_Policy(Pre => Check, Post => Check);

	subtype U8  is Interfaces.Unsigned_8;
	subtype U32 is Interfaces.Unsigned_32;
	subtype U64 is Interfaces.Unsigned_64;
	type Octets is array (Integer range <>) of U8;

	-- The end of frame status can have three states
	--
	-- Yes:
	--	The currently processed frame is a “modern” format frame
	--	and as such, it can indicate when no more data fóllows.
	--	End_Of_Frame = Yes means that this condition has been reached.
	-- No:
	--	The Decompressor context has currently “unfinished” data to
	--	process or the current frame is “modern” and end-of-frame has
	--	not been indicated yet.
	-- Maybe:
	--	The Decompressor context has an empty input buffer and is at
	--	the end of a block and the current frame format is legacy.
	--	This _could_ mean that no more data is to be processed. Since
	--	legacy format does not guarantee this, though, `Maybe` is
	--	reported.
	type End_Of_Frame is (Yes, No, Maybe);

	-- This exception is raised whenever an LZ4 checksum does not match.
	-- The library does not currently support bypassing the checksum
	-- verification (it is theoretically possible to remove checksums from
	-- the input data and then decode the remaining data, though).
	Checksum_Error:  exception;

	-- This exception is raised whenever internal assumptions of the LZ4
	-- frame or block format are violated. It indicates non-LZ4 or corrupted
	-- input data.
	Data_Corruption: exception;

	-- This exception is raised whenever values observed that the LZ4
	-- specification reports as reserved. As such, the values could indicate
	-- newer data formats/features being in use. As this need not be
	-- corrupted data but could be a valid new extension of the format,
	-- a dedicated Not_Supported exception is raised in this case.
	Not_Supported:   exception;

	type Decompressor(In_Last: Integer) is tagged limited private;

	-- Initializes a new Decompressor capable of decompressing an entire
	-- LZ4 frame.
	--
	-- @param Input
	-- 	The first input bytes. Need to supply enough such that the
	--	entire frame header can be processed.
	-- @param Num_Consumed
	-- 	Returns the number of input bytes processed
	-- @param Min_Buffer_Size
	-- 	Returns the minimum length of output buffers for decoding this
	--	LZ4 frame. The buffer size returned is not expected to take more
	--	than a single output block at a time. It is usually not useful
	--	to chose a larger buffer size than the reported minimum buffer
	--	size.
	-- @return
	--	Decompressor context
	function Init  (Input:           in  Octets;
			Num_Consumed:    out Integer;
			Min_Buffer_Size: out Integer)
			return Decompressor with Pre => Input'Length >= 7;

	-- Initializes a new Decompressor using Stream_Element_Array/_Offset
	-- data types rather than the library-internal custom data types.
	function Init  (Input:           in  Ada.Streams.Stream_Element_Array;
			Num_Consumed:    out Ada.Streams.Stream_Element_Offset;
			Min_Buffer_Size: out Ada.Streams.Stream_Element_Offset)
			return Decompressor with Pre => Input'Length >= 7;

	-- Decompress data using an “Octets”-based API
	--
	-- @param Ctx
	--	Decompressor Context
	-- @param Input
	--	Any positive (> 0) number of input bytes 
	-- @param Num_Consumed
	--	Reports how many bytes of the input data were processed. This
	--	need not be the same as the number of input bytes supplied
	--	because a small input can cause a large output making it
	--	necessary to pause input data consumption early.
	-- @param Buffer
	--	Buffer maintained between repeated `Update` calls.
	--	The buffer size should be at least `Min_Buffer_Size` and data in
	--	the buffer should not be changed between calls to `Update`.
	-- @param Output_First
	--	Marks the index of the first octet of the decompressed data in
	--	the output buffer (inclusive).
	-- @param Output_Last
	--	Marks the index of the last octet of the decompressed data in
	--	the output buffer (inclusive).
	procedure Update(Ctx:         in out Decompressor;
			Input:        in     Octets;
			Num_Consumed: out    Integer;
			Buffer:       in out Octets;
			Output_First: out    Integer;
			Output_Last:  out    Integer)
			with Pre => (Buffer'First = 0);

	-- Decompress data uing a “Stream_Element_Array”-based API
	--
	-- @param Ctx
	--	Decompressor Context
	-- @param Input
	--	Any positive (> 0) number of input bytes
	-- @param Num_Consumed
	--	After invocation, this is set to the number of input bytes
	--	processed. Generally, this can be less than the supplied input
	--	bytes.
	-- @param Buffer
	--	Buffer maintained between repeated `Update` calls.
	--	The buffer size should be at least `Min_Buffer_Size` and data
	--	in the buffer should not be changed between calls to `Update`.
	-- @param Output_First
	--	Index of the first octet of output in the Buffer (inclusive).
	-- @param Output_Last
	--	Index of the last octet of output in the Buffer (inclusive)
	-- @param Frame_Ended
	--	This Boolean is set to `True` when the LZ4 frame has ended.
	--	This marks the end of the lifecycle of the Decompressor. To
	--	process further frames, create a new Decompressor by calling
	--	`Init`. Do not call `Update` again after receiving a
	--	`Frame_Ended = True` outcome.
	procedure Update(Ctx:         in out Decompressor;
			Input:        in     Ada.Streams.Stream_Element_Array;
			Num_Consumed: out    Ada.Streams.Stream_Element_Offset;
			Buffer:       in out Ada.Streams.Stream_Element_Array;
			Output_First: out    Ada.Streams.Stream_Element_Offset;
			Output_Last:  out    Ada.Streams.Stream_Element_Offset);

	-- Checks the end of frame status of the given Decompressor context.
	--
	-- Applications are encouraged to check that once the input data has
	-- ended, this function does not return `No` because that indicates
	-- a data corrpution.
	--
	-- @param Ctx
	--	Decompressor to check
	-- @return
	--	Yes:    when this frame has definetely ended.
	--	No:     when there is input data in the buffer that has not been
	--	        processed yet.
	--	Maybe:  when legacy format is processed and end of block is
	--	        reached. Legacy format does not signal “end of frame”
	--	        explicitly.
	function Is_End_Of_Frame(Ctx: in Decompressor) return End_Of_Frame;

	-- Useful routines for testing purposes. Not part of the stable API!
	function To_Hex(Num: in U8)  return String;
	function To_Hex(Num: in U32) return String;

	-- XXHash32 Implementation based on the following C++ implementation:
	-- https://github.com/stbrumme/xxhash/blob/master/xxhash32.h
	package XXHash32 is
		type Hasher is tagged limited private;
		-- Initialize the hash function. Without a Seed, `0` is used.
		function  Init return Hasher;
		function  Init(Seed: in U32) return Hasher;
		-- Add data to process
		procedure Update(Ctx: in out Hasher; Input: in Octets);
		-- Compute Hash
		function  Final(Ctx: in Hasher) return U32;
		-- One-Stop API to directly compute the hash of data with Seed=0
		function  Hash(Input: in Octets) return U32;
	private
		Max_Buffer_Size: constant Integer := 16;
		Prime_1:         constant U32     := 2654435761;
		Prime_2:         constant U32     := 2246822519;
		Prime_3:         constant U32     := 3266489917;
		Prime_4:         constant U32     :=  668265263;
		Prime_5:         constant U32     :=  374761393;

		procedure Process(Ctx: in out Hasher; Data: in Octets)
				with Pre => Data'Length = Max_Buffer_Size;
		function Update1(Ctx: in out Hasher; Input: in U8)
								return Boolean;

		type Hasher is tagged limited record
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

	Magic_Modern:     constant U32     := 16#184d2204#;
	Magic_Legacy:     constant U32     := 16#184c2102#;
	History_Size:     constant Integer := 64 * 1024;
	Block_Size_Bytes: constant Integer := 4;

	-- Forward declarations
	procedure Check_Flag_Validity(FLG_Version: in U8;
							Reserved: in Boolean);
	function Block_Size_Table(BD_Block_Max_Size: in U8) return Integer;
	procedure Check_Header_Checksum(Data: in Octets; HC: in U8);
	function Load_64(Data: in Octets) return U64
						with Pre => Data'Length = 8;
	procedure Skip(Ctx: in out Decompressor; Input: in Octets;
						Num_Consumed: out Integer);
	procedure Check_End_Mark(Ctx: in out Decompressor; Input: in Octets;
						Num_Consumed: in out Integer);
	-- Num_Consumed = 0 predondition could be lifted by improving the code
	procedure Try_Detect_Input_Length(Ctx: in out Decompressor;
			Input: in Octets; Num_Consumed: in out Integer)
			with Pre => (Ctx.Input_Buffer_Filled < Block_Size_Bytes
					and Num_Consumed = 0);
	procedure Handle_Newly_Known_Input_Length(Ctx: in out Decompressor;
				Input: in Octets; Num_Consumed: in out Integer;
				Buffer: in out Octets;
				Output_First: in out Integer;
				Output_Last: in out Integer);
	procedure Cache_Data_And_Process_If_Full(Ctx: in out Decompressor;
			Input: in Octets; Num_Consumed: in out Integer;
			Buffer: in out Octets; Output_First: in out Integer;
			Output_Last: in out Integer);
	procedure Decode_Full_Block_With_Trailer(
				Ctx:          in out Decompressor;
				Input_Block:  in     Octets;
				Buffer:       in out Octets;
				Output_First: out    Integer;
				Output_Last:  out    Integer);
	procedure Check_Checksum(Data: in Octets; Expect_Checksum: in U32);
	procedure Update_Checksum(Ctx: in out Decompressor; Outp: in Octets);
	procedure Decompress_Full_Block(Ctx:          in out Decompressor;
					Raw_Data:     in     Octets;
					Buffer:       in out Octets;
					Output_First: out    Integer;
					Output_Last:  out    Integer);
	procedure Write_Output(Ctx: in out Decompressor; Data: in Octets;
					First: in Integer; Last: in Integer;
					Buffer: in out Octets);
	procedure Decrease_Data_Size_Remaining(Ctx: in out Decompressor;
							Data_Length: in U64);
	procedure Output_With_History(Ctx: in out Decompressor;
				Offset: in Integer; Match_Length: in Integer;
				Buffer: in out Octets);

	type Format is (Legacy, Modern, Skippable);

	type Decompressor(In_Last: Integer) is tagged limited record
		Is_Format:               Format;
		Content_Checksum_Length: Integer; -- 0 or 4
		Block_Checksum_Length:   Integer; -- 0 or 4
		Is_At_End_Mark:          Boolean;
		Status_EOF:              End_Of_Frame;
		Input_Buffer:            Octets(0 .. In_Last);
		Output_Pos:              Integer;
		Output_Pos_History:      Integer;
		Input_Buffer_Filled:     Integer; -- how much data is in there
		Input_Length:            Integer; -- Declared current block len
		Is_Compressed:           Boolean; -- current block compressed YN
		Has_Content_Size:        Boolean;
		Content_Size_Remaining:  U64;
		Hash_All_Data:           LZ4Ada.XXHash32.Hasher;
	end record;

	function Load_32(Src: in Octets) return U32
				is (U32(Src(Src'First)) or
				Shift_Left(U32(Src(Src'First + 1)), 8) or
				Shift_Left(U32(Src(Src'First + 2)), 16) or
				Shift_Left(U32(Src(Src'First + 3)), 24))
				with Pre => (Src'Length = 4);

end LZ4Ada;
