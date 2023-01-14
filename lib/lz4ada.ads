-- LZ4 Extractor Library in Ada 1.0.0,
-- (c) 2022, 2023 Ma_Sys.ma <info@masysma.net>.
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
-- Copyright (c) 2022, 2023 Ma_Sys.ma <info@masysma.net>
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
use  Ada.Streams;
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

	pragma Assertion_Policy(Pre => Check, Post => Check);

	subtype U8  is Interfaces.Unsigned_8;
	subtype U32 is Interfaces.Unsigned_32;
	subtype U64 is Interfaces.Unsigned_64;
	type Octets is array (Integer range <>) of U8;

	type Flexible_Memory_Reservation is (SZ_64_KiB, SZ_256_KiB, SZ_1_MiB,
				SZ_4_MiB, SZ_8_MiB, Use_First, Single_Frame);
	subtype Memory_Reservation is Flexible_Memory_Reservation range
							SZ_64_KiB .. SZ_8_MiB;
	For_Modern: constant Memory_Reservation := SZ_4_MiB;
	For_Legacy: constant Memory_Reservation := SZ_8_MiB;
	For_All:    constant Memory_Reservation := SZ_8_MiB;

	type End_Of_Frame is (Yes, No, Maybe);

	Checksum_Error:  exception;
	Data_Corruption: exception;
	Not_Supported:   exception;
	Data_Too_Large:  exception;
	Data_Too_Small:  exception;
	No_Progress:     exception;

	type Decompressor(In_Last: Integer) is tagged limited private;

	function Init(Min_Buffer_Size:   out    Stream_Element_Offset;
			Reservation:     in     Memory_Reservation := For_All)
			return Decompressor;

	procedure Update(Ctx:            in out Decompressor;
			Input:           in     Stream_Element_Array;
			Num_Consumed:    out    Stream_Element_Offset;
			Buffer:          in out Stream_Element_Array;
			Output_First:    out    Stream_Element_Offset;
			Output_Last:     out    Stream_Element_Offset);

	function Init(Min_Buffer_Size:   out    Integer;
			Reservation:     in     Memory_Reservation := For_All)
			return Decompressor;

	function Init_With_Header(Input: in     Octets;
			Num_Consumed:    out    Integer;
			Min_Buffer_Size: out    Integer;
			Reservation:     in     Flexible_Memory_Reservation
								:= Single_Frame)
			return Decompressor with Pre => Input'Length >= 7;

	procedure Update(Ctx:            in out Decompressor;
			Input:           in     Octets;
			Num_Consumed:    out    Integer;
			Buffer:          in out Octets;
			Output_First:    out    Integer;
			Output_Last:     out    Integer)
			with Pre => (Buffer'First = 0);

	function Is_End_Of_Frame(Ctx: in Decompressor) return End_Of_Frame;

	-- Useful routines for testing purposes. Not part of the stable API!
	function To_Hex(Num: in U8)  return String;
	function To_Hex(Num: in U32) return String;

	-- XXHash32 Implementation based on the following C++ implementation:
	-- https://github.com/stbrumme/xxhash/blob/master/xxhash32.h
	package XXHash32 is
		type Hasher is tagged limited private;
		-- Initialize the hash function.
		function  Init(Seed: in U32 := 0) return Hasher;
		procedure Reset(Ctx: in out Hasher; Seed: in U32 := 0);
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
	end XXHash32;

private

	Magic_Modern:     constant U32     := 16#184d2204#;
	Magic_Legacy:     constant U32     := 16#184c2102#;
	History_Size:     constant Integer := 64 * 1024;
	Block_Size_Bytes: constant Integer := 4;

	subtype Magic_Skippable is U32 range 16#184d2a50# .. 16#184d2a5f#;

	type Format is (TBD, Legacy, Modern, Skippable);
	type Header_Parsing_State is (Need_Magic, Need_Modern, Need_Flags,
					Need_Skippable_Length, Header_Complete);

	type Decompressor_Meta is record
		Is_Format:               Format               := TBD;
		Header_Parsing:          Header_Parsing_State := Need_Magic;
		Memory_Reservation:      Flexible_Memory_Reservation;
		Content_Checksum_Length: Integer              := 0; -- 0 or 4
		Block_Checksum_Length:   Integer              := 0; -- 0 or 4
		Status_EOF:              End_Of_Frame         := No;
		Input_Buffer_Filled:     Integer              := 0;
		Is_Compressed:           Boolean              := False;
		Has_Content_Size:        Boolean              := False;
		Size_Remaining:          U64                  := 4;
	end record;

	-- Forward declarations
	function Get_Block_Size(R: in Memory_Reservation) return Integer;
	procedure Process_Header_Bytes(M: in out Decompressor_Meta;
				Input_Buffer: in out Octets; Input: in Octets;
				Num_Consumed: out Integer)
				with Pre => M.Header_Parsing /= Header_Complete;
	procedure Process_Header_Magic(M: in out Decompressor_Meta;
						Input_Buffer: in Octets);
	procedure Process_Header_Magic(M: in out Decompressor_Meta;
							Magic_NB: in U32);
	procedure Process_Legacy_End_Of_Header(M: in out Decompressor_Meta);
	procedure Process_Header_Flags(M: in out Decompressor_Meta;
						Input_Buffer: in Octets);
	procedure Check_Flag_Validity(FLG_Version: in U8; Reserved: in Boolean);
	function Get_Block_Size_Reservation(BD_Block_Max_SZ: in U8)
						return Memory_Reservation;
	procedure Check_Reservation(Requested: in Flexible_Memory_Reservation;
					Effective: in out Memory_Reservation);
	procedure Process_Modern_End_Of_Header(M: in out Decompressor_Meta;
						Input_Buffer: in Octets);
	function Load_64(Data: in Octets) return U64
						with Pre => Data'Length = 8;
	procedure Check_Header_Checksum(Data: in Octets; HC: in U8);

	procedure Skip(Ctx: in out Decompressor; Input: in Octets;
						Num_Consumed: out Integer);
	procedure Reset_For_Next_Frame(Ctx: in out Decompressor;
				Input: in Octets; Num_Consumed: out Integer);
	procedure Reset_Outer_For_Next_Frame(Ctx: in out Decompressor);
	procedure Check_End_Mark(Ctx: in out Decompressor; Input: in Octets;
						Num_Consumed: in out Integer);
	-- Num_Consumed = 0 precondition could be lifted by improving the code
	procedure Try_Detect_Input_Length(Ctx: in out Decompressor;
			Input: in Octets; Num_Consumed: in out Integer)
			with Pre => (Ctx.M.Input_Buffer_Filled <
					Block_Size_Bytes and Num_Consumed = 0);
	function Is_Any_Magic_Number(Candidate: in U32) return Boolean;
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

	type Decompressor(In_Last: Integer) is tagged limited record
		M:                  Decompressor_Meta;
		Is_At_End_Mark:     Boolean              := False;
		Input_Buffer:       Octets(0 .. In_Last) := (others => 0);
		Output_Pos:         Integer              := 0;
		Output_Pos_History: Integer              := 0;
		-- Declared current block length
		Input_Length:       Integer              := -1;
		Hash_All_Data:      LZ4Ada.XXHash32.Hasher;
	end record;

	function Load_32(Src: in Octets) return U32
				is (U32(Src(Src'First)) or
				Shift_Left(U32(Src(Src'First + 1)), 8) or
				Shift_Left(U32(Src(Src'First + 2)), 16) or
				Shift_Left(U32(Src(Src'First + 3)), 24))
				with Pre => (Src'Length = 4);

end LZ4Ada;
