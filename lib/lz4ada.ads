-- LZ4 Extractor Library in Ada 1.0.0, (c) 2022 Ma_Sys.ma <info@masysma.net>.
--
-- This implementation has been created according to the LZ4 Block and Frame
-- format specifications taking some loose inspriation also from the
-- implemtnation of other decompressors. The XXHash32 implementation has been
-- directly ported from an existing C++ implementation
-- (c) 2016 Stephan Brumme (see box further down for details).
--
-- This implementation is available under CC0, the full license text follows:
--
-- CC0 1.0 Universal
-- 
--     CREATIVE COMMONS CORPORATION IS NOT A LAW FIRM AND DOES NOT PROVIDE
--     LEGAL SERVICES. DISTRIBUTION OF THIS DOCUMENT DOES NOT CREATE AN
--     ATTORNEY-CLIENT RELATIONSHIP. CREATIVE COMMONS PROVIDES THIS
--     INFORMATION ON AN "AS-IS" BASIS. CREATIVE COMMONS MAKES NO WARRANTIES
--     REGARDING THE USE OF THIS DOCUMENT OR THE INFORMATION OR WORKS
--     PROVIDED HEREUNDER, AND DISCLAIMS LIABILITY FOR DAMAGES RESULTING FROM
--     THE USE OF THIS DOCUMENT OR THE INFORMATION OR WORKS PROVIDED
--     HEREUNDER.
-- 
-- Statement of Purpose
-- 
-- The laws of most jurisdictions throughout the world automatically confer
-- exclusive Copyright and Related Rights (defined below) upon the creator
-- and subsequent owner(s) (each and all, an "owner") of an original work of
-- authorship and/or a database (each, a "Work").
-- 
-- Certain owners wish to permanently relinquish those rights to a Work for
-- the purpose of contributing to a commons of creative, cultural and
-- scientific works ("Commons") that the public can reliably and without fear
-- of later claims of infringement build upon, modify, incorporate in other
-- works, reuse and redistribute as freely as possible in any form whatsoever
-- and for any purposes, including without limitation commercial purposes.
-- These owners may contribute to the Commons to promote the ideal of a free
-- culture and the further production of creative, cultural and scientific
-- works, or to gain reputation or greater distribution for their Work in
-- part through the use and efforts of others.
-- 
-- For these and/or other purposes and motivations, and without any
-- expectation of additional consideration or compensation, the person
-- associating CC0 with a Work (the "Affirmer"), to the extent that he or she
-- is an owner of Copyright and Related Rights in the Work, voluntarily
-- elects to apply CC0 to the Work and publicly distribute the Work under its
-- terms, with knowledge of his or her Copyright and Related Rights in the
-- Work and the meaning and intended legal effect of CC0 on those rights.
-- 
-- 1. Copyright and Related Rights. A Work made available under CC0 may be
-- protected by copyright and related or neighboring rights ("Copyright and
-- Related Rights"). Copyright and Related Rights include, but are not
-- limited to, the following:
-- 
--   i. the right to reproduce, adapt, distribute, perform, display,
--      communicate, and translate a Work;
--  ii. moral rights retained by the original author(s) and/or performer(s);
-- iii. publicity and privacy rights pertaining to a person's image or
--      likeness depicted in a Work;
--  iv. rights protecting against unfair competition in regards to a Work,
--      subject to the limitations in paragraph 4(a), below;
--   v. rights protecting the extraction, dissemination, use and reuse of data
--      in a Work;
--  vi. database rights (such as those arising under Directive 96/9/EC of the
--      European Parliament and of the Council of 11 March 1996 on the legal
--      protection of databases, and under any national implementation
--      thereof, including any amended or successor version of such
--      directive); and
-- vii. other similar, equivalent or corresponding rights throughout the
--      world based on applicable law or treaty, and any national
--      implementations thereof.
-- 
-- 2. Waiver. To the greatest extent permitted by, but not in contravention
-- of, applicable law, Affirmer hereby overtly, fully, permanently,
-- irrevocably and unconditionally waives, abandons, and surrenders all of
-- Affirmer's Copyright and Related Rights and associated claims and causes
-- of action, whether now known or unknown (including existing as well as
-- future claims and causes of action), in the Work (i) in all territories
-- worldwide, (ii) for the maximum duration provided by applicable law or
-- treaty (including future time extensions), (iii) in any current or future
-- medium and for any number of copies, and (iv) for any purpose whatsoever,
-- including without limitation commercial, advertising or promotional
-- purposes (the "Waiver"). Affirmer makes the Waiver for the benefit of each
-- member of the public at large and to the detriment of Affirmer's heirs and
-- successors, fully intending that such Waiver shall not be subject to
-- revocation, rescission, cancellation, termination, or any other legal or
-- equitable action to disrupt the quiet enjoyment of the Work by the public
-- as contemplated by Affirmer's express Statement of Purpose.
-- 
-- 3. Public License Fallback. Should any part of the Waiver for any reason
-- be judged legally invalid or ineffective under applicable law, then the
-- Waiver shall be preserved to the maximum extent permitted taking into
-- account Affirmer's express Statement of Purpose. In addition, to the
-- extent the Waiver is so judged Affirmer hereby grants to each affected
-- person a royalty-free, non transferable, non sublicensable, non exclusive,
-- irrevocable and unconditional license to exercise Affirmer's Copyright and
-- Related Rights in the Work (i) in all territories worldwide, (ii) for the
-- maximum duration provided by applicable law or treaty (including future
-- time extensions), (iii) in any current or future medium and for any number
-- of copies, and (iv) for any purpose whatsoever, including without
-- limitation commercial, advertising or promotional purposes (the
-- "License"). The License shall be deemed effective as of the date CC0 was
-- applied by Affirmer to the Work. Should any part of the License for any
-- reason be judged legally invalid or ineffective under applicable law, such
-- partial invalidity or ineffectiveness shall not invalidate the remainder
-- of the License, and in such case Affirmer hereby affirms that he or she
-- will not (i) exercise any of his or her remaining Copyright and Related
-- Rights in the Work or (ii) assert any associated claims and causes of
-- action with respect to the Work, in either case contrary to Affirmer's
-- express Statement of Purpose.
-- 
-- 4. Limitations and Disclaimers.
-- 
--  a. No trademark or patent rights held by Affirmer are waived, abandoned,
--     surrendered, licensed or otherwise affected by this document.
--  b. Affirmer offers the Work as-is and makes no representations or
--     warranties of any kind concerning the Work, express, implied,
--     statutory or otherwise, including without limitation warranties of
--     title, merchantability, fitness for a particular purpose, non
--     infringement, or the absence of latent or other defects, accuracy, or
--     the present or absence of errors, whether or not discoverable, all to
--     the greatest extent permissible under applicable law.
--  c. Affirmer disclaims responsibility for clearing rights of other persons
--     that may apply to the Work or any use thereof, including without
--     limitation any person's Copyright and Related Rights in the Work.
--     Further, Affirmer disclaims responsibility for obtaining any necessary
--     consents, permissions or other rights required for any use of the
--     Work.
--  d. Affirmer understands and acknowledges that Creative Commons is not a
--     party to this document and has no duty or obligation with respect to
--     this CC0 or use of the Work.

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

	-- Public part of the computation context
	-- Users of the API are expected to process all of the values supplied
	-- this way. These are all `out` parameters in the sense that the API
	-- user should not (need to) write to these variables at any time.
	type Decompression_Status is record
		-- Num_Consumed reports how many bytes of the input data were
		-- processed. This need not be the same as the number of input
		-- bytes supplied because a small input can cause a large output
		-- making it necessary to pause input data consumption early.
		Num_Consumed:    Integer;

		-- Signals that an LZ4 frame has ended. This marks the end of
		-- the current Decompressor's lifecycle. If further frames are
		-- to be processed, `Init` has to be called again to obtain a
		-- new Decompressor.
		Frame_Has_Ended: Boolean;

		-- Marks the index of the first octet of the decompressed data
		-- in the output buffer (inclusive).
		--
		-- Example: To access exctracted data as a slice, denote it as
		--          follows: Buffer(Status.First .. Status.Last);
		First:           Integer;

		-- Marks the index of the last octet of the decompressed data
		-- in the output buffer (inclusive).
		Last:            Integer;
	end record;

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
	function Init  (Input:           in Octets;
			Num_Consumed:    out Integer;
			Min_Buffer_Size: out Integer)
			return Decompressor with Pre => Input'Length >= 7;

	-- Initializes a new Decompressor using Stream_Element_Array/_Offset
	-- data types rather than the library-internal custom data types.
	function Init  (Input:           in Ada.Streams.Stream_Element_Array;
			Num_Consumed:    out Ada.Streams.Stream_Element_Offset;
			Min_Buffer_Size: out Ada.Streams.Stream_Element_Offset)
			return Decompressor with Pre => Input'Length >= 7;

	-- Decompress data.
	-- This variant uses the library-internal custom data types.
	--
	-- @param Ctx
	--	Decompressor Context
	-- @param Input
	--	Any positive (> 0) number of input bytes 
	-- @param Buffer
	--	Buffer maintained between repeated `Update` calls.
	--	The buffer size should be at least `Min_Buffer_Size` and data in
	--	the buffer should not be changed between calls to `Update`.
	-- @param Status
	--	Record containing hints regarding the output.
	--	See record documentation for details.
	procedure Update(Ctx:    in out Decompressor;
			 Input:  in Octets;
			 Buffer: in out Octets;
			 Status: out Decompression_Status)
			with Pre => (Buffer'First = 0);

	-- Decompress data.
	-- This variant uses standard types. This makes the interface more
	-- verbose as multiple related parameters cannot be grouped to a record
	-- this way.
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
	procedure Update(Ctx:          in out Decompressor;
			 Input:        in Ada.Streams.Stream_Element_Array;
			 Num_Consumed: out Ada.Streams.Stream_Element_Offset;
			 Buffer:       in out Ada.Streams.Stream_Element_Array;
			 Output_First: out Ada.Streams.Stream_Element_Offset;
			 Output_Last:  out Ada.Streams.Stream_Element_Offset;
			 Frame_Ended:  out Boolean);

	-- Useful routines for testing purposes. Not part of the stable API!
	function To_Hex(Num: in U8)  return String;
	function To_Hex(Num: in U32) return String;

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
	-- | damages arising from the use of this software.                    |
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
					Status: in out Decompression_Status);
	procedure Check_End_Mark(Ctx: in out Decompressor; Input: in Octets;
					Status: in out Decompression_Status);
	-- Num_Consumed = 0 predondition could be lifted by improving the code
	procedure Try_Detect_Input_Length(Ctx: in out Decompressor;
			Input: in Octets; Num_Consumed: in out Integer)
			with Pre => (Ctx.Input_Buffer_Filled < Block_Size_Bytes
					and Num_Consumed = 0);
	procedure Handle_Newly_Known_Input_Length(Ctx: in out Decompressor;
				Input: in Octets; Buffer: in out Octets;
				Status: in out Decompression_Status);
	procedure Cache_Data_And_Process_If_Full(Ctx: in out Decompressor;
				Input: in Octets; Buffer: in out Octets;
				Status: in out Decompression_Status);
	procedure Decode_Full_Block_With_Trailer(
				Ctx:         in out Decompressor;
				Input_Block: in Octets;
				Buffer:      in out Octets;
				Status:      in out Decompression_Status);
	procedure Check_Checksum(Data: in Octets; Expect_Checksum: in U32);
	procedure Update_Checksum(Ctx: in out Decompressor;
					Buffer: in Octets;
					Status: in Decompression_Status);
	procedure Decompress_Full_Block(Ctx:    in out Decompressor;
					Raw_Data: in Octets;
					Buffer: in out Octets;
					Status: in out Decompression_Status);
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
