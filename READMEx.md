---
section: 32
x-masysma-name: lz4_ada
title: LZ4 Decompressor for Ada
date: 2022/12/14 20:49:20
lang: en-US
author: ["Linux-Fan, Ma_Sys.ma (info@masysma.net)"]
keywords: ["lz4", "decompress", "ada", "library"]
x-masysma-version: 1.0.0
x-masysma-website: https://masysma.net/32/lz4_ada.xhtml
x-masysma-repository: https://www.github.com/m7a/bo-lz4-ada
x-masysma-copyright: "2022, 2023 Ma_Sys.ma <info@masysma.net>"
---
Abstract
========

This repository provides an Ada implementation of an LZ4 Decompressor
(cf. <https://lz4.github.io/lz4/> for general information on LZ4).

This implementation only supports decompression!

 * LZ4 Frame Format is supported according to specification v.1.6.3 (2022/09/12)
 * Legacy Frame Format is supported.
 * Skippable Frames are supported.
 * Provided checksums are verified.
 * Provided length information is verified.
 * Big Endian architectures are _UNSUPPORTED_.

Performance is slower compared to reference and C implementations: On my system,
the library decompresses slower than 1100 MiB/s. The Debian-supplied `unlz4`
program for instance, achieves over 3000 MiB/s. See section _Performance_.

License
=======

This library is available under the Expat License.
See `LICENSE.txt` or `lz4ada.ads` for details.

Compiling
=========

The following dependencies are required for building:

 * Ada compiler (`gcc`, `gnatmake`)
 * Ant build tool (`ant`)

## Compile

	ant

## Run Tests

	ant
	./test_run.sh

## Install

It is advisable to generate a package by means of `ant package`. Alternatively,
install the library directly using the following commands (or similar depending
on your OS):

	install -DsT lib/liblz4ada.so /usr/local/lib/x86_64-linux-gnu
	install -m 644 -DT lib/lz4ada.ali /usr/local/lib/x86_64-linux-gnu/ada/adalib/lz4
	install -m 644 -DT lib/lz4ada.ads /usr/local/share/ada/adainclude/lz4

Repository Structure
====================

This repository contains multiple subdirectories for the various components of
the library.

~~~
/bo-lz4-ada
   │
   ├── lib/
   │    │
   │    ├── lz4ada.adb                *** This is the implementation. ***
   │    │
   │    ├── lz4ada.ads                *** Implementation header file. ***
   │    │
   │    └── build.xml                 Build instructions
   │
   ├── test_suite/                    Test Suite for LZ4 Library
   │    │
   │    ├── lz4test.adb               Test Suite Implementation
   │    │
   │    └── build.xml                 Build, run and coverage instructions
   │
   ├── test_vectors_lz4/              Sample Data for Testing
   │    │
   │    ├── ....bin                   Original data
   │    │
   │    └── ....lz4                   Compressed data
   │
   ├── tool_unlz4ada/
   │    │
   │    └── unlz4ada.adb              Example program capable of decompressing
   │                                  multiple, concatenated LZ4 Frames.
   │
   ├── tool_lz4hdrinfo/
   │    │
   │    └── lz4hdrinfo.adb            Debugging tool to decode LZ4 frame header.
   │
   ├── tool_unlz4ada_singleframe/
   │    │
   │    └── unlz4ada_singleframe.adb  Minimal example to decompress one frame.
   │
   ├── tool_xxhash32ada/
   │    │
   │    └── xxhash32ada.adb           Auxiliary tool to demonstrate computing
   │                                  the XXHash32 Hash function.
   │
   ├── test_benchmark.sh              Script to invoke a minimal benchmark.
   │
   ├── test_run.sh                    Script to test against the test vectors.
   │
   ├── README.md                      This file.
   │
   ├── LICENSE.txt                    Expat license.
   │
   └── build.xml                      Recursive antfile build instructions.
~~~

The important subdirectory regarding the library is `lib`. If you do not
need tests or example programs, it is sufficient to compile and use only the
files from that directory.

Sample Program
==============

Unfortunately, given the interesting property of decompression that a small
input can produce a larger output and given that the library is intended to
achieve decent performance, setting up a minimal example is already nontrivial.

The following code `unlz4ada_singleframe.adb` demonstrates a fully-working
LZ4 decompression using the library. It is limited to decoding just a single
LZ4 frame (which may well suffice for many applications). If you'd rather prefer
a variant that decodes consecutive frames, have a look at the example
`unlz4ada.adb` in directory `tool_unlz4ada`.

~~~{.ada}
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Streams;
use  Ada.Streams;
with LZ4Ada;

procedure UnLZ4Ada_Singleframe is
	-- 1.
	Stdin:  constant access Root_Stream_Type'Class :=
		Ada.Text_IO.Text_Streams.Stream(Ada.Text_IO.Standard_Input);
	Stdout: constant access Root_Stream_Type'Class :=
		Ada.Text_IO.Text_Streams.Stream(Ada.Text_IO.Standard_Output);
	Buf_In: Stream_Element_Array(0 .. 4095); -- 4k buffer
	Last:   Stream_Element_Offset;
begin
	-- 2.
	Read(Stdin.all, Buf_In, Last);
	if Last < 6 then
		raise Constraint_Error with "LZ4 Frame Header too short.";
	end if;
	declare
		Total_Consumed, Req_Buffer_Size: Stream_Element_Offset;
		Ctx: LZ4Ada.Decompressor := LZ4Ada.Init(Buf_In(0 .. Last),
					Total_Consumed, Req_Buffer_Size);
		-- 3.
		Buf: Stream_Element_Array(1 .. Req_Buffer_Size);
		Consumed, Output_First, Output_Last: Stream_Element_Offset;
	begin
		-- 4.
		loop
			if Total_Consumed > Last then
				Read(Stdin.all, Buf_In, Last);
				exit when Last < 0;
				Total_Consumed := 0;
			end if;
			Ctx.Update(Buf_In(Total_Consumed .. Last), Consumed,
						Buf, Output_First, Output_Last);
			Write(Stdout.all, Buf(Output_First .. Output_Last));
			Total_Consumed := Total_Consumed + Consumed;
		end loop;
		-- 5.
		if LZ4Ada."="(Ctx.Is_End_Of_Frame, LZ4Ada.No) then
			raise Constraint_Error with "Input ended mid-frame.";
		end if;
	end;
end UnLZ4Ada_Singleframe;
~~~

Here is how the sample program works:

 1. `Stdin` and `Stdout` allow accessing the respective streams for binary
    input/output. `Buf_In` defines an input buffer with an arbitrary size.
    In this example, a 4 KiB buffer is allocated.
 2. Initially, a single buffer of data is read from stdin.
    This buffer contains enough of the LZ4 data to parse the header and
    allocate a context.
 3. After parsing the header, the expected buffer size is known
    (`Req_Buffer_Size`) and a suitable buffer is allocated.
 4. The remainder of the data can be parsed in a loop:
     * If all buffered input data has been processed already
       (`Total_Consumed > Last`) then new data is read from `Stdin`.
     * `Ctx.Update` is invoked with the current chunk of input data to process.
     * `Write` is called to output any data decompressed by the `Update` call.
 5. After processing, condition `Ctx.Is_End_Of_Frame = No` is checked because:
    If this is not the end of frame, processing has ended mid-frame and the
    output is most likely to be incomplete.

Using the installed Library
===========================

Assuming the library is already installed on your system, you can compile
and run the sample program as follows:

	gnatmake -o unlz4ada unlz4ada.adb \
		-aO/usr/lib/x86_64-linux-gnu/ada/adalib/lz4 \
		-aI/usr/share/ada/adainclude/lz4 \
		-largs -llz4ada
	./unlz4ada < ../test_vectors_lz4/z1.lz4 | xxd

Output: `00000000: 00                                       .`

Using the Library without Installation
======================================

If the library is not installed on your system, it can be integrated using
multiple different approaches.

## Easy Vendoring

The quickest way to get started is to just include the `lz4ada.ads` and
`lz4ada.adb` files into the source tree.

	cp ../lib/lz4ada.ad? .

Here is what the directory structure may look like then:

~~~
  ...
   │
   ├── tool_unlz4ada/
   │    └── lz4ada.adb
   │    └── unlz4ada.adb
   │    └── lz4ada.ads
   │
  ...
~~~

Compilation and invocation then become trivial:

	gnatmake -o unlz4ada unlz4ada.adb
	./unlz4ada < ../test_vectors_lz4/z1.lz4 | xxd

Output: `00000000: 00                                       .`

## Inclusion from different directory

It may not be suitable to just copy-over the files. In this case, it is also
possible to import the compiled library from a different directory. Assume
that the library is compiled but not installed, then the file structure may
look as follows:

~~~
  ...
   │
   ├── lib/
   │    ├── lz4ada.adb
   │    ├── lz4ada.ads
   │    ├── lz4ada.ali
   │    ├── build.xml
   │    └── liblz4ada.so
   │
   ├── test_unlz4ada/
   │    ├── unlz4ada.adb
   │    └── build.xml
  ...
~~~

Compilation and invocation then have to account for the library not being
installed as follows:

	gnatmake -o unlz4ada unlz4ada.adb -aO../lib -aI../lib -largs -llz4ada
	LD_LIBRARY_PATH=$PWD/../lib ./unlz4ada < ../test_vectors_lz4/z1.lz4 | xxd

Decompression API
=================

This section describes the decompression API provided by this library. There is
also an API to make use of the XXHash32 code directly, see `XXHash32 API`
further down. Some of the data types are shared among both of the APIs and
only described here.

## Types

~~~{.ada}
subtype U8  is Interfaces.Unsigned_8;
subtype U32 is Interfaces.Unsigned_32;
subtype U64 is Interfaces.Unsigned_64;
type Octets is array (Integer range <>) of U8;
type End_Of_Frame is (Yes, No, Maybe);
type Decompressor(In_Last: Integer) is tagged limited private;
~~~

 * `U8`:           This type represents a single byte.
 * `U32`:          This type represents a 32 bit word.
 * `Octets`:       This type represents a byte string.
 * `End_Of_Frame`: This type represents the state of processing.
   It is a tristate value `Yes/No/Maybe` with the following meaning:
    * `Yes` indicates that the frame has ended.
    * `No` indicates that a frame is being processed and the current state
      cannot be the end.
    * `Maybe` indicates a special case: When a legacy frame is processed, the
      end of data may occur after any full block. `Maybe` indicates such an end
      of block that could be the end of the stream already. An application using
      the library should use external knowledge about the end of input data to
      decide whether this `Maybe` is an actual end of stream or just a temporary
      state that occurs before the next block begins. If an application intends
      to parse only non-legacy frames, it could as well raise an exception upon
      reaching this `Maybe` state.
 * `Decompressor`: This type represents an opaque context of operation.

As an alternative to the types defined by this library, the standard
`Stream_Element_Array` and `Stream_Element_Offset` types can be used. In this
case it is assumed that the stream elements are indeed bytes (this is not
required by the standard!).

There is currently no standard alternative for the `End_Of_Frame` type since
it has a peculiar tristate meaning (see above).

## API Rationale

This section describes some of the thoughts behind the API design. They may help
API users understand the overall idea behind the API better and are less focused
on the API usage.

### No Context Re-Use

The `Decompressor` contexts provided by this API can be used to decompress
one frame only. This is due to the fact that an internal buffer is allocated
according to the largest block contained in the LZ4 frame.

Since other frames may define another largest block size, a `Decompressor`
cannot be re-used in the general case. It also does not seem sensible to provide
a reset function that can only “sometimes” work i.e. when the new max block size
is smaller or equal compared to the previous one.

Hence, users must create a new `Decompressor` to decompress another LZ4 frame.

### `Min_Buffer_Size` Requirement

The minimum output buffer size is at least a single block size. If the output
buffer were possible to be chosen even smaller, internal computation would be
much more complicated since it would be necessary to pause and resume output
mid-buffer. Allowing the routines to assume that there is enough space for at
least _one_ buffer, makes the handling less complicated without impacting
performance.

### About `Buffer` and `Num_Consumed`

A small input may lead to a large output. To avoid using unbounded memory
amounts one must limit the output buffer size. The library implementation
ensures this by passing a target buffer as an `in out` parameter rather than a
return value. A supplied input's decompressed size may exceed the output buffer
capacity. In order to allow output for the remainder of the input to be
generated, it may become necessary to supply part of the same input again. This
is achieved by signalling the number of consumed bytes back to the caller
as `Num_Consumed`.

In addition to the output data block, the buffer also contains history
information that is used for “backreferences” during decompressing. Storing the
history information in the same buffer requires it to be an `in out` parameter
that must not be changed between invocations of the `Update` procedure. While
this slightly undercuts encapsulation, it can have a notable performance impact:
As all data from the history has been an output at some time, it makes sense to
try storing it only once i.e. as output without keeping a separate copy as
“history”. As a result, fewer copy operations are necessary, yielding better
overall performance.

### No `Final`

There is no need for a “Final” procedure: For non-legacy frames, LZ4 clearly
indicates when processing has reached the end of the frame. For legacy frames,
the library reports the end of block as `Maybe` end of frame.

This makes `Final` sort of an optional check to see if the end of input data
correlates whith LZ4's understanding of the end of frame. The recommended way
to perform this check in applications that include support for legacy frames
is as follows:

~~~{.ada}
if LZ4Ada."="(Ctx.Is_End_Of_Frame, LZ4Ada.No) then
	raise Constraint_Error with "Input ended mid-frame.";
end if;
~~~

Alternatively, other exception types might be suitable, e.g. the
library-supplied `Data_Corruption` could also be a sensible choice here.

### `No_Progress` Exception

During the development of the library, it was observed that under certain
circumstances, a naively implemented “single-frame decompression” (like the
single frame decompression proposed above) could run into an endless loop if it
was presented with data that it had not been originally intended to decode. Such
data included invalid data with arbitrary trailing bytes after the end of frame,
e.g. file `tralingbytes.err` and cases of multiple, validly concatenated frames.

The issue could be tracked down to the `Update` procedure which would (not) report
such cases by not consuming any more input data (since the frame had ended
already) and by not producing any more output data (also since the frame had
ended already). Problem was: Naive library usage may not expect this case to
occur.

While it is possible to check this explicitly, it seems safer to avoid this trap
at the API design level and produce a meaningful exception for such cases. Thus
the `No_Progress` exception was created.

If your application needs to be able to call `Update` repeatedly even if no
data is processed this way -- say, e.g. you'd hardcode to call it four times
in a row without any intermediate checks or something, then it is still possible
to achieve the previous behaviour by creating your own auxiliary procedure
that ignores `No_Progress` exceptions.

In most cases, it is expected that a library user will not call the `Update`
procedure more often than necessary, though. Hence it seems to be a sensible
tradeoff to bother with the `No_Progress` exception only in exceptional cases
and provide enhanced safety in all the naive and usual cases.

## Exceptions

Any of these exceptions can be thrown by the `Init` and `Update` procedures.

 * `Checksum_Error`:
    This exception is raised whenever an LZ4 checksum does not match.
    The library does not currently support bypassing the checksum
    verification.
 * `Data_Corruption`:
   This exception is raised whenever internal assumptions of the LZ4
   frame or block format are violated. It indicates non-LZ4 or corrupted
   input data.
 * `Not_Supported`:
   This exception is raised whenever values observed that the LZ4
   specification reports as reserved. As such, the values could indicate
   newer data formats/features being in use. As this need not be
   corrupted data but could be a valid new extension of the format,
   a dedicated Not_Supported exception is raised in this case.
 * `No_Progress`:
   This exception is raised whenever `Update` is called again after an end of
   frame condition has already been reached and was possible to detect by the
   library user already. If the library user's implementation ensures proper
   termination even in cases where none of the supplied data is consumed
   _and also_ no new output is generated then it may be safe to ignore this
   exception. In this sense it can be seen as conceptually similar but not
   exactly equal to the `End_Error` from the Ada Standard Library.

## Functions and Procedures

A detailed description of the API functions follows after the overview excerpt
from `lz4ada.ads`.

~~~{.ada}
function Init  (Input:           in Octets;
		Num_Consumed:    out Integer;
		Min_Buffer_Size: out Integer)
		return Decompressor with Pre => Input'Length >= 7;
function Init  (Input:           in Ada.Streams.Stream_Element_Array;
		Num_Consumed:    out Ada.Streams.Stream_Element_Offset;
		Min_Buffer_Size: out Ada.Streams.Stream_Element_Offset)
		return Decompressor with Pre => Input'Length >= 7;

procedure Update(Ctx:          in out Decompressor;
		 Input:        in     Octets;
		 Num_Consumed: out    Integer;
		 Buffer:       in out Octets;
		 Output_First: out    Integer;
		 Output_Last:  out    Integer)
		 with Pre => (Buffer'First = 0);
procedure Update(Ctx:          in out Decompressor;
		 Input:        in Ada.Streams.Stream_Element_Array;
		 Num_Consumed: out Ada.Streams.Stream_Element_Offset;
		 Buffer:       in out Ada.Streams.Stream_Element_Array;
		 Output_First: out Ada.Streams.Stream_Element_Offset;
		 Output_Last:  out Ada.Streams.Stream_Element_Offset;
		 Frame_Ended:  out Boolean);

function Is_End_Of_Frame(Ctx: in Decompressor) return End_Of_Frame;
~~~

### `function Init(Input: in; Num_Consumed: out; Min_Buffer_Size: out) return Decompressor`

This function creates an LZ4 decompression context. It requires the begin of
the frame to be supplied as `Input` and returns the decompression context.

Additionally, it outputs as `Num_Consumed` how many of the supplied bytes it
processed and as `Min_Buffer_Size` the suggested buffer size to be used for
`Update` calls. The bytes consumed by `Init` must not be sent to subsequent
`Update` calls, i.e. if there is still some data left in the input buffer, then
the first `Update` call is expected to take `Input(Num_Consumed .. Input'Last)`
rather than `Input` directly.

This function can be called with either `Octets` as data type and `Integer` as
number type or `Stream_Element_Array` and `Stream_Element_Offset` respectively.

### `procedure Update(Ctx: in out; Input: in, Num_Consumed: out, Buffer: in out; Output_First: out; Output_Last: out)`

This procedure can be called to decompress data.

`Ctx` and `Buffer` together form the context that is expected to be provided
each time data from the same LZ4 stream is to be decompressed.

`Input` must always point to previously unprocessed data. Check the value of
`Num_Consumed` after each invocation to find out how many of the input bytes
must be skipped (e.g. by using slice notation) when invoking `Update` again on
the same `Input` buffer.

`Buffer` must not be modified between invocations of `Update` since it is used
to hold “history” information about previously produced output that is integral
to the decompression process.

`Output_First` marks the index of the first octet of the decompressed data in
the `Buffer` (inclusive).

`Output_Last` marks the index of the last octet of the decompressed data in the
output buffer (inclusive).

If no output was generated, then `Output_Last` is smaller than `Output_First`.

The procedure exists in two variants: One with `Octets` and `Integer` and one
with `Stream_Element_Array` and `Stream_Element_Offset` types for ease of
integration with the Standard Stream APIs.

### `function Is_End_Of_Frame(Ctx: in) return End_Of_Frame;`

This function returns the “end-of-frame” state of the decompressor. See the
`End_Of_Frame` type's description for the meanings of the values.

Applications are expected to check this value, but depending on implementation
and intended formats to support, this check can happen at different times:

 * One variant is to check the value at the end of processing only
   (see example code `unlz4ada_singleframe.adb`). If end of frame is not checked
   explicitly, it may be reported by `No_Progress` exceptions in case an
   implementation which only expects to process a single LZ4 frame is passed
   data consisting of multiple frames.
 * Alternatively, the value can be checked for each invocation of `Update`
   for cases where the application intends to decode multiple, consecutive
   LZ4 frames. An example of this variant can be found in file
   `test_unlz4ada/unlz4ada.adb`.
 * Some of the API uses may even work without calling the function at all. This
   is a viable option if the decompressed data's validity is checked externally
   (e.g. by means of a cryptographic hash function) and there might thus be no
   need to find out if decompression was completed at the time of decompression
   already.

XXHash32 API
============

The subpackage `LZ4Ada.XXHash32` provides access to the `XXHash32` hash
function which is used internally by LZ4 but might be interesting to be used
in other contexts, too. Since I expect such use cases to be niche, no API using
the `Stream_Element_Array` types is provided here.

~~~{.ada}
package XXHash32 is
	type Hasher is tagged limited private;
	function  Init return Hasher;
	function  Init(Seed: in U32) return Hasher;
	procedure Update(Ctx: in out Hasher; Input: in Octets);
	function  Final(Ctx: in Hasher) return U32;
	function  Hash(Input: in Octets) return U32;
private
	-- ...
end XXHash32;
~~~

Type `Hasher` represents the internal computation context of the hash function,
suitable for computing a single `XXHash32`. There is currently no API to
reset the hasher after use, hence API users are suggested to create new `Hasher`
contexts whenever they need to compute the hash of different data.

## Functions and Procedures

The hashing API functions closely resemble the standard pattern of
`Init/Update/Final` with an additional convenience function that can be used
to perform the entire computation in a single step:

### `function Hash(Input: in) return U32;`

As a convenient means to compute the hash over input data without having to
call any of the `Init/Update/Final` routines, function `Hash` can be used to
compute the hash over the given `Input` data in a single step.

In case the input data is large, it might be better to design using the
`Init/Update/Final` set of functions since those allow processing arbitrarily
long data whereas `Hash` expects all of the input to be present in memory
at once.

### `function Init return Hasher;`

This function creates and returns a `Hasher` instance using `Seed => 0`.

### `function Init(Seed: in) return Hasher;`

This function creates and returns a `Hasher` instance using the supplied seed
value.

### `procedure Update(Ctx: in out; Input: in);`

Use this procedure to supply the data that is considered input into the hashing
function.

### `function Final(Ctx: in) return U32;`

This function outputs the 32-bit hash corresponding to the concatenation of all
data supplied with `Update` to the provided `Hasher` context and returns it,

It is possible to call `Update` on the same context again afterwards to append
input data and then invoke `Final` again to obtain the hash of the concatenation
of all preceding plus the newly added input data.

Performance
===========

On my system (Intel Xeon W-2295, inside a test VM), the following decompression
speeds are observed when running the `test_benchmark.sh` script:

	$ ./test_benchmark.sh -h
	benchmark zeroes
	Benchmark 1: ./tool_unlz4ada/unlz4ada < /tmp/zeroes.lz4
	  Time (mean ± σ):     978.1 ms ±  37.7 ms    [User: 971.0 ms, System: 6.7 ms]
	  Range (min … max):   939.5 ms … 1143.0 ms    50 runs
	
	benchmark reference zeroes
	Benchmark 1: unlz4 < /tmp/zeroes.lz4
	  Time (mean ± σ):     739.3 ms ±  23.9 ms    [User: 723.5 ms, System: 15.5 ms]
	  Range (min … max):   696.5 ms … 799.2 ms    50 runs
	 
	benchmark random
	Benchmark 1: ./tool_unlz4ada/unlz4ada < /tmp/random.lz4
	  Time (mean ± σ):      1.847 s ±  0.038 s    [User: 1.505 s, System: 0.342 s]
	  Range (min … max):    1.788 s …  1.961 s    50 runs
	 
	benchmark reference random
	Benchmark 1: unlz4 < /tmp/random.lz4
	  Time (mean ± σ):     649.2 ms ±  19.9 ms    [User: 378.6 ms, System: 270.2 ms]
	  Range (min … max):   616.1 ms … 701.8 ms    50 runs
	 
	benchmark text
	Benchmark 1: ./tool_unlz4ada/unlz4ada < /tmp/text.lz4
	  Time (mean ± σ):      1.863 s ±  0.043 s    [User: 1.523 s, System: 0.339 s]
	  Range (min … max):    1.791 s …  1.994 s    50 runs
	 
	benchmark reference text
	Benchmark 1: unlz4 < /tmp/text.lz4
	  Time (mean ± σ):     644.6 ms ±  20.2 ms    [User: 372.1 ms, System: 272.2 ms]
	  Range (min … max):   604.5 ms … 689.2 ms    50 runs
	
	$ ./test_benchmark.sh
	benchmark zeroes
	0+32768 records in
	0+32768 records out
	2147483648 bytes (2.1 GB, 2.0 GiB) copied, 1.4438 s, 1.5 GB/s
	
	benchmark reference zeroes
	0+32897 records in
	0+32897 records out
	2147483648 bytes (2.1 GB, 2.0 GiB) copied, 1.52586 s, 1.4 GB/s
	
	benchmark random
	0+32768 records in
	0+32768 records out
	2147483648 bytes (2.1 GB, 2.0 GiB) copied, 3.07091 s, 699 MB/s
	
	benchmark reference random
	0+65303 records in
	0+65303 records out
	2147483648 bytes (2.1 GB, 2.0 GiB) copied, 1.02479 s, 2.1 GB/s
	
	benchmark text
	0+32768 records in
	0+32768 records out
	2147483648 bytes (2.1 GB, 2.0 GiB) copied, 3.20152 s, 671 MB/s
	
	benchmark reference text
	0+33049 records in
	0+33049 records out
	2147483648 bytes (2.1 GB, 2.0 GiB) copied, 0.973975 s, 2.2 GB/s

Computing the speed from the measures yields the following results:

Case    Ref/Ada  DD [MiB/s]  Avg [MiB/s]  Min/6σ [MiB/s]  Max/6σ [MiB/s]  PercOfRef [%]
------  -------  ----------  -----------  --------------  --------------  -------------
Zero    Ada      1418        2094         1701            2723            76
Zero    Ref.     1342        2770         2320            3437            106
Random  Ada      667         1109         987             1265            35
Random  Ref.     1998        3155         2665            3866            33
Text    Ada      639         1099         966             1276            30
Text    Ref.     2103        3177         2674            3913            35

Raw Computation

	“Zero” Row
	2048/(1.4438);
	2048/(0.9781);
	2048/(0.9781+6*0.0377);
	2048/(0.9781-6*0.0377);
	(1/0.9781)/(1/0.7393);
	2048/(1.52586);
	2048/(0.7393);
	2048/(0.7393+6*0.0239);
	2048/(0.7393-6*0.0239);
	(1/1.4438)/(1/1.52586);
	
	“Random” Row
	2048/(3.07091)
	2048/(1.847);
	2048/(1.847+6*0.038);
	2048/(1.847-6*0.038);
	(1/1.847)/(1/0.6492);
	2048/(1.02479);
	2048/(0.6492);
	2048/(0.6492+6*0.0199);
	2048/(0.6492-6*0.0199);
	(1/3.07091)/(1/1.02479);
	
	“Text” Row
	2048/(3.20152);
	2048/(1.863);
	2048/(1.863+6*0.043);
	2048/(1.863-6*0.043);
	(1/1.863)/(1/0.6446);
	2048/(0.973975);
	2048/(0.6446);
	2048/(0.6446+6*0.0202);
	2048/(0.6446-6*0.0202);
	(1/3.20152)/(1/0.973975);

Short Summary: The Ada implementation seems to attain about one third of the
speed of the Debian-supplied `unlz4` command. In absolute figures this is
still around 1000 MiB/s (for the bad cases) which can be expected to be enough
for plenty of use cases. Measuring with `dd` consistently yields smaller
throughputs compared to the `hyperfine` approach. This could be explained by the
fact that `dd` needs to perform a copy whereas `hyperfine` just discards the
extracted output right away.

## Performance vs. Safety -- Use of `pragma Suppress`

During optimization, some areas in the library that are performance crtical
turned out to be hugely slowed down by compiler-generated length and overflow
checks. In order to balance safety and performance, some checks are currently
disabled for the `Write_Output` procedure in the library. All other checks were
left in place.

If you need maximum safety even accepting strong performance penalties, feel
free to comment-out the following `pragma` directives in procedure
`Write_Output` in file `lz4ada.adb`:

~~~{.ada}
pragma Suppress(Length_Check);
pragma Suppress(Overflow_Check);
pragma Suppress(Index_Check);
pragma Suppress(Range_Check);
~~~

Rationale and Usage Recommendation
==================================

This library was created out of the need to process data from a Rust program
that encodes data in LZ4. Of course, if decoding is enough for your purposes,
you may as well use this library in new designs. I encourage you to consider
alternatively choosing a more widely supported compression format such as e.g.
LZMA. This way, better-maintained implementations and also the compression side
might be available in Ada.

Interesting Links to alternative Compression/Decompression implementations in
Ada:

 * <https://unzip-ada.sourceforge.io/> -
   implements various compressors and decompressors natively in Ada.
 * <https://packages.debian.org/bullseye/libgnatcoll-lzma2> --
   an LZMA bindings library that is available in Debian.

Changes
=======

Feel free to send patches with bugfixes or missing functionality directly to
<info@masysma.net>. Include a note to confirm that you are OK with these
patches being included under Expat license and add your preferred copyright line
to the patch or e-mail.

Please note that API breaks are only accepted if _very strong reasons_ exist to
motivate them.
