#!/bin/sh -eu

# To compare with reference implementation
# LD_LIBRARY_PATH=lib tool_unlz4ada/unlz4ada < ~/wd/silesia.tar.lz4 | dd bs=1M of=/dev/null
# Here, my implementation achieves around 200 MiB/s vs.
# Debian's unlz4 has 830 MiB/s

if ! [ -f /tmp/zeroes.bin ]; then
	dd if=/dev/zero of=/tmp/zeroes.bin bs=1M count=2048
fi

if ! [ -f /tmp/random.bin ]; then
	dd if=/dev/urandom of=/tmp/random.bin bs=1M count=2048
fi

if ! [ -f /tmp/text.txt ]; then
	base64 < /dev/urandom | dd of=/tmp/text.txt bs=1M count=2048 iflag=fullblock
fi

for i in /tmp/zeroes.bin /tmp/random.bin /tmp/text.txt; do
	woext="${i%.*}"
	if ! [ -f "$woext.lz4" ]; then
		lz4 < "$i" > "$woext.lz4"
	fi
done

export LD_LIBRARY_PATH="$(dirname "$0")/lib"
bin="$(dirname "$0")/tool_unlz4ada/unlz4ada"
ulimit -s 60000

echo
echo benchmark zeroes
"$bin" < /tmp/zeroes.lz4 | dd of=/dev/null bs=1M
echo
echo benchmark random
"$bin" < /tmp/random.lz4 | dd of=/dev/null bs=1M
echo
echo benchmark text
"$bin" < /tmp/text.lz4 | dd of=/dev/null bs=1M
echo
echo benchmark reference zeroes
unlz4 < /tmp/zeroes.lz4 | dd of=/dev/null bs=1M
