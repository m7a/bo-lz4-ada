#!/bin/sh -eu

# To compare with reference implementation
# LD_LIBRARY_PATH=lib tool_unlz4ada/unlz4ada < ~/wd/silesia.tar.lz4 | dd bs=1M of=/dev/null

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
#bin="$(dirname "$0")/tool_unlz4ada_singleframe/unlz4ada_singleframe"
ulimit -s 60000

if [ $# = 1 ] && [ "$1" = "-h" ]; then
	run() {
		hyperfine -m 50 "$1 < $2"
	}
elif [ $# = 1 ] && [ "$1" = "-p" ]; then
	run() {
		"$1" < "$2" | pv > /dev/null
	}
else
	run() {
		"$1" < "$2" | dd of=/dev/null bs=1M
	}
fi

echo
echo benchmark zeroes
run "$bin" /tmp/zeroes.lz4
echo
echo benchmark reference zeroes
run unlz4 /tmp/zeroes.lz4
echo
echo benchmark random
run "$bin" /tmp/random.lz4
echo
echo benchmark reference random
run unlz4 /tmp/random.lz4
echo
echo benchmark text
run "$bin" /tmp/text.lz4
echo
echo benchmark reference text
run unlz4 /tmp/text.lz4
