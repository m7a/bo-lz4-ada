#!/bin/sh -eu
# LZ4 Ada Test Script 1.0.0 (c) 2022 Ma_Sys.ma <info@masysma.net>

ulimit -s 60000 # need enough stack size!

root="$(cd "$(dirname "$0")" && pwd)"
tmpd="$(mktemp -d)"
arc=0
uut="$root/tool_unlz4ada/unlz4ada"
#uut="$root/tool_unlz4ada_singleframe/unlz4ada_singleframe"

printf "Using temporary directory %s\n" "$tmpd"
for i in test_vectors_lz4/*.lz4; do
	basename="$(basename "$i")"
	namepart="${basename%.*}"
	uncompressed="test_vectors_lz4/$namepart.bin"
	num_bytes="$(wc -c < "$uncompressed")"
	decoded="$tmpd/$namepart.uut"
	printf "Test %-14s ... " "$namepart"
	rv=0
	LD_LIBRARY_PATH="$root/lib" time -p "$uut" < "$i" > "$decoded" \
					2> "$tmpd/$namepart.tim" || rv=$?
	# more robust than `cmp` which chokes on empty files
	chkgood="$(sha256sum < "$uncompressed")"
	chkchck="$(sha256sum < "$decoded")"
	if [ "$rv" = 0 ] && [ "$chkgood" = "$chkchck" ]; then
		real="$(head -n 1 "$tmpd/$namepart.tim")"
		realf="${real##* }"
		# 0.0001 to avoid div by zero if `time` command does not
		# capture any runtime
		mbs="$(printf "%s / (%s + 0.0001) / 1024 / 1024\n" \
						"$num_bytes" "$realf" | bc -l)"
		printf '[ OK ] (%3.3f MiB/s)\n' "$mbs"
	else
		printf '[FAIL] rv=%s (%s)\n' "$rv" \
			"$(head -c 40 "$tmpd/$namepart.tim" | tr '\n' ' ')"
		#printf 'good: %s != found: %s\n' "$chkgood" "$chkchck" # debug!
		arc=1
	fi
done

if [ "$arc" = 0 ]; then
	rm -r "$tmpd"
fi
exit "$arc"
