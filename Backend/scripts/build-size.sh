#!/usr/bin/env bash

BUILD="${1:-../dist-newstyle}"

if [ ! -d "$BUILD" ]; then
  echo "Directory $BUILD not found"
  exit 1
fi

echo "=== Total size ==="
du -sh "$BUILD"
echo

echo "=== Size by extension (top 20) ==="
find "$BUILD" -type f -name '*.*' 2>/dev/null | \
  sed 's/.*\.\([^./]*\)$/\1/' | \
  sort | uniq -c | sort -rn | head -20 | \
  while read count ext; do
    size=$(find "$BUILD" -type f -name "*.$ext" -exec du -ch {} + 2>/dev/null | tail -1 | cut -f1)
    printf "  %-12s %6d files   %s\n" ".$ext" "$count" "$size"
  done

echo
echo "=== Size of files with NO extension (likely executables) ==="
find "$BUILD" -type f ! -name '*.*' -exec du -ch {} + 2>/dev/null | tail -1
echo

echo "=== Largest individual files (top 15) ==="
find "$BUILD" -type f -exec du -h {} + 2>/dev/null | sort -rh | head -15

echo
echo "=== Size by top-level subdir ==="
du -sh "$BUILD"/* 2>/dev/null | sort -rh | head -10