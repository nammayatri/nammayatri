#!/usr/bin/env bash
# Every test in this folder, one after another; a summary; exit 1 if any failed.
#
#     bash tests/run-all.sh            (npm ci in tests/ first, once)
#
# What CI runs (algeria: node tests). It globs rather than lists, so a test
# added here runs in CI without anyone remembering to add it -- until phase 5
# (2026-10-06) CI named four tests and seven others never ran there.
set -uo pipefail
cd "$(dirname "$0")"

if [ ! -d node_modules/@electric-sql/pglite ]; then
  echo "tests/node_modules is missing -- run: (cd tests && npm ci)" >&2
  exit 2
fi

pass=0
failed=()
run() {
  local name="$1"; shift
  local started=$SECONDS out
  if out="$("$@" 2>&1)"; then
    pass=$((pass + 1))
    printf '  ok    %-40s %3ss\n' "$name" "$((SECONDS - started))"
  else
    failed+=("$name")
    printf '  FAIL  %-40s %3ss\n' "$name" "$((SECONDS - started))"
    printf "%s\n" "$out" | tail -60 | sed 's/^/        /'
  fi
}

echo "node $(node -v)"
for t in *.test.js; do run "$t" node "$t"; done
for t in *.test.py; do run "$t" python3 "$t"; done
for t in *.test.sh; do run "$t" bash "$t"; done

echo
if [ ${#failed[@]} -eq 0 ]; then
  echo "all $pass test files passed"
else
  echo "${#failed[@]} failed: ${failed[*]}  ($pass passed)"
  exit 1
fi
