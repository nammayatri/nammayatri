#!/usr/bin/env bash
# Counts the place index's points of interest per type and per country, with
# the type mapping that ships (« Lieux proposés », day 1, 2026-11-09).
#
# Runs ON the server, where ny-postgres is. From the laptop, in one connection
# each (ufw limits SSH to five in thirty seconds):
#
#   scp investigations/probe-place-types.sh investigations/probe-place-types.sql \
#       stack/db/suggested-places.sql ny:/tmp/
#   ssh ny 'bash /tmp/probe-place-types.sh' | tee ~/probe-place-types.log
#
# Why a wrapper and not just the .sql: the counts need movin.place_type_of,
# which no release has installed yet. So this copies the function's block out
# of suggested-places.sql (between its `>>> place_type_of` / `<<<` markers)
# into pg_temp -- this session's own schema, gone when psql exits -- and the
# probe calls pg_temp.place_type_of. The mapping counted is the mapping that
# ships, and movin is never touched. Read-only otherwise: see the .sql header.
#
#   PROBE_PRINT=1 bash probe-place-types.sh   # print the SQL, run nothing
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
die() { echo "probe-place-types: $*" >&2; exit 1; }

# Beside this script (copied with it), else in a checkout of the repository.
fn_file=""
for f in "$here/suggested-places.sql" "$here/../stack/db/suggested-places.sql"; do
  [ -f "$f" ] && { fn_file=$f; break; }
done
[ -n "$fn_file" ] || die "suggested-places.sql not found beside $0 -- copy it with the probe"
[ -f "$here/probe-place-types.sql" ] || die "probe-place-types.sql not found beside $0"

block=$(awk '/^-- >>> place_type_of/{on=1; next} /^-- <<< place_type_of/{on=0} on' "$fn_file")
# A rename that matches nothing is silent, and would leave the probe calling a
# function that does not exist -- or worse, CREATE the real one in movin.
n=$(grep -c 'CREATE OR REPLACE FUNCTION movin.place_type_of(' <<<"$block" || true)
[ "$n" = 1 ] || die "expected the function once between the markers in $fn_file, found $n"
block=${block//movin.place_type_of/pg_temp.place_type_of}
grep -q 'movin\.' <<<"$block" && die "the copied block still names movin -- refusing to run it"

compose() {
  echo '\set ON_ERROR_STOP on'
  printf '%s\n' "$block"
  cat "$here/probe-place-types.sql"
}

if [ "${PROBE_PRINT:-0}" = 1 ]; then compose; exit 0; fi

echo "probe-place-types  $(date -u +%FT%TZ)  mapping from $fn_file ($(sha256sum "$fn_file" | cut -c1-12))"
compose | docker exec -i ny-postgres psql -U postgres -d atlas_dev -X -q
