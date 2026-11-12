#!/usr/bin/env bash
# The photos of a proposed place, live, after the day-4 release (« Lieux
# proposés », 2026-11-12). Read-only, and needs no account.
#
# Runs ON the server. From the laptop, in one connection (ufw limits SSH to
# five in thirty seconds):
#
#   scp investigations/probe-place-photos.sh ny:/tmp/
#   ssh ny 'bash /tmp/probe-place-photos.sh' | tee ~/probe-place-photos.log
#
# Three parts, each a list of `ok` / `BAD` lines:
#   db    the table the release made: its rules, the cascade, no photo yet
#   shim  maps-shim on 127.0.0.1:8030 -- the photo routes exist and refuse
#         a caller with no token, a GET, a photo 3
#   edge  the public address, as a phone reaches it: an image-sized body
#         reaches the shim (401, not 413), a body over 512 KB is refused by
#         nginx (413), and /places/ itself still refuses over 4 KB
#
# What it does NOT prove, and why: a photo from somebody else's account, and
# one for a place already decided, both need a signed-in token, and no test
# account exists on the live server (2026-10-01). Both are proven against the
# real table in tests/suggested-places-routes.test.js, section 9; live, they
# are part of the end-of-day test on a phone.
#
#   PROBE_PARTS="shim edge" bash probe-place-photos.sh   # some parts only
#   EDGE=https://… SHIM=http://… bash probe-place-photos.sh
set -uo pipefail

EDGE=${EDGE:-https://api.169-58-139-65.sslip.io}
SHIM=${SHIM:-http://127.0.0.1:8030}
PARTS=${PROBE_PARTS:-db shim edge}

pass=0
fail=0
check() { # check <got> <want> <what>
  if [ "$1" = "$2" ]; then
    pass=$((pass + 1)); echo "  ok   $3"
  else
    fail=$((fail + 1)); echo "  BAD  $3   -- got: $1, want: $2"
  fi
}
# The status of one request; 000 when it never got an answer.
status() { curl -s -o /dev/null -w '%{http_code}' --max-time 20 "$@"; }

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
# Bodies of a given size. Their bytes do not matter here: every request below
# is refused before the shim looks at them (no token, or too large).
head -c 307200 /dev/zero >"$tmp/300k"
head -c 614400 /dev/zero >"$tmp/600k"
head -c 5120 /dev/zero >"$tmp/5k"
head -c 3072 /dev/zero >"$tmp/3k"

echo "probe-place-photos  $(date -u +%FT%TZ)  parts: $PARTS"

if [[ " $PARTS " == *" db "* ]]; then
  echo; echo "db -- movin.suggested_place_photo"
  # Words and names only, never a boolean: psql prints t / f. And NOT NULL
  # left out (contype 'n'): Postgres 18 lists those as constraints too.
  q() { docker exec ny-postgres psql -U postgres -d atlas_dev -X -A -t -q -c "$1" 2>&1; }
  check "$(q "SELECT CASE WHEN to_regclass('movin.suggested_place_photo') IS NULL THEN 'no' ELSE 'yes' END")" "yes" \
    "the table exists"
  check "$(q "SELECT string_agg(conname, ',' ORDER BY conname) FROM pg_constraint
               WHERE conrelid = 'movin.suggested_place_photo'::regclass AND contype <> 'n'")" \
    "suggested_place_photo_content_type_check,suggested_place_photo_is_its_type,suggested_place_photo_n_check,suggested_place_photo_pkey,suggested_place_photo_size,suggested_place_photo_suggested_place_id_fkey" \
    "its six rules: the key, 1 or 2, JPEG or PNG, the bytes are their type, 512 KB, its place"
  check "$(q "SELECT confdeltype FROM pg_constraint WHERE conname = 'suggested_place_photo_suggested_place_id_fkey'")" "c" \
    "deleted with its place (ON DELETE CASCADE)"
  check "$(q "SELECT (regexp_match(pg_get_constraintdef(oid), '<= (\d+)'))[1] FROM pg_constraint
               WHERE conname = 'suggested_place_photo_size'")" "524288" "512 KB at most (524288 bytes)"
  check "$(q "SELECT attstorage FROM pg_attribute WHERE attrelid = 'movin.suggested_place_photo'::regclass AND attname = 'bytes'")" \
    "e" "the bytes stored out of line, not compressed again"
  echo "  --   photos stored so far: $(q 'SELECT count(*) FROM movin.suggested_place_photo') (0 until an app sends one)"
fi

if [[ " $PARTS " == *" shim "* ]]; then
  echo; echo "shim -- $SHIM"
  check "$(status -X PUT --data-binary @"$tmp/3k" "$SHIM/places/photos/u1/1")" 401 "PUT a photo with no token: 401"
  check "$(status -X PUT -H 'token: not-a-real-token' --data-binary @"$tmp/3k" "$SHIM/places/photos/u1/1")" 401 \
    "PUT with a made-up token: 401 (the backend does not know it)"
  check "$(status -X DELETE "$SHIM/places/photos/u1/2")" 401 "DELETE with no token: 401"
  check "$(status "$SHIM/places/photos/u1/1")" 405 "GET a photo: 405 (photos are for the office, through the console)"
  check "$(status -X PUT "$SHIM/places/photos/u1/3")" 404 "photo 3: 404 (two at most)"
  check "$(status "$SHIM/places/mine")" 401 "/places/mine still needs a token: 401"
fi

if [[ " $PARTS " == *" edge "* ]]; then
  echo; echo "edge -- $EDGE"
  check "$(status -X PUT -H 'content-type: image/jpeg' --data-binary @"$tmp/300k" "$EDGE/places/photos/u1/1")" 401 \
    "300 KB, the size of a phone's photo: through the edge to the shim (401 -- no token), not refused as too large"
  check "$(status -X PUT -H 'content-type: image/jpeg' --data-binary @"$tmp/600k" "$EDGE/places/photos/u1/1")" 413 \
    "600 KB: refused by nginx (413), before it reaches the shim"
  check "$(status -X POST -H 'content-type: application/json' --data-binary @"$tmp/5k" "$EDGE/places/suggest")" 413 \
    "/places/suggest with 5 KB: still 413 -- the 4k limit kept for everything but photos"
  check "$(status -X PUT --data-binary @"$tmp/3k" "$EDGE/places/u1/photo/1")" 404 \
    "the plan's first spelling, /places/u1/photo/1: not a route (404)"
fi

echo
if [ "$fail" = 0 ]; then echo "ALL PASSED ($pass)"; else echo "FAILED ($fail of $((pass + fail)))"; exit 1; fi
