#!/usr/bin/env bash
# Shared-route (corporate shuttle) dev fixture, DATA half. LOCAL ONLY.
#
# Encrypts the fixture's phone numbers and the bus number with the local passetto, hashes them with the dev salt, and runs
# fixture-data.sql (next to this script) with the results as psql variables. Run after `, run-mobility-stack-dev` is up; the config
# half (dev/local-testing-data/shared-route-extra-setup.sql) is applied by the stack itself with the other local-testing-data files.
#
#   Backend/dev/scripts/shared-route-fixture/apply-fixture-data.sh
#
# This lives under dev/scripts, not dev/local-testing-data, on purpose: the context API applies every *.sql in local-testing-data
# verbatim at startup, and fixture-data.sql needs the variables this script supplies.
#
# Knobs: PASSETTO_URL (default http://127.0.0.1:8079), PSQL (default: psql against the dev DB on 5434), PGPASSWORD if your psql needs it.
# Idempotent: the SQL guards every row, so a re-run (which mints fresh ciphertexts) changes nothing already there.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PASSETTO_URL="${PASSETTO_URL:-http://127.0.0.1:8079}"
PSQL="${PSQL:-psql -h localhost -p 5434 -U atlas_superuser -d atlas_dev}"
# dev encHashSalt, Backend/dhall-configs/dev/secrets/*.dhall
SALT='How wonderful it is that nobody need wait a single moment before starting to improve the world'

# passetto wants the value as Haskell `show` of the text: S"..."
enc() {
  curl -sS --fail --max-time 5 "$PASSETTO_URL/encrypt" \
    -H 'Content-Type: application/json' \
    --data-raw "{\"value\":\"S\\\"$1\\\"\"}" \
    | python3 -c 'import json, sys; print(json.load(sys.stdin)["value"])'
}

# sha256(salt || plaintext), hex; the SQL wraps it in decode(..., 'hex')
hsh() {
  printf '%s%s' "$SALT" "$1" | shasum -a 256 | cut -d' ' -f1
}

if ! curl -sS --max-time 3 "$PASSETTO_URL/encrypt" -H 'Content-Type: application/json' --data-raw '{"value":"S\"ping\""}' >/dev/null; then
  echo "passetto is not answering at $PASSETTO_URL; start the dev stack first (, run-mobility-stack-dev)" >&2
  exit 1
fi

declare -a vars=()
add() { # name plaintext
  vars+=(-v "$1_enc=$(enc "$2")" -v "$1_hash=$(hsh "$2")")
}
add fo  9999900110
add d1  9999900111
add d2  9999900112
add bus TN01SH0001
add r1  9999900121
add r2  9999900122
add r3  9999900123

# shellcheck disable=SC2086
$PSQL -v ON_ERROR_STOP=1 "${vars[@]}" -f "$here/fixture-data.sql"
echo "shared-route fixture data applied (fleet owner 9999900110, drivers 9999900111/112, bus TN01SH0001, riders 9999900121/122/123; OTP 7891)"
