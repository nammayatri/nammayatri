#!/usr/bin/env bash
# Shared by the fleet-scope shell checks (fleet-scope-extras.sh, fleet-scope-review.sh): the local endpoints,
# small helpers, the mode switch, and the cleanup that removes the fleets, operators and persons a run made.
# Source it; do not run it.
set -uo pipefail
# macOS's bash 3.2 splits a JSON body written inside "$( ... )" at its comma: two broken requests are sent, and a
# check comparing what comes back can pass without having tested anything. These checks need a newer bash.
if [ "${BASH_VERSINFO[0]}" -lt 4 ]; then
  echo "These checks need bash 4 or newer (this is $BASH_VERSION). For example: nix develop .#backend --command bash <script>" >&2
  exit 2
fi
export PGPORT=${PGPORT:-5434} # the dev stack's primary Postgres

DRIVER=http://localhost:8016/direct-dashboard/NAMMA_YATRI_PARTNER/Bangalore
DASH=http://localhost:8018
MOCK=http://localhost:8080
ADMIN_TOKEN=${ADMIN_TOKEN:-local-admin-token-bangalore-namma-yatri}
MERCHANT=NAMMA_YATRI_PARTNER
REDIS_PORT=${REDIS_PORT:-30001}
REDIS_PREFIX=${REDIS_PREFIX:-dynamic-offer-driver-app:} # the app's Redis key prefix
OTP=${OTP:-7891}

PASS=0
FAIL=0
SINCE=$(psql -d atlas_dev -tAc "select now()" 2>/dev/null)

ok() { PASS=$((PASS + 1)); echo "  ok   $1"; }
bad() { FAIL=$((FAIL + 1)); echo "  FAIL $1"; }
check() {
  if [ "$#" != 3 ]; then bad "$1 (mistake in the test: check was given $# arguments, not 3)"
  elif [ "$2" = "$3" ]; then ok "$1"
  else bad "$1 (got '$2', wanted '$3')"; fi
}
json() { python3 -c "import sys,json
try:
  d=json.load(sys.stdin)
  print($1)
except Exception: print('')"; }

mobile() { echo "9$(python3 -c 'import random;print(random.randint(100000000,999999999))')"; }

login() { login_phone "$(mobile)"; }

login_phone() { # prints a session token for the fleet owner with this phone, creating it on first use
  local m=$1 token attempt
  for attempt in 1 2 3; do # the code is stored asynchronously, so a verify straight after can miss it
    curl -s -m 20 -X POST "$DRIVER/fleet/v2/login/otp" -H 'Content-Type: application/json' \
      -d "{\"mobileNumber\":\"$m\",\"mobileCountryCode\":\"+91\"}" >/dev/null
    sleep 2
    token=$(curl -s -m 20 -X POST "$DRIVER/fleet/v2/verify/otp" -H 'Content-Type: application/json' \
      -d "{\"mobileNumber\":\"$m\",\"mobileCountryCode\":\"+91\",\"otp\":\"$OTP\"}" | json "d['authToken']")
    [ -n "$token" ] && break
    sleep 3
  done
  echo "$token"
}

person_of() { psql -d atlas_dev -tAc "select person_id from atlas_dashboard.registration_token where token='$1'"; }

wait_sql() { # wait_sql "<query>" "<wanted>" [seconds]: the app writes through Redis and a drainer copies to Postgres
  local got="" i
  for i in $(seq 1 "${3:-30}"); do
    got=$(psql -d atlas_dev -tAc "$1")
    [ "$got" = "$2" ] && break
    sleep 1
  done
  echo "$got"
}

status() { curl -s -m 20 -o /dev/null -w '%{http_code}' "$@"; }

redis() { redis-cli -c -p "$REDIS_PORT" "$@" 2>/dev/null; }

selection_of() { # the fleets this token has selected, sorted, comma-separated; empty when none is stored
  psql -d atlas_dev -tAc "select coalesce((select string_agg(f, ',' order by f) from unnest(selected_fleet_ids) f), '') from atlas_dashboard.registration_token where token='$1'" 2>/dev/null
}

set_mode() {
  psql -d atlas_dev -tAc "update atlas_dashboard.merchant set fleet_scope_mode='$1' where short_id='$MERCHANT'" >/dev/null
  curl -s -m 10 -X POST "$MOCK/mock/redis/del" -H 'Content-Type: application/json' \
    -d "{\"pattern\":\"*${MERCHANT}*\",\"target\":\"cluster\"}" >/dev/null
}

cleanup() {
  set_mode OFF
  psql -d atlas_dev -tA >/dev/null <<SQL
CREATE TEMP TABLE t_ids AS SELECT id FROM atlas_driver_offer_bpp.person WHERE role='FLEET_OWNER' AND created_at > '$SINCE'
  UNION SELECT fleet_owner_person_id FROM atlas_driver_offer_bpp.fleet_owner_information WHERE created_at > '$SINCE';
CREATE TEMP TABLE o_ids AS SELECT id FROM atlas_driver_offer_bpp.person WHERE role='OPERATOR' AND created_at > '$SINCE';
-- An inspection request left behind for a removed person makes the whole hub request list answer "person not found".
DELETE FROM atlas_driver_offer_bpp.operation_hub_requests WHERE driver_id IN (SELECT id FROM atlas_driver_offer_bpp.person WHERE role='DRIVER' AND created_at > '$SINCE')
  OR creator_id IN (SELECT id FROM t_ids UNION SELECT id FROM o_ids UNION SELECT id FROM atlas_driver_offer_bpp.person WHERE role='DRIVER' AND created_at > '$SINCE');
DELETE FROM atlas_driver_offer_bpp.vehicle_registration_certificate WHERE fleet_owner_id IN (SELECT id FROM t_ids);
DELETE FROM atlas_driver_offer_bpp.fleet_operator_association WHERE fleet_owner_id IN (SELECT id FROM t_ids) OR operator_id IN (SELECT id FROM o_ids);
DELETE FROM atlas_dashboard.person_role_assignment WHERE person_id IN (SELECT id FROM t_ids UNION SELECT id FROM o_ids) OR scope_id IN (SELECT id FROM t_ids);
DELETE FROM atlas_dashboard.registration_token WHERE person_id IN (SELECT id FROM t_ids UNION SELECT id FROM o_ids);
DELETE FROM atlas_dashboard.merchant_access WHERE person_id IN (SELECT id FROM t_ids UNION SELECT id FROM o_ids);
DELETE FROM atlas_dashboard.person WHERE id IN (SELECT id FROM t_ids UNION SELECT id FROM o_ids);
DELETE FROM atlas_driver_offer_bpp.registration_token WHERE entity_id IN (SELECT id FROM t_ids UNION SELECT id FROM o_ids);
DELETE FROM atlas_driver_offer_bpp.fleet_driver_association WHERE fleet_owner_id IN (SELECT id FROM t_ids);
DELETE FROM atlas_driver_offer_bpp.fleet_member_association WHERE fleet_owner_id IN (SELECT id FROM t_ids) OR fleet_member_id IN (SELECT id FROM t_ids);
DELETE FROM atlas_driver_offer_bpp.fleet_owner_information WHERE fleet_owner_person_id IN (SELECT id FROM t_ids);
DELETE FROM atlas_driver_offer_bpp.person WHERE id IN (SELECT id FROM t_ids UNION SELECT id FROM o_ids);
DELETE FROM atlas_driver_offer_bpp.person WHERE role='DRIVER' AND created_at > '$SINCE';
SQL
}
trap cleanup EXIT
