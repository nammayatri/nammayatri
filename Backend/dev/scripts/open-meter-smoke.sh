#!/usr/bin/env bash
# Smoke test for the rider-app open meter APIs (spec/API/OpenMeter.yaml) against a locally
# running rider-app (`, run-mobility-stack-dev` plus the rider-app server). Needs curl, jq and
# uuidgen. The rider's RiderConfig.allowMultipleOpenMeterRides must be off (the default).
#
# Usage:
#   Backend/dev/scripts/open-meter-smoke.sh                   # logs in with the dev OTP
#   TOKEN=<rider token> Backend/dev/scripts/open-meter-smoke.sh
#   PSQL='psql -h localhost -p 5434 -U postgres -d atlas_dev' Backend/dev/scripts/open-meter-smoke.sh
#
# Env: BASE_URL (default http://localhost:8013/v2), TOKEN, or MOBILE_NUMBER / MERCHANT_ID / OTP
# for the login (defaults from dev/rest-client/rider-app/auth.http).
# PSQL (optional): a psql command for the rider-app database (schema atlas_app). With it the script
# also reads rows in Postgres, seeds older trips there (as if the drainer had moved them), and runs
# the logout and account deletion checks with a supplied TOKEN: it puts the token row back after the
# logout and deletes a throwaway copy of the rider. Without PSQL those two checks would revoke the
# session or delete the account behind TOKEN, so they run only on the script's own login.

set -euo pipefail

BASE_URL="${BASE_URL:-http://localhost:8013/v2}"
MOBILE_NUMBER="${MOBILE_NUMBER:-7777777777}"
MERCHANT_ID="${MERCHANT_ID:-JUSPAY.CABS.BAP}"
OTP="${OTP:-7891}"

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
FAILURES=0

# req OUT_FILE METHOD PATH [JSON_BODY] -> prints the HTTP status; the response body goes to OUT_FILE.
req() {
  local out=$1 method=$2 path=$3 body=${4:-}
  local args=(-sS -o "$out" -w '%{http_code}' -X "$method" -H 'Content-Type: application/json' -H "token: ${TOKEN:-}")
  if [[ -n $body ]]; then args+=(--data "$body"); fi
  curl "${args[@]}" "$BASE_URL$path"
}

check() {
  local name=$1 expected=$2 actual=$3
  if [[ $expected == "$actual" ]]; then
    echo "PASS  $name"
  else
    echo "FAIL  $name: expected [$expected], got [$actual]"
    FAILURES=$((FAILURES + 1))
  fi
}

new_id() { uuidgen | tr '[:upper:]' '[:lower:]'; }
now() { date -u +%Y-%m-%dT%H:%M:%SZ; }
start_body() { jq -nc --arg id "$1" --arg t "$(now)" '{rideId: $id, startTime: $t, startLatLong: {lat: 12.9716, lon: 77.5946}}'; }

# Starts RIDE_ID into OUT_FILE, retrying once on the retryable 409 OPEN_METER_RIDE_BUSY as the app does.
start_with_retry() {
  local out=$1 ride_id=$2 code
  code=$(req "$out" POST /openMeter/ride/start "$(start_body "$ride_id")")
  if [[ $code == 409 && $(jq -r '.errorCode // empty' "$out") == OPEN_METER_RIDE_BUSY ]]; then
    sleep 1
    code=$(req "$out" POST /openMeter/ride/start "$(start_body "$ride_id")")
  fi
  echo "$code"
}

# sql QUERY [psql args]: runs QUERY through $PSQL and prints the bare result. Pass values with
# -v name=value and read them as :'name' in QUERY.
sql() {
  local query=$1
  shift
  printf '%s\n' "$query" | $PSQL -X -q -t -A -v ON_ERROR_STOP=1 "$@"
}

# Logs in with the dev OTP and sets TOKEN.
login() {
  req "$WORK/auth" POST /auth "$(jq -nc --arg m "$MOBILE_NUMBER" --arg mid "$MERCHANT_ID" '{mobileCountryCode: "+91", mobileNumber: $m, merchantId: $mid}')" >/dev/null
  local auth_id
  auth_id=$(jq -r '.authId' "$WORK/auth")
  req "$WORK/verify" POST "/auth/$auth_id/verify" "$(jq -nc --arg otp "$OTP" '{otp: $otp, deviceToken: ""}')" >/dev/null
  TOKEN=$(jq -r '.token' "$WORK/verify")
}

LOGIN_BY_SCRIPT=
if [[ -z ${TOKEN:-} ]]; then
  LOGIN_BY_SCRIPT=1
  login
fi

# The status enum's FromHttpApiData comes from mkHttpInstancesForEnum, which decodes the value as
# JSON: the query param is the quoted string "RUNNING" (%22RUNNING%22). A bare RUNNING gives 400.
RUNNING_RIDES="/openMeter/rides?status=%22RUNNING%22&limit=50"

# Rides left RUNNING by an earlier run would turn the first start into a 409.
code=$(req "$WORK/running" GET "$RUNNING_RIDES")
check "list of RUNNING rides gives 200" 200 "$code"
for leftover in $(jq -r '.[].id' "$WORK/running"); do
  req "$WORK/cancel" POST "/openMeter/ride/$leftover/cancel" >/dev/null
done

RIDE_ID=$(new_id)

start_with_retry "$WORK/start1" "$RIDE_ID" >"$WORK/start1.code" &
start_with_retry "$WORK/start2" "$RIDE_ID" >"$WORK/start2.code" &
wait
check "two concurrent starts with the same id both succeed" "200 200" "$(cat "$WORK/start1.code") $(cat "$WORK/start2.code")"
req "$WORK/list" GET "$RUNNING_RIDES" >/dev/null
check "two concurrent starts with the same id give one row" 1 "$(jq --arg id "$RIDE_ID" '[.[] | select(.id == $id)] | length' "$WORK/list")"
# The id is the primary key, so the list shows one row even if both starts created it. A second
# create would carry its own createdAt, so identical bodies show that one start returned the other's ride.
check "two concurrent starts with the same id return identical bodies" "$(cat "$WORK/start1")" "$(cat "$WORK/start2")"

code=$(req "$WORK/second" POST /openMeter/ride/start "$(start_body "$(new_id)")")
check "a second id gives 409" "409 OPEN_METER_RIDE_ALREADY_ACTIVE" "$code $(jq -r '.errorCode // empty' "$WORK/second")"

# The id column is varchar(36): a longer id would be accepted by KV and lost when the drainer's insert fails.
code=$(req "$WORK/not-uuid" POST /openMeter/ride/start "$(start_body "$(new_id)x")")
check "a start with a rideId that is not a UUID gives 400" "400 INVALID_REQUEST" "$code $(jq -r '.errorCode // empty' "$WORK/not-uuid")"

# "???gE" is (0, 0) -> (0, 0.001), about 111 m.
END_BODY=$(jq -nc --arg t "$(now)" '{endTime: $t, endLatLong: {lat: 12.9726, lon: 77.5956}, distanceMeters: 111, waitingTimeSeconds: 30, fare: 45.5, currency: "INR", encodedPolyline: "???gE", pointsCount: 2, estimatedGapMeters: 0, endReason: "USER"}')
code1=$(req "$WORK/end1" POST "/openMeter/ride/$RIDE_ID/end" "$END_BODY")
code2=$(req "$WORK/end2" POST "/openMeter/ride/$RIDE_ID/end" "$END_BODY")
check "end twice gives 200" "200 200" "$code1 $code2"
check "end twice gives identical bodies" "$(cat "$WORK/end1")" "$(cat "$WORK/end2")"

# Rides cannot be deleted: there is no DELETE route, and the ride is still there afterwards.
code=$(req "$WORK/delete-ended" DELETE "/openMeter/ride/$RIDE_ID")
check "a DELETE of a ride is not served" refused "$([[ $code == 4?? ]] && echo refused || echo "$code")"
code=$(req "$WORK/after-delete" GET "/openMeter/ride/$RIDE_ID")
check "the ride is still there after a DELETE" "200 ENDED" "$code $(jq -r '.status // empty' "$WORK/after-delete")"

# Starts and ends a new ride.
make_ended_ride() {
  local id
  id=$(new_id)
  start_with_retry "$WORK/made-start" "$id" >/dev/null
  req "$WORK/made-end" POST "/openMeter/ride/$id/end" "$END_BODY" >/dev/null
}

# Logout ends every session of the rider, so no phone can end a RUNNING ride any more: the backend
# AUTO_ENDs it at its last heartbeat. Left RUNNING, it would refuse the rider's next start with
# ALREADY_ACTIVE until it went stale (openMeterMaxIdleHours, 12 h). After the logout, PSQL puts the
# token row back, so the logout is tested alone; otherwise the script logs in again with the OTP,
# and that login ends the ride the same way.
if [[ -n ${PSQL:-} || -n $LOGIN_BY_SCRIPT ]]; then
  LOGOUT_RIDE_ID=$(new_id)
  code=$(start_with_retry "$WORK/logout-start" "$LOGOUT_RIDE_ID")
  check "a start before logout gives 200" 200 "$code"
  HEARTBEAT_AT=$(now)
  code=$(req "$WORK/logout-heartbeat" POST "/openMeter/ride/$LOGOUT_RIDE_ID/heartbeat" "$(jq -nc --arg t "$HEARTBEAT_AT" '{sampledAt: $t, distanceMeters: 250, waitingTimeSeconds: 10}')")
  check "a heartbeat before logout gives 200" 200 "$code"
  if [[ -n ${PSQL:-} ]]; then
    TOKEN_ROW=$(sql "SELECT row_to_json(t) FROM atlas_app.registration_token t WHERE token = :'token'" -v token="$TOKEN")
  fi
  code=$(req "$WORK/logout" POST /auth/logout)
  check "logout gives 200" 200 "$code"
  if [[ -n ${PSQL:-} ]]; then
    sql "INSERT INTO atlas_app.registration_token SELECT * FROM json_populate_record(NULL::atlas_app.registration_token, :'row'::json)" -v row="$TOKEN_ROW" >/dev/null
  else
    login
  fi
  code=$(req "$WORK/logout-ride" GET "/openMeter/ride/$LOGOUT_RIDE_ID")
  check "logout auto-ends the rider's RUNNING ride at its last heartbeat" "200 AUTO_ENDED $HEARTBEAT_AT" "$code $(jq -r '"\(.status) \(.endTime)"' "$WORK/logout-ride")"
  AFTER_LOGOUT_ID=$(new_id)
  code=$(start_with_retry "$WORK/after-logout" "$AFTER_LOGOUT_ID")
  check "a start after logout is not refused as ALREADY_ACTIVE" 200 "$code"
  req "$WORK/after-logout-cancel" POST "/openMeter/ride/$AFTER_LOGOUT_ID/cancel" >/dev/null
fi

# Trip history pages by offset: two pages of 20 must read as the first 40 rides in one page. With
# PSQL, 25 older ENDED rides first go straight to Postgres, as if the drainer had moved them, so the
# history spans KV and Postgres (the two rides made here are in KV). There, a table outside
# kv_configs.tablesForExactPaging repeats rides of the first page on the second and skips others
# (dev/ddl-migrations/rider-app/1587-open-meter-ride-exact-paging.sql).
if [[ -n ${PSQL:-} ]]; then
  PERSON_ID=$(sql "SELECT entity_id FROM atlas_app.registration_token WHERE token = :'token'" -v token="$TOKEN")
  sql "INSERT INTO atlas_app.open_meter_ride (id, person_id, status, start_time, server_started_at, end_time, distance_meters, estimated_gap_meters, points_count, waiting_time_seconds, created_at, updated_at)
       SELECT gen_random_uuid()::text, :'person', 'ENDED', t, t, t + interval '20 minutes', 1000, 0, 0, 0, t, t
       FROM (SELECT now() - make_interval(days => 30 + i) AS t FROM generate_series(1, 25) AS i) AS older" -v person="$PERSON_ID" >/dev/null
fi
make_ended_ride
make_ended_ride
req "$WORK/history" GET "/openMeter/rides?limit=50" >/dev/null
RIDES=$(jq 'length' "$WORK/history")
while ((RIDES < 22)); do
  make_ended_ride
  RIDES=$((RIDES + 1))
done
req "$WORK/first40" GET "/openMeter/rides?limit=40&offset=0" >/dev/null
req "$WORK/page1" GET "/openMeter/rides?limit=20&offset=0" >/dev/null
code=$(req "$WORK/page2" GET "/openMeter/rides?limit=20&offset=20")
check "page 2 of the trip history gives 200" 200 "$code"
check "page 2 repeats no ride of page 1" 0 "$(jq -n --slurpfile a "$WORK/page1" --slurpfile b "$WORK/page2" '[$a[0][].id] - ([$a[0][].id] - [$b[0][].id]) | length')"
check "pages of 20 by offset read as the first 40 rides" true "$(jq -n --slurpfile all "$WORK/first40" --slurpfile a "$WORK/page1" --slurpfile b "$WORK/page2" '[$all[0][].id] == [$a[0][].id, $b[0][].id]')"

# deletedPerson keeps open meter rides. The account deleted is a throwaway copy of the rider made in
# Postgres when PSQL is set, so a supplied TOKEN keeps its account; otherwise it is the script's own
# login. Another rider then starts a ride with the deleted account's ride id: TOKEN, or a new login for
# the same number, which is a new person. A ride id still stored under any rider is "not found" to
# everyone else, so that start is refused only if the deletion kept the row.
# (The new person's history is empty either way, so it proves nothing.)
if [[ -n ${PSQL:-} || -n $LOGIN_BY_SCRIPT ]]; then
  KEEP_TOKEN=$TOKEN
  if [[ -n ${PSQL:-} ]]; then
    DEL_PERSON_ID=$(new_id)
    TOKEN=$(new_id)
    sql "INSERT INTO atlas_app.person SELECT * FROM json_populate_record(NULL::atlas_app.person, (
           SELECT ((to_jsonb(p) - 'mobile_number_hash' - 'mobile_number_encrypted' - 'email_hash' - 'email_encrypted'
                    - 'device_token' - 'customer_referral_code' - 'operator_badge_token') || jsonb_build_object('id', :'person'))::json
           FROM atlas_app.person p WHERE p.id = (SELECT entity_id FROM atlas_app.registration_token WHERE token = :'keep')));
         INSERT INTO atlas_app.registration_token SELECT * FROM json_populate_record(NULL::atlas_app.registration_token, (
           SELECT (to_jsonb(t) || jsonb_build_object('id', :'token_id', 'token', :'token', 'entity_id', :'person'))::json
           FROM atlas_app.registration_token t WHERE t.token = :'keep'))" \
      -v person="$DEL_PERSON_ID" -v token="$TOKEN" -v token_id="$(new_id)" -v keep="$KEEP_TOKEN" >/dev/null
  fi
  DEL_RIDE_ID=$(new_id)
  code=$(start_with_retry "$WORK/del-start" "$DEL_RIDE_ID")
  check "a start before account deletion gives 200" 200 "$code"
  code=$(req "$WORK/del-account" POST /deleted/person '{"reasonToDelete": "open meter smoke test"}')
  check "deleting the account gives 200" 200 "$code"
  if [[ -n ${PSQL:-} ]]; then
    TOKEN=$KEEP_TOKEN
  else
    login
  fi
  code=$(start_with_retry "$WORK/del-reuse" "$DEL_RIDE_ID")
  check "deletedPerson keeps open meter rides (their ids stay taken)" "400 OPEN_METER_RIDE_NOT_FOUND" "$code $(jq -r '.errorCode // .id' "$WORK/del-reuse")"
fi

if ((FAILURES > 0)); then
  echo "$FAILURES check(s) failed"
  exit 1
fi
echo "All open meter smoke checks passed"
