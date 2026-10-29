#!/usr/bin/env bash
# Runs the shared-cab scenarios against a live rider-app. usage: run.sh [driver|flush|rider|boarding|callchat|calloff|tripcheck|nearbybus|journeyowner|all] (default all)
# ENV (default local.env) picks the variables file; REDIS_CLI (default redis-cli) reaches the rider-app's Redis for the flush
# step (a local cluster needs REDIS_CLI="redis-cli -c -p 30001"). Local setup is documented in README.md, not done here:
# seed/*.sql applied, the apps' Redis config caches cleared (each app has its own key prefix) and the processes restarted,
# LTS geo config for the SC routes, and a cab that pings LTS. The stack must run the scheduler PRODUCER (producer-exe) next to
# rider-app-scheduler-exe: scheduled jobs sit in Scheduled_Jobs_Rider{n} until the producer moves them to the stream, so without
# it no tick, timeout or refund ever fires and bookings stay FINDING.
# A leftover LTS ride of the same driver on another route (a session that was never ended) blocks the attach: the cab's pings keep
# feeding the old route and the allocation stays FINDING. End it with LTS POST /internal/ride/{rideId}/end before re-running.
set -euo pipefail
cd "$(dirname "$0")"
env=${ENV:-local.env}
date=$(TZ=Asia/Kolkata date +%F)
var() { sed -n "s/^$1=//p" "$env"; }
session_file=$(mktemp)
trap 'rm -f "$session_file"' EXIT
h() { hurl --test --variables-file "$env" --variable date="$date" "$@"; }
# A failed flow leaves its booking live (tickets ACTIVE or INPROGRESS) holding seats on the cab, which starves the next flow. With
# CLEAN_DB_URL set (a LOCAL rider-app Postgres only: localhost / 127.0.0.1 is enforced) every flow starts by ending them through the
# rider app: cancel; "I got down" once the ride has started; beside a cab waiting at the stop, walk to the drop point (a status poll
# records the fix) and cancel. frfs_ticket_booking is on KV, so raw SQL changes Postgres only and the app still sees the booking live:
# it is the last resort, and says so. The list comes from Postgres, so a booking the drainer has not written yet is missed. Only the
# hurl rider's bookings are touched: its id comes from the login (or CLEAN_RIDER_ID when TOKEN is given).
clean() {
  [ -n "${CLEAN_DB_URL:-}" ] || return 0
  case $CLEAN_DB_URL in
    *@localhost[:/]* | *@127.0.0.1[:/]* | *//localhost[:/]* | *//127.0.0.1[:/]*) ;;
    *) echo "CLEAN_DB_URL must point at a local database"; exit 2 ;;
  esac
  local rider tok base booking journey leg
  rider=$(rider_id)
  [ -n "$rider" ] || { echo "no hurl rider id (login failed; with TOKEN set, set CLEAN_RIDER_ID): not cleaning"; exit 2; }
  tok=$(token)
  base=$(var base_url)
  ended() { case $(curl -s -o /dev/null -w '%{http_code}' -X POST -H "token: $tok" "$base$1") in 2??) ;; *) return 1 ;; esac; }
  walked_away() {
    curl -s -o /dev/null -X POST -H "token: $tok" -H 'Content-Type: application/json' "$base/v2/multimodal/$1/rider/location" \
      -d "{\"latLong\": {\"lat\": $(var to_lat), \"lon\": $(var to_lon)}, \"currTime\": \"$(date -u +%Y-%m-%dT%H:%M:%SZ)\"}"
    curl -s -o /dev/null -H "token: $tok" "$base/v2/multimodal/journey/$1/status"
  }
  while IFS='|' read -r booking journey leg; do
    [ -n "$journey" ] && ended "/v2/multimodal/$journey/order/$leg/cancel" && continue
    [ -n "$journey" ] && ended "/v2/multimodal/$journey/order/$leg/subleg/1/setStatus/Completed" && continue
    [ -n "$journey" ] && walked_away "$journey" && ended "/v2/multimodal/$journey/order/$leg/cancel" && continue
    echo "clean: the app would not end booking $booking; cancelled in Postgres only (KV still has it live)"
    psql "$CLEAN_DB_URL" -q -At -v booking="$booking" <<'SQL'
UPDATE atlas_app.frfs_ticket SET status = 'CANCELLED' WHERE status IN ('ACTIVE', 'INPROGRESS') AND frfs_ticket_booking_id = :'booking';
UPDATE atlas_app.frfs_ticket_booking SET status = 'CANCELLED' WHERE status = 'CONFIRMED' AND id = :'booking';
SQL
  done < <(psql "$CLEAN_DB_URL" -q -At -v rider="$rider" <<'SQL'
SELECT b.id, coalesce(m.journey_id, jl.journey_id), coalesce(m.sequence_number, jl.sequence_number)
  FROM atlas_app.frfs_ticket_booking b
  LEFT JOIN atlas_app.journey_leg jl ON jl.leg_id = b.search_id
  LEFT JOIN atlas_app.journey_leg_mapping m ON m.journey_leg_id = jl.id AND m.is_deleted IS NOT TRUE
 WHERE b.service_tier_type = 'SHARED_CAB' AND b.status = 'CONFIRMED' AND b.rider_id = :'rider'
   AND EXISTS (SELECT 1 FROM atlas_app.frfs_ticket t WHERE t.frfs_ticket_booking_id = b.id AND t.status IN ('ACTIVE', 'INPROGRESS'));
SQL
)
}

driver() { clean; h driver-flow.hurl; }
flush() {
  clean
  h flush-before.hurl
  # one DEL per key: the keys hash to different cluster slots
  ${REDIS_CLI:-redis-cli} DEL "sharedcab:session:$(var plate)" >/dev/null
  ${REDIS_CLI:-redis-cli} DEL "sharedcab:route:$(var route_a)" >/dev/null
  h flush-after.hurl
}
# one login per run.sh invocation (/v2/auth is rate limited): the verify response is cached in a file
session() { [ -s "$session_file" ] || hurl --variables-file "$env" login.hurl >"$session_file"; cat "$session_file"; }
token() { echo "${TOKEN:-$(session | jq -r .token)}"; }
rider_id() { echo "${CLEAN_RIDER_ID:-$(session 2>/dev/null | jq -r '.person.id // empty')}"; }
rider() { clean; h --variable token="$(token)" rider-flow.hurl; }
boarding() { clean; h --variable token="$(token)" boarding-flow.hurl; }
quantity() { clean; h --variable token="$(token)" quantity-flow.hurl; }
callchat() { clean; h --variable token="$(token)" call-chat-flow.hurl; }
nearbybus() { h --variable token="$(token)" nearby-bus.hurl; }
standalone() { clean; h --variable token="$(token)" standalone-flow.hurl; }
# A second rider (RIDER_B_MOBILE, default 9000000032) must be refused on the hurl rider's journey.
journeyowner() {
  clean
  tok_b=$(hurl --variables-file "$env" --variable mobileNumber="${RIDER_B_MOBILE:-9000000032}" --variable deviceToken=hurl-shared-cab-b login.hurl | jq -r .token)
  h --variable token="$(token)" --variable token_b="$tok_b" journey-owner.hurl
}
# SUSPEND_CMD / RESUME_CMD flip rider_config.shared_cab_enabled and refresh the rider-app and scheduler config caches (they cache for
# an hour, so locally that is the UPDATE plus a restart; see README). The flow needs shared_cab_finding_timeout_sec = 25 for the city.
suspended() {
  : "${SUSPEND_CMD:?set SUSPEND_CMD (see README)}" "${RESUME_CMD:?set RESUME_CMD (see README)}"
  clean
  tok=$(token)
  cap() { jq -r --arg n "$1" '[.entries[].captures[]? | select(.name == $n)][-1].value' <<<"$out"; }
  out=$(hurl --json --variables-file "$env" --variable date="$date" --variable token="$tok" suspended-city-before.hurl)
  vars=(--variable token="$tok" --variable journeyId="$(cap journeyId)" --variable legOrder="$(cap legOrder)" --variable journeyId2="$(cap journeyId2)" --variable legOrder2="$(cap legOrder2)" --variable categoryId2="$(cap categoryId2)")
  bash -c "$SUSPEND_CMD"
  trap 'bash -c "$RESUME_CMD"' EXIT
  h "${vars[@]}" suspended-city-after.hurl
  bash -c "$RESUME_CMD"; trap - EXIT
}

# CALL_OFF_CMD / CALL_ON_CMD flip rider_config.shared_cab_call_enabled and shared_cab_chat_enabled and refresh the config caches (as SUSPEND_CMD).
calloff() {
  : "${CALL_OFF_CMD:?set CALL_OFF_CMD (see README)}" "${CALL_ON_CMD:?set CALL_ON_CMD (see README)}"
  clean
  bash -c "$CALL_OFF_CMD"
  trap 'bash -c "$CALL_ON_CMD"' EXIT
  h --variable token="$(token)" call-chat-off-flow.hurl
  bash -c "$CALL_ON_CMD"; trap - EXIT
}

# A rider boarded on one trip must not reach the cab running another: tripcheck points the boarded booking at a different trip (SQL, so
# CLEAN_DB_URL must name the local database) and expects call and chat closed. The SQL edit only lands with frfs_ticket_booking off KV,
# which the local config no longer does (prod has it on KV): set OFF_KV=1 once it is off, otherwise the flow is skipped.
tripcheck() {
  [ "${OFF_KV:-}" = 1 ] || { echo "tripcheck skipped: needs frfs_ticket_booking off KV (set OFF_KV=1 once it is)"; return 0; }
  : "${CLEAN_DB_URL:?set CLEAN_DB_URL (a local Postgres)}"
  clean
  tok=$(token)
  cap() { jq -r --arg n "$1" '[.entries[].captures[]? | select(.name == $n)][-1].value' <<<"$out"; }
  out=$(hurl --json --variables-file "$env" --variable date="$date" --variable token="$tok" call-chat-trip-before.hurl)
  booking=$(cap bookingId)
  psql "$CLEAN_DB_URL" -q -At -v booking="$booking" <<'SQL'
UPDATE atlas_app.frfs_ticket_booking b SET vehicle_trip_id = (SELECT t.id FROM atlas_app.vehicle_trip t WHERE t.id <> b.vehicle_trip_id ORDER BY t.started_at LIMIT 1) WHERE b.id = :'booking';
SQL
  h --variable token="$tok" --variable journeyId="$(cap journeyId)" --variable legOrder="$(cap legOrder)" --variable bookingId="$booking" call-chat-trip-after.hurl
}

case ${1:-all} in
  clean) clean ;;
  driver) driver ;;
  flush) flush ;;
  rider) rider ;;
  boarding) boarding ;;
  standalone) standalone ;;
  callchat) callchat ;;
  nearbybus) nearbybus ;;
  tripcheck) tripcheck ;;
  calloff) calloff ;;
  suspended) suspended ;;
  quantity) quantity ;;
  journeyowner) journeyowner ;;
  all) driver; flush; rider; boarding; standalone; quantity ;;
  *) echo "usage: $0 [clean|driver|flush|rider|boarding|standalone|quantity|journeyowner|suspended|all]"; exit 2 ;;
esac
