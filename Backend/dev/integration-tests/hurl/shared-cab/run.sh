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
# A failed flow leaves its CONFIRMED booking (tickets ACTIVE) holding seats on the cab, which starves the next flow. With
# CLEAN_DB_URL set (a LOCAL rider-app Postgres only: localhost / 127.0.0.1 is enforced) every flow starts by cancelling them.
# Raw SQL cancels bypass the app (status changes, updated_at and the Redis hash do not), so only the hurl rider's bookings are
# touched: its id comes from the login (or CLEAN_RIDER_ID when TOKEN is given). Another rider's live booking is never cancelled.
clean() {
  [ -n "${CLEAN_DB_URL:-}" ] || return 0
  case $CLEAN_DB_URL in
    *@localhost[:/]* | *@127.0.0.1[:/]* | *//localhost[:/]* | *//127.0.0.1[:/]*) ;;
    *) echo "CLEAN_DB_URL must point at a local database"; exit 2 ;;
  esac
  local rider
  rider=$(rider_id)
  [ -n "$rider" ] || { echo "no hurl rider id (login failed; with TOKEN set, set CLEAN_RIDER_ID): not cleaning"; exit 2; }
  psql "$CLEAN_DB_URL" -q -At -v rider="$rider" <<'SQL'
UPDATE atlas_app.frfs_ticket SET status = 'CANCELLED'
 WHERE status IN ('ACTIVE', 'INPROGRESS') AND frfs_ticket_booking_id IN
   (SELECT id FROM atlas_app.frfs_ticket_booking WHERE service_tier_type = 'SHARED_CAB' AND rider_id = :'rider');
UPDATE atlas_app.frfs_ticket_booking SET status = 'CANCELLED'
 WHERE service_tier_type = 'SHARED_CAB' AND status = 'CONFIRMED' AND rider_id = :'rider';
SQL
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
# CLEAN_DB_URL must name the local database) and expects call and chat closed. Needs the frfs_ticket_booking table off KV (seed/05-local-config.sql).
tripcheck() {
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
