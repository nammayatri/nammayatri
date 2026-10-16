#!/usr/bin/env bash
# Runs the shared-cab scenarios against a live rider-app. usage: run.sh [driver|flush|rider|boarding|all] (default all)
# ENV (default local.env) picks the variables file; REDIS_CLI (default redis-cli) reaches the rider-app's Redis for the flush
# step (a local cluster needs REDIS_CLI="redis-cli -c -p 30001"). Local setup is documented in README.md, not done here:
# seed/*.sql applied, the apps' Redis config caches cleared (each app has its own key prefix) and the processes restarted,
# LTS geo config for the SC routes, and a cab that pings LTS. The stack must run the scheduler PRODUCER (producer-exe) next to
# rider-app-scheduler-exe: scheduled jobs sit in Scheduled_Jobs_Rider{n} until the producer moves them to the stream, so without
# it no tick, timeout or refund ever fires and bookings stay FINDING.
set -euo pipefail
cd "$(dirname "$0")"
env=${ENV:-local.env}
date=$(TZ=Asia/Kolkata date +%F)
var() { sed -n "s/^$1=//p" "$env"; }
h() { hurl --test --variables-file "$env" --variable date="$date" "$@"; }
# A failed flow leaves its CONFIRMED booking (tickets ACTIVE) holding seats on the cab, which starves the next flow. With
# CLEAN_DB_URL set (a LOCAL rider-app Postgres only: localhost / 127.0.0.1 is enforced) every flow starts by cancelling them.
clean() {
  [ -n "${CLEAN_DB_URL:-}" ] || return 0
  case $CLEAN_DB_URL in
    *@localhost[:/]* | *@127.0.0.1[:/]* | *//localhost[:/]* | *//127.0.0.1[:/]*) ;;
    *) echo "CLEAN_DB_URL must point at a local database"; exit 2 ;;
  esac
  psql "$CLEAN_DB_URL" -q -At <<'SQL'
UPDATE atlas_app.frfs_ticket SET status = 'CANCELLED'
 WHERE status IN ('ACTIVE', 'INPROGRESS') AND frfs_ticket_booking_id IN (SELECT id FROM atlas_app.frfs_ticket_booking WHERE service_tier_type = 'SHARED_CAB');
UPDATE atlas_app.frfs_ticket_booking SET status = 'CANCELLED' WHERE service_tier_type = 'SHARED_CAB' AND status = 'CONFIRMED';
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
token() { echo "${TOKEN:-$(hurl --variables-file "$env" login.hurl | jq -r .token)}"; }
rider() { clean; h --variable token="$(token)" rider-flow.hurl; }
boarding() { clean; h --variable token="$(token)" boarding-flow.hurl; }
quantity() { clean; h --variable token="$(token)" quantity-flow.hurl; }
standalone() { clean; h --variable token="$(token)" standalone-flow.hurl; }
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

case ${1:-all} in
  driver) driver ;;
  flush) flush ;;
  rider) rider ;;
  boarding) boarding ;;
  standalone) standalone ;;
  suspended) suspended ;;
  quantity) quantity ;;
  all) driver; flush; rider; boarding; standalone; quantity ;;
  *) echo "usage: $0 [driver|flush|rider|boarding|standalone|quantity|suspended|all]"; exit 2 ;;
esac
