#!/usr/bin/env bash
# Runs the shared-cab scenarios against a live rider-app. usage: run.sh [driver|flush|rider|boarding|all] (default all)
# ENV (default local.env) picks the variables file; REDIS_CLI (default redis-cli) reaches the rider-app's Redis for the flush step.
#   (the local Redis is a cluster: REDIS_CLI="redis-cli -c -p 30001")
#
# REQUIRED LOCAL SETUP (documented, not done by this script; full recipe in README.md):
#   * rebuild with the two hard gates flipped ON in a LOCAL-ONLY commit (never merge):
#       SharedLogic/SharedCab/DegradedSweepSchedule.hs  sharedCabAllocationEnabled = True   (defined here since batch9 H1)
#       SharedLogic/SharedCab/DegradedSweepSchedule.hs  sharedCabDegradedSweepEnabled = True
#   * seed/*.sql applied (config, geometry, drivers, fares, local DB flags), then CLEAR THE REDIS CONFIG CACHES and restart:
#       every app has its own prefix: app-backend:*, rider-app-scheduler:*, driver-offer:* (allocator), dynamic-offer-driver-app:*
#       (keys ConfigPilot:<Table> and CachedQueries:<Table>:...). A stale cache reads the old config; the driver auth token
#       cache dynamic-offer-driver-app:providerPlatform:authTokenCacheKey:<token> too.
#   * LTS: Backend/geo_config/<region>.json and Backend/route_geo_json_config/<routeCode>.geojson for the SC routes, and a cab
#     that pings LTS (POST :8081/ui/driver/location) or the allocation tick never sees it.
#   * the allocation tick chain dies with a scheduler restart: re-run route/select (or the driver flow) to reseed it.
set -euo pipefail
cd "$(dirname "$0")"
env=${ENV:-local.env}
date=$(TZ=Asia/Kolkata date +%F)
var() { sed -n "s/^$1=//p" "$env"; }
h() { hurl --test --variables-file "$env" --variable date="$date" "$@"; }

driver() { h driver-flow.hurl; }
flush() {
  h flush-before.hurl
  # one DEL per key: the keys hash to different cluster slots
  ${REDIS_CLI:-redis-cli} DEL "sharedcab:session:$(var plate)" >/dev/null
  ${REDIS_CLI:-redis-cli} DEL "sharedcab:route:$(var route_a)" >/dev/null
  h flush-after.hurl
}
token() { echo "${TOKEN:-$(hurl --variables-file "$env" login.hurl | jq -r .token)}"; }
rider() { h --variable token="$(token)" rider-flow.hurl; }
boarding() { h --variable token="$(token)" boarding-flow.hurl; }
standalone() { h --variable token="$(token)" standalone-flow.hurl; }

case ${1:-all} in
  driver) driver ;;
  flush) flush ;;
  rider) rider ;;
  boarding) boarding ;;
  standalone) standalone ;;
  all) driver; flush; rider; boarding; standalone ;;
  *) echo "usage: $0 [driver|flush|rider|boarding|standalone|all]"; exit 2 ;;
esac
