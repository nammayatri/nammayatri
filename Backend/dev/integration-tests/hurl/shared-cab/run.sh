#!/usr/bin/env bash
# Runs the shared-cab scenarios against a live rider-app. usage: run.sh [driver|flush|rider|boarding|all] (default all)
# ENV (default local.env) picks the variables file; REDIS_CLI (default redis-cli) reaches the rider-app's Redis for the flush
# step (a local cluster needs REDIS_CLI="redis-cli -c -p 30001"). Local setup is documented in README.md, not done here:
# seed/*.sql applied, the apps' Redis config caches cleared (each app has its own key prefix) and the processes restarted,
# LTS geo config for the SC routes, and a cab that pings LTS. The allocation tick chain dies with a scheduler restart: re-run
# route/select to reseed it.
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
quantity() { h --variable token="$(token)" quantity-flow.hurl; }
standalone() { h --variable token="$(token)" standalone-flow.hurl; }

case ${1:-all} in
  driver) driver ;;
  flush) flush ;;
  rider) rider ;;
  boarding) boarding ;;
  standalone) standalone ;;
  quantity) quantity ;;
  all) driver; flush; rider; boarding; standalone; quantity ;;
  *) echo "usage: $0 [driver|flush|rider|boarding|standalone|quantity|all]"; exit 2 ;;
esac
