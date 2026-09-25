#!/usr/bin/env bash
# Runs the shared-cab scenarios against a live rider-app. usage: run.sh [driver|flush|rider|boarding|all] (default all)
# ENV (default local.env) picks the variables file; REDIS_CLI (default redis-cli) reaches the rider-app's Redis for the flush step.
set -euo pipefail
cd "$(dirname "$0")"
env=${ENV:-local.env}
date=$(TZ=Asia/Kolkata date +%F)
var() { sed -n "s/^$1=//p" "$env"; }
h() { hurl --test --variables-file "$env" --variable date="$date" "$@"; }

driver() { h driver-flow.hurl; }
flush() {
  h flush-before.hurl
  ${REDIS_CLI:-redis-cli} DEL "sharedcab:session:$(var plate)" "sharedcab:route:$(var route_a)" >/dev/null
  h flush-after.hurl
}
token() { echo "${TOKEN:-$(hurl --variables-file "$env" login.hurl | jq -r .token)}"; }
rider() { h --variable token="$(token)" rider-flow.hurl; }
boarding() { h --variable token="$(token)" boarding-flow.hurl; }

case ${1:-all} in
  driver) driver ;;
  flush) flush ;;
  rider) rider ;;
  boarding) boarding ;;
  all) driver; flush; rider; boarding ;;
  *) echo "usage: $0 [driver|flush|rider|boarding|all]"; exit 2 ;;
esac
