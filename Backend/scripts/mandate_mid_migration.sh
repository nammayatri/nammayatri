#!/usr/bin/env bash
#
# Mandate MID migration: suspend + re-subscribe drivers via bpp-dashboard APIs.
#
# Uses the V1 plan routes (hardcoded to YATRI_SUBSCRIPTION server-side); the
# deployed dashboard 404s the /{serviceName}/v2/ variants.
#
# For each driver:
#   1. PUT  /plan/{driverId}/suspend            -> kills autopay locally, fees -> manual
#                                                  (fails harmlessly if driver not ACTIVE)
#   2. POST /plan/{driverId}/{planId}/subscribe -> status PENDING (blocks "resume"),
#                                                  sends WhatsApp mandate-setup link (new MID)
#
# planId MUST be the driver's CURRENT plan id (driver_plan.plan_id) — subscribing
# with a different plan id would switch their plan.
#
# Subscribe runs even if suspend fails, so re-running a half-migrated
# (SUSPENDED) driver completes them instead of leaving the resume footgun open.
#
# PREREQUISITE: the city's payment merchant_service_config must already point to the NEW MID
# before running this — the subscribe call creates the mandate order on whatever MID is live.
#
# Usage:
#   Test on one driver :  ./mandate_mid_migration.sh test <driverId> <planId>
#   Full batch         :  ./mandate_mid_migration.sh run <file>
#                         file lines: driverId,planId   (one per line, # comments ok)
#
# Required environment variables:
#   DASHBOARD_HOST      e.g. https://dashboard.c2.moving.tech/api
#   DASHBOARD_TOKEN     ops dashboard token with SUBSCRIPTION suspend/subscribe access
#   MERCHANT_SHORT_ID   e.g. NAMMA_YATRI_PARTNER
#   CITY                e.g. Thrissur
# Optional:
#   SLEEP_BETWEEN       seconds between drivers in batch mode, default 1

set -uo pipefail

SLEEP_BETWEEN="${SLEEP_BETWEEN:-1}"
RESULTS_FILE="mid_migration_results_$(date +%Y%m%d_%H%M%S).csv"

die() { echo "ERROR: $*" >&2; exit 1; }

command -v curl >/dev/null || die "curl not found"
command -v jq   >/dev/null || die "jq not found"

MODE="${1:-}"
[[ "$MODE" == "test" || "$MODE" == "run" ]] \
  || die "usage: $0 test <driverId> <planId> | run <file with driverId,planId lines>"

: "${DASHBOARD_HOST:?set DASHBOARD_HOST}"
: "${DASHBOARD_TOKEN:?set DASHBOARD_TOKEN}"
: "${MERCHANT_SHORT_ID:?set MERCHANT_SHORT_ID}"
: "${CITY:?set CITY}"

DASHBOARD_HOST="${DASHBOARD_HOST%/}"   # tolerate trailing slash
BASE="$DASHBOARD_HOST/bpp/driver-offer/$MERCHANT_SHORT_ID/$CITY/plan"

# api <METHOD> <url>  -> sets HTTP_STATUS and BODY
api() {
  local method="$1" url="$2"
  echo "  -> $method $url" >&2
  local resp
  resp=$(curl -s -w '\n%{http_code}' -X "$method" \
    -H "token: $DASHBOARD_TOKEN" -H "Content-Type: application/json" \
    "$url") || { HTTP_STATUS=000; BODY="curl failed"; return 1; }
  HTTP_STATUS=$(tail -n1 <<<"$resp")
  BODY=$(sed '$d' <<<"$resp")
  [[ "$HTTP_STATUS" == 2* ]]
}

record() { echo "$1,$2,$3,$4" >> "$RESULTS_FILE"; }

migrate_driver() {
  local driverId="$1" planId="$2"
  echo "----------------------------------------------------------------"
  echo "Driver: $driverId  plan: $planId"

  # 1. Suspend (expected to fail with InvalidAutoPayStatus if driver isn't ACTIVE —
  #    e.g. re-running a half-migrated driver — so we continue to subscribe regardless)
  local suspendResult
  if api PUT "$BASE/$driverId/suspend"; then
    suspendResult="ok"
    echo "  suspended OK"
  else
    suspendResult="http_$HTTP_STATUS"
    echo "  suspend failed (HTTP $HTTP_STATUS): $(head -c 200 <<<"$BODY")"
    echo "  continuing to subscribe anyway..."
  fi

  # 2. Subscribe (moves to PENDING, sends WhatsApp setup link on new MID)
  if ! api POST "$BASE/$driverId/$planId/subscribe"; then
    echo "  FAIL: subscribe (HTTP $HTTP_STATUS): $(head -c 200 <<<"$BODY")"
    if [[ "$suspendResult" == "ok" ]]; then
      echo "  !! driver left SUSPENDED — resume footgun open, re-run this driver ASAP"
      record "$driverId" "FAIL" "suspend_$suspendResult" "subscribe_http_${HTTP_STATUS}_LEFT_SUSPENDED"
    else
      record "$driverId" "FAIL" "suspend_$suspendResult" "subscribe_http_$HTTP_STATUS"
    fi
    return 1
  fi

  local orderId
  orderId=$(jq -r '.orderId // "null"' <<<"$BODY" 2>/dev/null || echo "unparsed")
  echo "  subscribed OK -> driver now PENDING (new mandate orderId=$orderId)"
  record "$driverId" "OK" "suspend_$suspendResult" "order_$orderId"
  return 0
}

echo "host=$DASHBOARD_HOST merchant=$MERCHANT_SHORT_ID city=$CITY mode=$MODE"
echo "driverId,result,suspend,detail" > "$RESULTS_FILE"

if [[ "$MODE" == "test" ]]; then
  DRIVER_ID="${2:-}"; PLAN_ID="${3:-}"
  [[ -n "$DRIVER_ID" && -n "$PLAN_ID" ]] || die "usage: $0 test <driverId> <planId>"
  migrate_driver "$DRIVER_ID" "$PLAN_ID"
  echo "----------------------------------------------------------------"
  echo "Results: $RESULTS_FILE"
  exit 0
fi

# Batch mode
FILE="${2:-}"
[[ -n "$FILE" && -f "$FILE" ]] || die "driver file not found: ${FILE:-<missing>}"
LINES=()
while IFS= read -r line; do LINES+=("$line"); done \
  < <(grep -vE '^\s*(#|$)' "$FILE" | tr -d ' \r')
echo "Batch mode: ${#LINES[@]} drivers from $FILE"
read -r -p "Type 'yes' to proceed: " confirm
[[ "$confirm" == "yes" ]] || die "aborted"

ok=0; bad=0
for entry in "${LINES[@]}"; do
  driverId="${entry%%,*}"
  planId="${entry#*,}"
  if [[ -z "$driverId" || -z "$planId" || "$driverId" == "$planId" ]]; then
    echo "SKIP malformed line: '$entry' (expected driverId,planId)"
    record "$entry" "SKIP" "-" "malformed_line"
    bad=$((bad+1))
    continue
  fi
  if migrate_driver "$driverId" "$planId"; then ok=$((ok+1)); else bad=$((bad+1)); fi
  sleep "$SLEEP_BETWEEN"
done

echo "================================================================"
echo "Done. success=$ok failed/skipped=$bad  results: $RESULTS_FILE"
[[ $bad -gt 0 ]] && echo "Review failures — any 'LEFT_SUSPENDED' driver must be re-run immediately."
