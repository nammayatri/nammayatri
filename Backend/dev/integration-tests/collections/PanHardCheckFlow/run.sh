#!/bin/bash
# Run PAN hard-check integration tests.
#
# Usage:
#   ./run.sh                # default: NY_Bangalore
#   ./run.sh NY_Bangalore   # explicit city
#
# The collection is self-contained: it resolves the merchant uuid + seed
# driver/fleet-owner ids, seeds the fleet-owner FOI row, sets individual_pan_check,
# and resets it — all via the mock-server SQL endpoints (/mock/sql/*). So this
# runner only needs to materialize the ${VAR:default} port placeholders that the
# 7070 UI expands but newman does not. Override MOCK_SERVER_PORT /
# PROVIDER_DASHBOARD_PORT if your reachable ports differ.
#
# Prereqs: running stack (mock-server, provider-dashboard, driver-app) + seeded DB.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
CITY="${1:-NY_Bangalore}"
ENV_FILE="$SCRIPT_DIR/Local/Local_${CITY}.postman_environment.json"
[ -f "$ENV_FILE" ] || { echo "ERROR: env file not found: $ENV_FILE"; exit 1; }

MOCK_SERVER_PORT="${MOCK_SERVER_PORT:-8080}"
PROVIDER_DASHBOARD_PORT="${PROVIDER_DASHBOARD_PORT:-8018}"
DRIVER_APP_PORT="${DRIVER_APP_PORT:-8016}"
RESOLVED_ENV="$(mktemp -t pan-hard-check-env.XXXXXX.json)"
trap 'rm -f "$RESOLVED_ENV"' EXIT
sed -e "s/\${MOCK_SERVER_PORT:8080}/${MOCK_SERVER_PORT}/g" \
    -e "s/\${PROVIDER_DASHBOARD_PORT:8018}/${PROVIDER_DASHBOARD_PORT}/g" \
    -e "s/\${DRIVER_APP_PORT:8016}/${DRIVER_APP_PORT}/g" \
    "$ENV_FILE" > "$RESOLVED_ENV"

newman run "$SCRIPT_DIR/01-PanHardCheck.json" \
  -e "$RESOLVED_ENV" \
  --bail \
  --timeout-request 60000 \
  --reporters cli
