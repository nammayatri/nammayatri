#!/bin/bash
# Run driver-image integration tests with pre-test cleanup.
#
# Usage:
#   ./run.sh                     # Run tests (default: NY_Bangalore)
#   ./run.sh NY_Bangalore        # Explicit city
#
# Prerequisites:
#   - Local BPP server running on port 8116
#   - Seed data applied (dynamic-offer-driver-app.sql)
#   - newman installed

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
CITY="${1:-NY_Bangalore}"
ENV_FILE="$SCRIPT_DIR/Local/Local_${CITY}.postman_environment.json"

DB_HOST="localhost"
DB_PORT="5434"
DB_NAME="atlas_dev"
DB_USER="atlas_superuser"

if [ ! -f "$ENV_FILE" ]; then
    echo "ERROR: Environment file not found: $ENV_FILE"
    exit 1
fi

echo "Cleaning up AadhaarCard images from previous test runs for seed users..."
psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" -tAc "
  DELETE FROM atlas_driver_offer_bpp.image
  WHERE image_type = 'AadhaarCard'
    AND person_id IN (
      SELECT md5(m.id || ':seed-driver-person')::uuid::text FROM atlas_driver_offer_bpp.merchant m
      UNION
      SELECT md5(m.id || ':seed-fleet-owner')::uuid::text FROM atlas_driver_offer_bpp.merchant m
    );
" 2>/dev/null || echo "  (cleanup skipped — DB not reachable)"

echo ""
newman run "$SCRIPT_DIR/01-DriverImageFetch.json" \
  -e "$ENV_FILE" \
  --bail \
  --timeout-request 60000 \
  --reporters cli
