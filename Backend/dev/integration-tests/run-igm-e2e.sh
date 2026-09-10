#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
RIDE_DIR="$SCRIPT_DIR/collections/RideBookingFlow"
LOCAL_TEST_DATA="$SCRIPT_DIR/../local-testing-data"
ENV_FILE_RAW="$RIDE_DIR/Local/Local_NY_Bangalore.postman_environment.json"
ENV_FILE="/tmp/ny-env-resolved.json"
EXPORTED_ENV="/tmp/ny-env-after-ride.json"

# Resolve ${VAR:default} patterns that newman can't handle
python3 -c "
import json, re
with open('$ENV_FILE_RAW') as f:
    d = json.load(f)
for v in d['values']:
    v['value'] = re.sub(r'\\\$\{[^:]+:([^}]+)\}', r'\1', str(v.get('value','')))
with open('$ENV_FILE', 'w') as f:
    json.dump(d, f, indent=2)
print('  Env vars resolved (ports expanded).')
"

DB_HOST="localhost"
DB_PORT="5434"
DB_NAME="atlas_dev"
DB_USER_SUPER="atlas_superuser"

restore_kv() {
  psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER_SUPER" -d "$DB_NAME" -c \
    "UPDATE atlas_driver_offer_bpp.system_configs SET config_value = jsonb_set(config_value::jsonb, '{allTablesDisabled}', 'false') WHERE id = 'kv_configs';" > /dev/null 2>&1
  echo "  KV restored."
}
trap restore_kv EXIT

echo "=========================================="
echo "  Step 0: Seed Dashboard Tokens"
echo "=========================================="
psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER_SUPER" -d "$DB_NAME" \
  -f "$LOCAL_TEST_DATA/provider-dashboard.sql" > /dev/null 2>&1
echo "  Provider dashboard seeded."

psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER_SUPER" -d "$DB_NAME" \
  -f "$LOCAL_TEST_DATA/rider-dashboard.sql" > /dev/null 2>&1
echo "  Rider dashboard seeded."

# Disable KV BEFORE ride flow so bookings go to Postgres, not Redis
psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER_SUPER" -d "$DB_NAME" -c \
  "UPDATE atlas_driver_offer_bpp.system_configs SET config_value = jsonb_set(config_value::jsonb, '{allTablesDisabled}', 'true') WHERE id = 'kv_configs';" > /dev/null 2>&1
echo "  KV disabled."

# Flush Redis (same as run-tests.sh)
redis-cli -p 6379 FLUSHALL > /dev/null 2>&1 || true
for port in 30001 30002 30003 30004 30005 30006; do
    redis-cli -p $port FLUSHALL > /dev/null 2>&1 || true
done
redis-cli -p 30001 -c XGROUP CREATE Available_Jobs myGroup 0 MKSTREAM > /dev/null 2>&1 || true
redis-cli -p 30001 -c XGROUP CREATE Available_Jobs_Rider myGroup_Rider 0 MKSTREAM > /dev/null 2>&1 || true
redis-cli -p 30001 -c XGROUP CREATE Available_Chakras myGroup_Chakras 0 MKSTREAM > /dev/null 2>&1 || true
redis-cli -p 30001 -c XGROUP CREATE "ab:n_c:Available_Jobs_Rider" myGroup_Rider 0 MKSTREAM > /dev/null 2>&1 || true
echo "  Redis flushed."

# Wait for config polling to pick up KV disable
echo "  Waiting 10s for config polling..."
sleep 10

echo ""
echo "=========================================="
echo "  Step 1: Run Auto Ride Flow"
echo "=========================================="
newman run "$RIDE_DIR/01-AutoRideFlow.json" \
  -e "$ENV_FILE" \
  --export-environment "$EXPORTED_ENV" \
  --bail \
  --timeout-request 60000 \
  --reporters cli \
  --verbose

# Resolve BPP booking ID from BAP booking ID
# BAP booking (customer_bookingId) -> bpp_ride_booking_id -> BPP booking
BAP_BOOKING_ID=$(python3 -c "
import json
with open('$EXPORTED_ENV') as f:
    d = json.load(f)
for v in d['values']:
    if v['key'] == 'customer_bookingId':
        print(v['value'])
        break
")

BPP_BOOKING_ID=$(psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER_SUPER" -d "$DB_NAME" -tAc \
  "SELECT bpp_ride_booking_id FROM atlas_app.booking WHERE id = '$BAP_BOOKING_ID';" 2>/dev/null | tr -d ' ')

echo "  BAP booking: $BAP_BOOKING_ID -> BPP booking: $BPP_BOOKING_ID"

# Inject BPP booking ID as customer_bookingId for IGM tests
python3 -c "
import json
with open('$EXPORTED_ENV') as f:
    d = json.load(f)
for v in d['values']:
    if v['key'] == 'customer_bookingId':
        v['value'] = '$BPP_BOOKING_ID'
        break
with open('$EXPORTED_ENV', 'w') as f:
    json.dump(d, f, indent=2)
"
echo "  customer_bookingId overwritten with BPP booking ID"

echo ""
echo "=========================================="
echo "  Step 2: Seed IGM Prerequisites"
echo "=========================================="

psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER_SUPER" -d "$DB_NAME" <<'SQL'
-- IGM Config
INSERT INTO atlas_driver_offer_bpp.igm_config (
  id, merchant_id, gro_name, gro_email, gro_phone,
  expected_response_time, expected_resolution_time,
  respondent_name, respondent_phone, respondent_email,
  resolution_provider_name, resolution_provider_phone, resolution_provider_email,
  created_at, updated_at
) VALUES (
  'igm-config-test', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f',
  'Test GRO', 'gro@nammayatri.in', '9999999999',
  7200, 86400,
  'NammaYatri Support', '8888888888', 'support@nammayatri.in',
  'NammaYatri Resolution', '7777777777', 'resolution@nammayatri.in',
  NOW(), NOW()
) ON CONFLICT (id) DO NOTHING;

-- Issue Category with IGM mapping
INSERT INTO atlas_driver_offer_bpp.issue_category (
  id, category, logo_url, priority, igm_category,
  merchant_id, merchant_operating_city_id, category_type, is_active,
  created_at, updated_at
) SELECT
  'igm-cat-test                        ', 'ride related',
  'https://assets.moving.tech/beckn/nammayatri/driver/images/ny_ic_issue.png',
  1, 'FULFILLMENT',
  '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f',
  '1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98',
  'Category', true, NOW(), NOW()
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_driver_offer_bpp.issue_category WHERE id = 'igm-cat-test                        '
);

-- Issue Option with IGM sub-category mapping
INSERT INTO atlas_driver_offer_bpp.issue_option (
  id, issue_category_id, option, priority, igm_sub_category,
  merchant_id, merchant_operating_city_id, is_active,
  created_at, updated_at
) SELECT
  'igm-opt-test                        ', 'igm-cat-test                        ',
  'Driver unable to end trip', 1, 'FLM111',
  '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f',
  '1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98',
  true, NOW(), NOW()
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_driver_offer_bpp.issue_option WHERE id = 'igm-opt-test                        '
);

-- Dashboard capability endpoints for IGM APIs
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id, created_at)
VALUES
  ('city-operations.driver_issue.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_IGM_ISSUE_TRAIL', NOW()),
  ('city-operations.driver_issue.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_IGM_ISSUE_TRIGGER_ACTION_UPDATE', NOW())
ON CONFLICT (server_name, endpoint_id) DO NOTHING;
SQL

echo "  IGM prerequisites seeded."
echo ""
echo "  Waiting 10s for config polling..."
sleep 10

echo "=========================================="
echo "  Step 3: Run IGM OffUs Flow"
echo "=========================================="
newman run "$RIDE_DIR/14-IGMOffUsFlow.json" \
  -e "$EXPORTED_ENV" \
  --timeout-request 60000 \
  --reporters cli \
  --verbose

echo ""
echo "=========================================="
echo "  IGM E2E Complete"
echo "=========================================="
