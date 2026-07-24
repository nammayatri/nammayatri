#!/usr/bin/env bash
# Self-seeding integration test for the FleetRegistration verified-gate.
# Runs the newman collection, then tears down the run's test fleet owner (PG + KV).
# The FleetRegistration fleet-doc-config rows are left in place (feature config; the
# collection re-seeds idempotently via ON CONFLICT DO NOTHING).
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COLLECTION="$HERE/01-FleetRegistrationGate.json"
ENV_FILE="$HERE/Local/Local_NY_Bangalore.postman_environment.json"
ENV_OUT="/tmp/freg-env-out.json"

PGH=localhost; PGP=5434; PGDB=atlas_dev
MOCK=http://localhost:8080

echo "== Running FleetRegistrationGate collection (CLI / newman) =="
newman run "$COLLECTION" -e "$ENV_FILE" --export-environment "$ENV_OUT" \
  --timeout-request 60000 --reporters cli
RC=$?

# ---- Teardown: remove the fleet owner this run created ----
FID=$(python3 -c "import json,sys
try:
  d=json.load(open('$ENV_OUT'))
  print(next((v['value'] for v in d.get('values',[]) if v['key']=='fleet_owner_id'), ''))
except Exception: print('')" 2>/dev/null)

if [ -n "$FID" ]; then
  echo "== Teardown: removing test fleet owner $FID =="
  psql -h "$PGH" -p "$PGP" -d "$PGDB" -tA -c "DELETE FROM atlas_driver_offer_bpp.registration_token WHERE entity_id='$FID';" 2>/dev/null
  psql -h "$PGH" -p "$PGP" -d "$PGDB" -tA -c "DELETE FROM atlas_driver_offer_bpp.fleet_owner_information WHERE fleet_owner_person_id='$FID';" 2>/dev/null
  psql -h "$PGH" -p "$PGP" -d "$PGDB" -tA -c "DELETE FROM atlas_driver_offer_bpp.person WHERE id='$FID';" 2>/dev/null
  curl -s -m 10 -X POST "$MOCK/mock/redis/del" -H 'Content-Type: application/json' \
    -d "{\"pattern\":\"*${FID}*\",\"target\":\"cluster\"}" >/dev/null 2>&1
else
  echo "== Teardown skipped (no fleet_owner_id exported) =="
fi

# Restore env: remove the FleetRegistration fleet-doc-config this run seeded (NAMMA/Bangalore).
MOC=f067bccf-5b34-fb51-a5a3-9d6fa6baac26
echo "== Teardown: removing seeded FleetRegistration config (NAMMA/Bangalore) =="
psql -h "$PGH" -p "$PGP" -d "$PGDB" -tA -c "DELETE FROM atlas_driver_offer_bpp.fleet_owner_document_verification_config WHERE document_type='FleetRegistration' AND merchant_operating_city_id='$MOC';" 2>/dev/null

echo "== Done (newman exit=$RC) =="
exit $RC
