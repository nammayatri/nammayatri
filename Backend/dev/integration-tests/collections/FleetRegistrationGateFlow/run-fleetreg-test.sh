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

q() { psql -h "$PGH" -p "$PGP" -d "$PGDB" -tAq "$@" 2>/dev/null; }
SINCE=$(q -c "select now()")
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

if [ -n "$FID" ] && [ "$(q -c "select count(*) from atlas_driver_offer_bpp.person where id='$FID' and created_at > '$SINCE'")" = 1 ]; then
  echo "== Teardown: removing test fleet owner $FID =="
  psql -h "$PGH" -p "$PGP" -d "$PGDB" -tA -c "DELETE FROM atlas_driver_offer_bpp.registration_token WHERE entity_id='$FID';" 2>/dev/null
  psql -h "$PGH" -p "$PGP" -d "$PGDB" -tA -c "DELETE FROM atlas_driver_offer_bpp.fleet_owner_information WHERE fleet_owner_person_id='$FID';" 2>/dev/null
  psql -h "$PGH" -p "$PGP" -d "$PGDB" -tA -c "DELETE FROM atlas_driver_offer_bpp.person WHERE id='$FID';" 2>/dev/null
  curl -s -m 10 -X POST "$MOCK/mock/redis/del" -H 'Content-Type: application/json' \
    -d "{\"pattern\":\"*${FID}*\",\"target\":\"cluster\"}" >/dev/null 2>&1
else
  echo "== Teardown skipped (no fleet owner made by this run to remove) =="
fi

# Safety net: anything else the run made (fleet owners, drivers and dashboard persons created after it started).
# The app writes through Redis and a drainer copies to Postgres a little later, so rows can still be in flight:
# sweep until two sweeps in a row find nothing.
sweep() {
  q <<SQL
CREATE TEMP TABLE swept AS SELECT id FROM atlas_driver_offer_bpp.person WHERE created_at > '$SINCE' AND role IN ('FLEET_OWNER','DRIVER','OPERATOR')
  UNION SELECT fleet_owner_person_id FROM atlas_driver_offer_bpp.fleet_owner_information WHERE created_at > '$SINCE'
  UNION SELECT id FROM atlas_dashboard.person WHERE created_at > '$SINCE';
DELETE FROM atlas_driver_offer_bpp.fleet_driver_association WHERE fleet_owner_id IN (SELECT id FROM swept) OR driver_id IN (SELECT id FROM swept);
DELETE FROM atlas_dashboard.person_role_assignment WHERE person_id IN (SELECT id FROM swept) OR scope_id IN (SELECT id FROM swept);
DELETE FROM atlas_dashboard.registration_token WHERE person_id IN (SELECT id FROM swept);
DELETE FROM atlas_dashboard.merchant_access WHERE person_id IN (SELECT id FROM swept);
DELETE FROM atlas_dashboard.person WHERE id IN (SELECT id FROM swept);
DELETE FROM atlas_driver_offer_bpp.registration_token WHERE entity_id IN (SELECT id FROM swept);
DELETE FROM atlas_driver_offer_bpp.fleet_owner_information WHERE fleet_owner_person_id IN (SELECT id FROM swept);
DELETE FROM atlas_driver_offer_bpp.person WHERE id IN (SELECT id FROM swept);
SELECT count(*) FROM swept;
SQL
}
quiet=0
for _ in 1 2 3 4 5 6 7 8; do
  if [ "$(sweep | tail -1)" = 0 ]; then quiet=$((quiet + 1)); else quiet=0; fi
  [ "$quiet" -ge 2 ] && break
  sleep 15
done

# Restore env: remove the FleetRegistration fleet-doc-config this run seeded (NAMMA/Bangalore).
MOC=$(python3 -c "import json
try:
  d=json.load(open('$ENV_OUT'))
  print(next((v['value'] for v in d.get('values',[]) if v['key']=='moc_id'), ''))
except Exception: print('')" 2>/dev/null)
echo "== Teardown: removing seeded FleetRegistration config (NAMMA/Bangalore) =="
psql -h "$PGH" -p "$PGP" -d "$PGDB" -tA -c "DELETE FROM atlas_driver_offer_bpp.fleet_owner_document_verification_config WHERE document_type='FleetRegistration' AND merchant_operating_city_id='$MOC';" 2>/dev/null

curl -s -m 10 -X POST "$MOCK/mock/redis/del" -H 'Content-Type: application/json' \
  -d '{"pattern":"*FleetOwnerDocumentVerificationConfig*","target":"cluster"}' >/dev/null 2>&1
echo "== Done (newman exit=$RC) =="
exit $RC
