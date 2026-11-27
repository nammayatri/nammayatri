#!/usr/bin/env bash
# Runs the FleetManagement collection against the local stack on a data set it can pass on, then puts everything back.
#
# The collection acts as a dashboard admin who names a fleet owner's fleet. That only works when
#   - the admin has a person row in the driver-app database (the driver-app looks the requestor up there), and
#   - the merchant does not use the fleet member hierarchy (the collection sends hasFleetMemberHierarchy=false,
#     and the dashboard decides this from the merchant's own flag).
# A fresh local database has neither, so this script sets both for the run and restores them afterwards.
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COLLECTION="$HERE/01-DriverNameAndAssociation.json"
ENV_FILE="$HERE/Local/Local_NY_Bangalore.postman_environment.json"
ENV_OUT="/tmp/fleetmgmt-env-out.json"

PGH=localhost; PGP=5434; PGDB=atlas_dev
MOCK=http://localhost:8080
MERCHANT=${MERCHANT:-NAMMA_YATRI_PARTNER}
ADMIN_TOKEN=${ADMIN_TOKEN:-local-admin-token-bangalore-namma-yatri}
q() { psql -h "$PGH" -p "$PGP" -d "$PGDB" -tAq "$@" 2>/dev/null; }
drop_cache() { curl -s -m 10 -X POST "$MOCK/mock/redis/del" -H 'Content-Type: application/json' -d "{\"pattern\":\"$1\",\"target\":\"cluster\"}" >/dev/null 2>&1; }

SINCE=$(q -c "select now()")
ADMIN_ID=$(q -c "select person_id from atlas_dashboard.registration_token where token='$ADMIN_TOKEN'")
[ -n "$ADMIN_ID" ] || { echo "no dashboard session for the admin token; is the stack up?"; exit 2; }
ORIG_HIERARCHY=$(q -c "select has_fleet_member_hierarchy from atlas_dashboard.merchant where short_id='$MERCHANT'")
ADMIN_ROW_ADDED=no
if [ "$(q -c "select count(*) from atlas_driver_offer_bpp.person where id='$ADMIN_ID'")" = 0 ]; then
  q <<SQL
INSERT INTO atlas_driver_offer_bpp.person SELECT r.* FROM (SELECT * FROM atlas_driver_offer_bpp.person ORDER BY created_at LIMIT 1) p
  CROSS JOIN LATERAL jsonb_populate_record(null::atlas_driver_offer_bpp.person,
    to_jsonb(p) || jsonb_build_object('id','$ADMIN_ID','role','ADMIN','first_name','Admin','last_name','Local',
      'mobile_number_hash','\\x' || md5(random()::text))) r;
SQL
  ADMIN_ROW_ADDED=yes
fi
q -c "update atlas_dashboard.merchant set has_fleet_member_hierarchy=false where short_id='$MERCHANT'"
drop_cache "*${MERCHANT}*"

echo "== Running FleetManagement collection (CLI / newman) =="
newman run "$COLLECTION" -e "$ENV_FILE" --export-environment "$ENV_OUT" --timeout-request 60000 --reporters cli
RC=$?

value_of() { python3 -c "import json
try:
  d=json.load(open('$ENV_OUT'))
  print(next((v['value'] for v in d.get('values',[]) if v['key']=='$1'), ''))
except Exception: print('')" 2>/dev/null; }

for id in "$(value_of fleet_owner_id)" "$(value_of driver_id)"; do
  [ -z "$id" ] && continue
  # Only what this run made: a stale or wrong id must never reach seed data.
  [ "$(q -c "select count(*) from atlas_driver_offer_bpp.person where id='$id' and created_at > '$SINCE'")" = 1 ] || continue
  echo "== Teardown: removing $id =="
  q <<SQL
DELETE FROM atlas_driver_offer_bpp.fleet_driver_association WHERE fleet_owner_id='$id' OR driver_id='$id';
DELETE FROM atlas_dashboard.person_role_assignment WHERE person_id='$id' OR scope_id='$id';
DELETE FROM atlas_dashboard.registration_token WHERE person_id='$id';
DELETE FROM atlas_dashboard.merchant_access WHERE person_id='$id';
DELETE FROM atlas_dashboard.person WHERE id='$id';
DELETE FROM atlas_driver_offer_bpp.registration_token WHERE entity_id='$id';
DELETE FROM atlas_driver_offer_bpp.fleet_owner_information WHERE fleet_owner_person_id='$id';
DELETE FROM atlas_driver_offer_bpp.person WHERE id='$id';
SQL
  drop_cache "*${id}*"
done

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

[ "$ADMIN_ROW_ADDED" = yes ] && q -c "DELETE FROM atlas_driver_offer_bpp.person WHERE id='$ADMIN_ID'"
if [ -n "$ORIG_HIERARCHY" ]; then ORIG_SQL="'$ORIG_HIERARCHY'"; else ORIG_SQL=null; fi
q -c "update atlas_dashboard.merchant set has_fleet_member_hierarchy=$ORIG_SQL where short_id='$MERCHANT'"
drop_cache "*${MERCHANT}*"
echo "== Done (newman exit=$RC) =="
exit $RC
