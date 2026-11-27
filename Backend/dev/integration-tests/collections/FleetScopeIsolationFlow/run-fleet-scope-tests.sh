#!/usr/bin/env bash
# Runs the fleet-scope collections against the local stack, then removes the fleets they created.
#
#   FleetScopeIsolationFlow/01-CrossFleetIsolation.json   one owner cannot reach another's fleet (ENFORCE)
#   FleetScopeLifecycleFlow/01-CreateAndLifecycle.json    create a fleet, its admin, last-admin guard, temp code
#   FleetScopeReviewFlow/01-RejectAndApprove.json         reject a fleet, what closes, approve it, what comes back
#
# Needs the local stack up (driver-app on 8016, provider-dashboard on 8018, mock-server on 8080) and the
# fleet-scope migrations applied: dev/ddl-migrations/{provider-dashboard 0112 0113, dynamic-offer-driver-app 0900}.
# The isolation collection switches the merchant to ENFORCE for its duration and restores the mode after.
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
PGH=localhost; PGP=5434; PGDB=atlas_dev
MOCK=http://localhost:8080
RC=0

run() {
  local name="$1" collection="$2" env_in="$3" env_out="$4"
  echo "== Running $name =="
  newman run "$collection" -e "$env_in" --export-environment "$env_out" --timeout-request 60000 --reporters cli
  local rc=$?
  [ $rc -ne 0 ] && RC=$rc
}

value_of() { # $1 = env export, $2 = key
  python3 -c "import json,sys
try:
  d=json.load(open('$1'))
  print(next((v['value'] for v in d.get('values',[]) if v['key']=='$2'), ''))
except Exception: print('')" 2>/dev/null
}

remove_fleet() {
  local id="$1"
  [ -z "$id" ] && return
  echo "== Teardown: removing test fleet $id =="
  psql -h "$PGH" -p "$PGP" -d "$PGDB" -tAq <<SQL 2>/dev/null
DELETE FROM atlas_dashboard.person_role_assignment WHERE scope_id='$id' OR person_id='$id';
DELETE FROM atlas_dashboard.registration_token WHERE person_id='$id';
DELETE FROM atlas_dashboard.merchant_access WHERE person_id='$id';
DELETE FROM atlas_dashboard.person WHERE id='$id';
DELETE FROM atlas_driver_offer_bpp.registration_token WHERE entity_id='$id';
DELETE FROM atlas_driver_offer_bpp.fleet_owner_information WHERE fleet_owner_person_id='$id';
DELETE FROM atlas_driver_offer_bpp.vehicle_registration_certificate WHERE fleet_owner_id='$id';
DELETE FROM atlas_driver_offer_bpp.person WHERE id='$id';
SQL
  curl -s -m 10 -X POST "$MOCK/mock/redis/del" -H 'Content-Type: application/json' \
    -d "{\"pattern\":\"*${id}*\",\"target\":\"cluster\"}" >/dev/null 2>&1
}

run "cross-fleet isolation" "$HERE/01-CrossFleetIsolation.json" \
  "$HERE/Local/Local_NY_Bangalore.postman_environment.json" /tmp/fleetscope-iso-env.json
run "create and lifecycle" "$ROOT/FleetScopeLifecycleFlow/01-CreateAndLifecycle.json" \
  "$ROOT/FleetScopeLifecycleFlow/Local/Local_NY_Bangalore.postman_environment.json" /tmp/fleetscope-life-env.json
run "reject and approve" "$ROOT/FleetScopeReviewFlow/01-RejectAndApprove.json" \
  "$ROOT/FleetScopeReviewFlow/Local/Local_NY_Bangalore.postman_environment.json" /tmp/fleetscope-review-env.json

# Teardown: every fleet and owner the runs created.
for f in /tmp/fleetscope-iso-env.json /tmp/fleetscope-life-env.json /tmp/fleetscope-review-env.json; do
  for key in fleet_a_id fleet_b_id new_fleet_id; do
    remove_fleet "$(value_of "$f" "$key")"
  done
done

# The role the review run gives the admin is looked up above by the fleet's id. A run that never got that id stores
# the role with an empty one, which no id-based teardown finds, so it is also removed by the name the run gives it.
psql -h "$PGH" -p "$PGP" -d "$PGDB" -tAq -c \
  "DELETE FROM atlas_dashboard.person_role_assignment WHERE trim(id) LIKE '%-review-assignment';" 2>/dev/null

# Safety net: never leave the merchant in a scope mode the run set.
psql -h "$PGH" -p "$PGP" -d "$PGDB" -tAq -c \
  "UPDATE atlas_dashboard.merchant SET fleet_scope_mode='OFF' WHERE fleet_scope_mode='ENFORCE' AND short_id='NAMMA_YATRI_PARTNER';" 2>/dev/null

exit $RC
