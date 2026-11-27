#!/usr/bin/env bash
# Checks for the fleet-scope pieces the Newman collections cannot reach, because they read the session's
# selection out of Redis or need a person's sessions to be ended:
#
#   1. capabilities per scope      GET /user/capabilities?fleetIds=...
#   2. the old fleet toggles       they change the session's fleet selection once the merchant is in ENFORCE
#   3. sessions end on an admin grant
#   4. the `cities` filter on the fleet lists, on real vehicle rows (which carry cityId and cityName)
#   5. the operator link's consent OTP, sent by fleet id to the fleet's own contact phone
#   6. the provider-dashboard OTP login mints the session for the person the OTP was issued to
#   7. the `cities` filter on the list served through provider-dashboard, outside ENFORCE
#
# Needs the local stack up (driver-app on 8016 behind caddy, provider-dashboard on 8018, mock-server on 8080)
# and the fleet-scope migrations applied. The script switches the merchant to ENFORCE for its duration, restores
# the mode afterwards, and removes the fleets and persons it created. Run it from anywhere:
#
#   PATH=~/fleet-scope-test-tools/bin:$PATH Backend/dev/integration-tests/fleet-scope-extras.sh
#
# Needs: curl, python3, psql (the wrapper above talks to the dev database over the socket as the superuser)
# and redis-cli (inside `nix develop .#backend`).
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/fleet-scope-lib.sh"

# The plan, ride-list and cash-ride routes are mapped to capabilities the seeded fleet-owner role does not hold,
# and an unmapped or ungranted route is refused before the handler runs. Grant them for this run, before the owner
# logs in (a person's capabilities are cached once read), and take back only the ones added here.
FLEET_OWNER_ROLE=$(psql -d atlas_dev -tAc "select id from atlas_dashboard.role where dashboard_access_type='FLEET_OWNER'")
GRANTED_CAPS=""
for cap in city-operations.subscription.read city-operations.ride.read city-operations.driver.write; do
  if [ "$(psql -d atlas_dev -tAc "select count(*) from atlas_dashboard.role_capability where role_id='$FLEET_OWNER_ROLE' and capability_id='$cap'")" = 0 ]; then
    psql -d atlas_dev -tA >/dev/null -c "insert into atlas_dashboard.role_capability (role_id, capability_id) values ('$FLEET_OWNER_ROLE', '$cap')"
    GRANTED_CAPS="$GRANTED_CAPS $cap"
  fi
done
ASSIGNED_ROLE=$(psql -d atlas_dev -tAc "select id from atlas_dashboard.role where name='FLEET'")
GRANTED_PAIRS=""
for pattern in '%/DRIVER/GET_DRIVER_STATS' '%/DRIVER/GET_DRIVER_FY_EARNINGS' '%/DRIVER/GET_DRIVER_IDENTITY_INFO' '%/DRIVER/POST_DRIVER_ASSOCIATION_CHANGE' \
               '%/DRIVER/POST_DRIVER_CLEAR_FEE' '%/DRIVER/POST_DRIVER_REFUND_BY_PAYOUT' '%/DRIVER/DELETE_DRIVER_PERMANENTLY_DELETE' \
               '%/GET_FINANCE_MANAGEMENT_FINANCE_INVOICE_LIST' '%/GET_FINANCE_MANAGEMENT_TDS_REIMBURSEMENT_STATUS' '%/POST_FINANCE_MANAGEMENT_TDS_REIMBURSEMENT_REQUEST_SUBMIT' \
               'PROVIDER_RIDE_BOOKING/DRIVER/GET_DRIVER_INFO' '%REGISTRATION_V2_CONTACT' '%GET_DRIVER_FLEET_SCHEDULED_BOOKING_LIST'; do
  for cap in $(psql -d atlas_dev -tAc "select distinct capability_id from atlas_dashboard.capability_endpoint where endpoint_id like '$pattern'"); do
    for role in "$FLEET_OWNER_ROLE" "$ASSIGNED_ROLE"; do
      if [ "$(psql -d atlas_dev -tAc "select count(*) from atlas_dashboard.role_capability where role_id='$role' and capability_id='$cap'")" = 0 ]; then
        psql -d atlas_dev -tA >/dev/null -c "insert into atlas_dashboard.role_capability (role_id, capability_id) values ('$role', '$cap')"
        GRANTED_PAIRS="$GRANTED_PAIRS $role:$cap"
      fi
    done
  done
done
revoke_caps() {
  for cap in $GRANTED_CAPS; do psql -d atlas_dev -tA >/dev/null -c "delete from atlas_dashboard.role_capability where role_id='$FLEET_OWNER_ROLE' and capability_id='$cap'"; done
  for pair in $GRANTED_PAIRS; do psql -d atlas_dev -tA >/dev/null -c "delete from atlas_dashboard.role_capability where role_id='${pair%%:*}' and capability_id='${pair##*:}'"; done
}
trap 'revoke_caps; cleanup' EXIT

echo "== setup: an owner holding two fleets"
TOKEN=$(login)
FLEET_A=$(person_of "$TOKEN")
# The mark that lets an OTP session be refused later is on the session's own row, so it lasts as long as the session.
check "an OTP session is marked on its row" "$(psql -d atlas_dev -tAc "select is_otp_session from atlas_dashboard.registration_token where token='$TOKEN'")" t
[ -n "$TOKEN" ] && [ -n "$FLEET_A" ] && ok "owner logged in, fleet A is $FLEET_A" || { bad "owner login"; exit 1; }
FLEET_B=$(curl -s -m 20 -X POST "$DRIVER/fleet/v2/create" -H "token: $TOKEN" -H 'Content-Type: application/json' \
  -d "{\"fleetName\":\"Extras fleet B\",\"fleetType\":\"NORMAL_FLEET\",\"contactMobileCountryCode\":\"+91\",\"contactMobileNumber\":\"$(mobile)\",\"contactEmail\":\"extras-b@example.com\"}" | json "d['fleetId']")
[ -n "$FLEET_B" ] && ok "owner created fleet B ($FLEET_B)" || { bad "create fleet B"; exit 1; }
sleep 12 # the app writes through Redis and a drainer copies to Postgres

echo "== 1. capabilities per scope"
caps() { curl -s -m 20 "$DASH/user/capabilities$1" -H "token: $TOKEN" | json "','.join(sorted(d['capabilities']))"; }
CAPS_NONE=$(caps "")
CAPS_A=$(caps "?fleetIds=$FLEET_A")
CAPS_AB=$(caps "?fleetIds=$FLEET_A,$FLEET_B")
[ -n "$CAPS_A" ] && ok "capabilities for one fleet are not empty" || bad "capabilities for fleet A came back empty"
check "two fleets with the same role give that role's capabilities" "$CAPS_AB" "$CAPS_A"
check "a fleet the caller does not hold is refused" "$(status "$DASH/user/capabilities?fleetIds=00000000-0000-4000-8000-000000000000" -H "token: $TOKEN")" 403
[ -n "$CAPS_NONE" ] && ok "no fleet named still returns the person's own capabilities" || bad "own capabilities came back empty"

echo "== 2. the old toggles change the session selection (ENFORCE)"
set_mode ENFORCE
SEL_URL="$DRIVER/driver/fleet/v2/access/select"
MULTI_URL="$DRIVER/driver/fleet/v2/access/multiOwnerId/select"
check "select fleet A only" "$(status -X POST "$SEL_URL?enable=true&onlyCurrent=true&fleetOwnerId=$FLEET_A" -H "token: $TOKEN")" 200
check "the session holds just fleet A" "$(selection_of "$TOKEN")" "$FLEET_A"
check "add fleet B (everything held = no selection)" "$(status -X POST "$SEL_URL?enable=true&fleetOwnerId=$FLEET_B" -H "token: $TOKEN")" 200
check "the selection is cleared when it covers every fleet held" "$(selection_of "$TOKEN")" ""
check "turn fleet B off through the multi-fleet toggle" "$(status -X POST "$MULTI_URL?enable=false" -H "token: $TOKEN" -H 'Content-Type: application/json' -d "{\"fleetOwnerIds\":[\"$FLEET_B\"]}")" 200
check "the session holds just fleet A again" "$(selection_of "$TOKEN")" "$FLEET_A"
check "the last selected fleet cannot be turned off" "$(status -X POST "$SEL_URL?enable=false&fleetOwnerId=$FLEET_A" -H "token: $TOKEN")" 400
check "a fleet that is not held is refused" "$(status -X POST "$SEL_URL?enable=true&fleetOwnerId=00000000-0000-4000-8000-000000000000" -H "token: $TOKEN")" 403

echo "== 4. the cities filter on a fleet list"
LIST="$DRIVER/driver/fleet/vehicleAssociation?Limit=10&Offset=0"
check "a known city is accepted" "$(status "$LIST&cities=Bangalore" -H "token: $TOKEN")" 200
# A fleet-bound caller may name only cities they have access to. This owner has Bangalore only, until Chennai is given.
check "a city the owner has no access to is refused" "$(status "$LIST&cities=Chennai" -H "token: $TOKEN")" 403
grant_chennai() {
  psql -d atlas_dev -tA >/dev/null <<SQL
INSERT INTO atlas_dashboard.merchant_access SELECT r.* FROM atlas_dashboard.merchant_access m
  CROSS JOIN LATERAL jsonb_populate_record(null::atlas_dashboard.merchant_access,
    to_jsonb(m) || jsonb_build_object('id', gen_random_uuid()::text, 'operating_city', 'Chennai')) r
  WHERE m.person_id='$FLEET_A' AND m.operating_city='Bangalore';
SQL
}
revoke_chennai() { psql -d atlas_dev -tA >/dev/null -c "delete from atlas_dashboard.merchant_access where person_id='$FLEET_A' and operating_city='Chennai'"; }
grant_chennai
check "several known cities are accepted" "$(status "$LIST&cities=Bangalore,Chennai" -H "token: $TOKEN")" 200
check "a city the merchant does not operate in is refused" "$(status "$LIST&cities=Atlantis" -H "token: $TOKEN")" 403

# Two vehicles for fleet A, one in each city, copied from existing certificates (their encrypted numbers decrypt).
BLR=$(psql -d atlas_dev -tAc "select id from atlas_driver_offer_bpp.merchant_operating_city where merchant_short_id='$MERCHANT' and city='Bangalore'")
CHN=$(psql -d atlas_dev -tAc "select id from atlas_driver_offer_bpp.merchant_operating_city where merchant_short_id='$MERCHANT' and city='Chennai'")
for CITY in "$BLR" "$CHN"; do
  psql -d atlas_dev -tA >/dev/null <<SQL
INSERT INTO atlas_driver_offer_bpp.vehicle_registration_certificate SELECT r.* FROM
  (SELECT * FROM atlas_driver_offer_bpp.vehicle_registration_certificate ORDER BY id LIMIT 1) v
  CROSS JOIN LATERAL jsonb_populate_record(null::atlas_driver_offer_bpp.vehicle_registration_certificate,
    to_jsonb(v) || jsonb_build_object('id', gen_random_uuid()::text, 'fleet_owner_id','$FLEET_A', 'merchant_operating_city_id','$CITY',
      'certificate_number_hash', '\\x' || md5(random()::text), 'verification_status','VALID')) r;
SQL
done
listed() { curl -s -m 30 "$LIST$1" -H "token: $TOKEN" | json "','.join(sorted((i.get('cityName') or '-') for i in d['listItem']))"; }
check "every city is covered by default in ENFORCE, and rows carry their city" "$(listed "")" "Bangalore,Chennai"
check "narrowing to one city returns only its vehicles" "$(listed "&cities=Chennai")" "Chennai"
check "narrowing to two cities returns both" "$(listed "&cities=Bangalore,Chennai")" "Bangalore,Chennai"
check "a city std code is read like its name (the UI sends codes)" "$(listed "&cities=std:044")" "Chennai"
# The active and inactive vehicle lists (isActive) take the same `cities`. An active vehicle has a current driver
# association; an inactive one only an ended association. Chennai's certificate is made active and Bangalore's inactive.
RC_BLR=$(psql -d atlas_dev -tAc "select id from atlas_driver_offer_bpp.vehicle_registration_certificate where fleet_owner_id='$FLEET_A' and merchant_operating_city_id='$BLR' limit 1")
RC_CHN=$(psql -d atlas_dev -tAc "select id from atlas_driver_offer_bpp.vehicle_registration_certificate where fleet_owner_id='$FLEET_A' and merchant_operating_city_id='$CHN' limit 1")
ASSOC_TAG="extras-assoc-$$"
psql -d atlas_dev -tA -v ON_ERROR_STOP=1 >/dev/null <<SQL
INSERT INTO atlas_driver_offer_bpp.driver_rc_association (id, driver_id, rc_id, associated_on, associated_till, consent, consent_timestamp, is_rc_active, merchant_id, merchant_operating_city_id, created_at, updated_at)
  VALUES ('$ASSOC_TAG-1', '$FLEET_A', '$RC_CHN', now() - interval '1 day', now() + interval '1 day', true, now(), true, NULL, '$CHN', now(), now()),
         ('$ASSOC_TAG-2', '$FLEET_A', '$RC_BLR', now() - interval '3 day', now() - interval '2 day', true, now(), false, NULL, '$BLR', now(), now());
SQL
sleep 6
ALL_VEH="$DRIVER/driver/fleet/getAllVehicle?mblimit=20&mboffset=0"
vcities() { curl -s -m 30 "$ALL_VEH$1" -H "token: $TOKEN" | json "','.join(sorted((i.get('cityName') or '-') for i in d['vehicles']))"; }
check "active vehicles: every city by default" "$(vcities "&mbIsActive=true")" "Chennai"
check "active vehicles: narrowed to a city that has none" "$(vcities "&mbIsActive=true&cities=Bangalore")" ""
check "active vehicles: narrowed to a city that has one" "$(vcities "&mbIsActive=true&cities=Chennai")" "Chennai"
check "inactive vehicles: every city by default" "$(vcities "&mbIsActive=false")" "Bangalore"
check "inactive vehicles: narrowed to a city that has none" "$(vcities "&mbIsActive=false&cities=Chennai")" ""
check "inactive vehicles: narrowed to a city that has one" "$(vcities "&mbIsActive=false&cities=Bangalore")" "Bangalore"
psql -d atlas_dev -tA >/dev/null -c "DELETE FROM atlas_driver_offer_bpp.driver_rc_association WHERE id LIKE '$ASSOC_TAG-%'"

# The counts above the list follow the same scope: all the fleets selected, the cities the request names.
SUMMARY="$DRIVER/driver/fleet/%22VEHICLE_STATUS%22/statusSummary"
check "select both fleets for the counts" "$(status -X POST "$SEL_URL?enable=true&fleetOwnerId=$FLEET_B" -H "token: $TOKEN")" 200
check "the counts need no fleet named when several are selected (they were refused: choose a fleet first)" "$(status "$SUMMARY" -H "token: $TOKEN")" 200
check "the counts take one fleet the caller holds" "$(status "$SUMMARY?fleetOwnerId=$FLEET_B" -H "token: $TOKEN")" 200
check "the counts refuse a fleet the caller does not hold" "$(status "$SUMMARY?fleetOwnerId=00000000-0000-4000-8000-000000000000" -H "token: $TOKEN")" 403
check "the counts take known cities" "$(status "$SUMMARY?cities=Bangalore,Chennai" -H "token: $TOKEN")" 200
check "the counts refuse a city the merchant does not operate in" "$(status "$SUMMARY?cities=Atlantis" -H "token: $TOKEN")" 403
# In ENFORCE the fleet picker and a named fleet come from the role assignments alone. Fleets made the new
# way have no fleet_member_association row, so these pass only if that table is not read.
check "the fleets have no member association rows" "$(psql -d atlas_dev -tAc "select count(*) from atlas_driver_offer_bpp.fleet_member_association where fleet_owner_id in ('$FLEET_A','$FLEET_B') or fleet_member_id in ('$FLEET_A','$FLEET_B')")" 0
# A stale member row pointing at someone else must not leak into the picker: the old list would show them.
STALE=$(psql -d atlas_dev -tAc "select id from atlas_driver_offer_bpp.person where id not in ('$FLEET_A','$FLEET_B') limit 1")
psql -d atlas_dev -tA >/dev/null -c "insert into atlas_driver_offer_bpp.fleet_member_association (fleet_member_id, fleet_owner_id, enabled, is_fleet_owner) values ('$FLEET_A', '$STALE', true, true)"
check "the picker lists the fleets held by assignment, not a stale member association" "$(curl -s -m 20 "$DRIVER/driver/fleet/access/list" -H "token: $TOKEN" | json "','.join(sorted(o['fleetOwnerId'] for o in d['ownersList']))")" "$(printf '%s\n%s\n' "$FLEET_A" "$FLEET_B" | sort | paste -sd, -)"
psql -d atlas_dev -tA >/dev/null -c "delete from atlas_driver_offer_bpp.fleet_member_association where fleet_member_id='$FLEET_A'"
check "a list can name the owner's second fleet without a member association" "$(status "$LIST&fleetOwnerId=$FLEET_B" -H "token: $TOKEN")" 200
# The routes that used to take the dashboard person as "the" fleet now take the fleets from the scope.
error_of() { curl -s -m 30 "$1" -H "token: $TOKEN" | json "d.get('errorCode', 'none')"; }
post_error_of() { curl -s -m 30 -X POST "$1" -H "token: $TOKEN" -H 'Content-Type: application/json' -d "$2" | json "d.get('errorCode', 'none') + ' ' + str(d.get('errorMessage', ''))"; }
check "the plan of the owner's second fleet is not refused" "$([ "$(error_of "$DRIVER/plan/$FLEET_B/list")" != ACCESS_DENIED ] && echo yes || echo no)" yes
check "the plan of a subject in no fleet is refused" "$(error_of "$DRIVER/plan/00000000-0000-4000-8000-000000000000/list")" ACCESS_DENIED
check "the bookings of several fleets are listed" "$(status "$DRIVER/driver/fleet/bookings?limit=5&offset=0" -H "token: $TOKEN")" 200
check "the assignments of several fleets are listed" "$(status "$DRIVER/driver/fleet/assignments?limit=5&offset=0" -H "token: $TOKEN")" 200
check "the driver requests of several fleets are listed" "$(status "$DRIVER/driver/fleet/getDriverRequests?mbLimit=5&mbOffset=0" -H "token: $TOKEN")" 200
# Rows for each fleet, so that a list over several fleets can be told from one over a single fleet.
MERCHANT_DB_ID=$(psql -d atlas_dev -tAc "select id from atlas_driver_offer_bpp.merchant where short_id='$MERCHANT'")
CITY_DB_ID=$(psql -d atlas_dev -tAc "select id from atlas_driver_offer_bpp.merchant_operating_city where merchant_short_id='$MERCHANT' and city='Bangalore'")
RUN_TAG="extras-$$"
for F in "$FLEET_A" "$FLEET_B"; do
  psql -d atlas_dev -tA -v ON_ERROR_STOP=1 >/dev/null <<SQL
INSERT INTO atlas_driver_offer_bpp.fleet_booking_information (id, booking_id, fleet_owner_id, merchant_id, merchant_operating_city_id, vehicle_no, place_name, service_name, status, created_at, updated_at, visit_date)
VALUES (gen_random_uuid()::text, '$RUN_TAG-bk-$F', '$F', '$MERCHANT_DB_ID', '$CITY_DB_ID', 'KA01$RUN_TAG', 'Place', 'Service', 'CONFIRMED', now(), now(), current_date);
INSERT INTO atlas_driver_offer_bpp.fleet_booking_assignments (id, booking_id, fleet_owner_id, main_assignment_id, vehicle_no, merchant_id, merchant_operating_city_id, created_at, updated_at, visit_date)
VALUES (gen_random_uuid()::text, '$RUN_TAG-as-$F', '$F', gen_random_uuid()::text, 'KA01$RUN_TAG', '$MERCHANT_DB_ID', '$CITY_DB_ID', now(), now(), current_date);
INSERT INTO atlas_driver_offer_bpp.fleet_badge (id, badge_name, fleet_owner_id, merchant_id, merchant_operating_city_id, badge_type, created_at, updated_at)
VALUES (gen_random_uuid()::text, '$RUN_TAG-badge-$F', '$F', '$MERCHANT_DB_ID', '$CITY_DB_ID', 'DRIVER', now(), now());
SQL
done
TRIP_DRIVER=$(python3 -c 'import uuid; print(uuid.uuid4())')
for F in "$FLEET_A" "$FLEET_B"; do
  psql -d atlas_dev -tA -v ON_ERROR_STOP=1 >/dev/null <<SQL
INSERT INTO atlas_driver_offer_bpp.trip_transaction (id, driver_id, fleet_owner_id, merchant_id, merchant_operating_city_id, route_code, status, vehicle_number, vehicle_service_tier_type, allow_ending_mid_route, deviation_count, end_stop_code, is_currently_deviated, created_at, updated_at)
VALUES (gen_random_uuid()::text, '$TRIP_DRIVER', '$F', '$MERCHANT_DB_ID', '$CITY_DB_ID', '$RUN_TAG-route-$F', 'COMPLETED', 'KA01$RUN_TAG', 'SEDAN', false, 0, 'STOP', false, now(), now());
SQL
done
sleep 5 # the assignments are read from the replica
SORTED_FLEETS="$(printf '%s\n%s\n' "$FLEET_A" "$FLEET_B" | sort | paste -sd, -)"
check "the bookings listed are those of both fleets" "$(curl -s -m 30 "$DRIVER/driver/fleet/bookings?limit=50&offset=0" -H "token: $TOKEN" | json "','.join(sorted(set(b['fleetOwnerId'] for b in d['bookings'] if '$RUN_TAG' in (b.get('bookingId') or ''))))")" "$SORTED_FLEETS"
check "the assignments listed are those of both fleets" "$(curl -s -m 30 "$DRIVER/driver/fleet/assignments?limit=50&offset=0" -H "token: $TOKEN" | json "','.join(sorted(set(b['fleetOwnerId'] for b in d['bookings'] if '$RUN_TAG' in (b.get('bookingId') or ''))))")" "$SORTED_FLEETS"
badges_of() { curl -s -m 30 "$DRIVER/driver/fleet/getAllBadge?mblimit=50&mboffset=0&mbFleetOwnerId=$1" -H "token: $TOKEN" | grep -o "$RUN_TAG-badge-[0-9a-f-]*" | sort -u | paste -sd, -; }
check "the badges of the second fleet are listed when it is named" "$(badges_of "$FLEET_B")" "$RUN_TAG-badge-$FLEET_B"
check "the badges of the first fleet are listed when it is named" "$(badges_of "$FLEET_A")" "$RUN_TAG-badge-$FLEET_A"
# The active and inactive lists come from the badge's driver associations; once enforced they hold only the fleet named.
for F in "$FLEET_A" "$FLEET_B"; do
  for ACTIVE in true false; do
    psql -d atlas_dev -tA -v ON_ERROR_STOP=1 >/dev/null <<SQL
INSERT INTO atlas_driver_offer_bpp.fleet_badge_association (id, badge_id, driver_id, fleet_owner_id, is_active, badge_type, associated_on, associated_till, created_at, updated_at)
SELECT gen_random_uuid()::text, id, gen_random_uuid()::text, '$F', $ACTIVE, 'DRIVER', now(), now() + interval '1 day', now(), now()
FROM atlas_driver_offer_bpp.fleet_badge WHERE badge_name='$RUN_TAG-badge-$F';
SQL
  done
done
sleep 6
flagged_badges_of() { curl -s -m 30 "$DRIVER/driver/fleet/getAllBadge?mblimit=50&mboffset=0&mbIsActive=$2&mbFleetOwnerId=$1" -H "token: $TOKEN" | grep -o "$RUN_TAG-badge-[0-9a-f-]*" | sort -u | paste -sd, -; }
check "the active badges of a named fleet are only its own" "$(flagged_badges_of "$FLEET_A" true)" "$RUN_TAG-badge-$FLEET_A"
check "the inactive list of a named fleet holds none of another fleet's badges" "$(flagged_badges_of "$FLEET_B" false | grep -c "$FLEET_A")" 0
check "the badges of a fleet the caller does not hold are refused" "$(status "$DRIVER/driver/fleet/getAllBadge?mblimit=50&mboffset=0&mbFleetOwnerId=00000000-0000-4000-8000-000000000000" -H "token: $TOKEN")" 403
message_of() { curl -s -m 30 "$1" -H "token: $TOKEN" | json "d.get('errorMessage', 'none')"; }
trips_of() { curl -s -m 30 "$DRIVER/driver/fleet/trip/transactions/$TRIP_DRIVER?fleetOwnerId=$1&limit=50&offset=0" -H "token: $TOKEN" | grep -o "$RUN_TAG-route-[0-9a-f-]*" | sort -u | paste -sd, -; }
check "the trips of the second fleet are listed when it is named" "$(trips_of "$FLEET_B")" "$RUN_TAG-route-$FLEET_B"
check "the trips of the first fleet are listed when it is named" "$(trips_of "$FLEET_A")" "$RUN_TAG-route-$FLEET_A"
check "the trips of a fleet the caller does not hold are refused" "$(status "$DRIVER/driver/fleet/trip/transactions/$TRIP_DRIVER?fleetOwnerId=00000000-0000-4000-8000-000000000000&limit=50&offset=0" -H "token: $TOKEN")" 403
check "trips: with several fleets selected, one has to be named" "$(message_of "$DRIVER/driver/fleet/trip/transactions/$TRIP_DRIVER?limit=50&offset=0")" "Choose a fleet first"
psql -d atlas_dev -tA >/dev/null <<SQL
DELETE FROM atlas_driver_offer_bpp.fleet_booking_information WHERE booking_id LIKE '$RUN_TAG-%';
DELETE FROM atlas_driver_offer_bpp.fleet_booking_assignments WHERE booking_id LIKE '$RUN_TAG-%';
DELETE FROM atlas_driver_offer_bpp.fleet_badge_association WHERE badge_id IN (SELECT id FROM atlas_driver_offer_bpp.fleet_badge WHERE badge_name LIKE '$RUN_TAG-%');
DELETE FROM atlas_driver_offer_bpp.fleet_badge WHERE badge_name LIKE '$RUN_TAG-%';
DELETE FROM atlas_driver_offer_bpp.trip_transaction WHERE route_code LIKE '$RUN_TAG-%';
SQL
# A fleet owner's ride list needs a date range.
RIDES="$DRIVER/ride/list?limit=5&offset=0&from=2026-10-01T00:00:00Z&to=2026-10-08T00:00:00Z"
message_of() { curl -s -m 30 "$1" -H "token: $TOKEN" | json "d.get('errorMessage', 'none')"; }
check "a ride list for several fleets needs one named" "$(message_of "$RIDES")" "Choose a fleet first"
check "a ride list can name the owner's second fleet" "$(status "$RIDES&fleetOwnerId=$FLEET_B" -H "token: $TOKEN")" 200
check "a ride list refuses a fleet the caller does not hold" "$(status "$RIDES&fleetOwnerId=00000000-0000-4000-8000-000000000000" -H "token: $TOKEN")" 403
check "the payout account takes the owner's second fleet (past the access check)" "$(post_error_of "$DRIVER/fleet/payout/account" "{\"accountType\":\"BANK\",\"fleetOwnerId\":\"$FLEET_B\"}" | grep -c -i "Invalid fleet owner")" 0
check "the payout account refuses a fleet the caller does not hold" "$(status -X POST "$DRIVER/fleet/payout/account" -H "token: $TOKEN" -H 'Content-Type: application/json' -d '{"accountType":"BANK","fleetOwnerId":"00000000-0000-4000-8000-000000000000"}')" 403
check "a cash-ride update can name the owner's second fleet (not refused)" "$([ "$(post_error_of "$DRIVER/driver/fleet/cashRideUpdate?fleetOwnerId=$FLEET_B" '{"enableCashRide":true,"targetIds":[]}' | cut -d' ' -f1)" != ACCESS_DENIED ] && echo yes || echo no)" yes
# Routes that read or change one driver, or a fleet, named by id; and the notification lookup.
# A driver is a person plus its information and stats rows, copied from an existing driver.
mkdriver() {
  local src; src=$(psql -d atlas_dev -tAc "select p.id from atlas_driver_offer_bpp.person p join atlas_driver_offer_bpp.driver_information i on i.driver_id = p.id join atlas_driver_offer_bpp.merchant_operating_city c on c.id = p.merchant_operating_city_id where p.role='DRIVER' and c.merchant_short_id='$MERCHANT' and c.city='Bangalore' order by p.id limit 1")
  psql -d atlas_dev -tA -v ON_ERROR_STOP=1 >/dev/null <<SQL
INSERT INTO atlas_driver_offer_bpp.person SELECT r.* FROM (SELECT * FROM atlas_driver_offer_bpp.person WHERE id='$src') p
  CROSS JOIN LATERAL jsonb_populate_record(null::atlas_driver_offer_bpp.person,
    to_jsonb(p) || jsonb_build_object('id','$1','mobile_number_hash','\\x' || md5(random()::text),'created_at', to_char(now(), 'YYYY-MM-DD"T"HH24:MI:SS"Z"'))) r;
INSERT INTO atlas_driver_offer_bpp.driver_information SELECT r.* FROM (SELECT * FROM atlas_driver_offer_bpp.driver_information WHERE driver_id='$src') d
  CROSS JOIN LATERAL jsonb_populate_record(null::atlas_driver_offer_bpp.driver_information, to_jsonb(d) || jsonb_build_object('driver_id','$1')) r;
INSERT INTO atlas_driver_offer_bpp.driver_stats SELECT r.* FROM (SELECT * FROM atlas_driver_offer_bpp.driver_stats WHERE driver_id='$src' LIMIT 1) d
  CROSS JOIN LATERAL jsonb_populate_record(null::atlas_driver_offer_bpp.driver_stats, to_jsonb(d) || jsonb_build_object('driver_id','$1')) r;
SQL
}
DRV_B=$(python3 -c 'import uuid; print(uuid.uuid4())'); DRV_NONE=$(python3 -c 'import uuid; print(uuid.uuid4())')
mkdriver "$DRV_B"; mkdriver "$DRV_NONE"
psql -d atlas_dev -tA -v ON_ERROR_STOP=1 >/dev/null -c "INSERT INTO atlas_driver_offer_bpp.fleet_driver_association (id, driver_id, fleet_owner_id, is_active, associated_on, associated_till) VALUES (gen_random_uuid()::text, '$DRV_B', '$FLEET_B', true, now(), now() + interval '1 year')"
sleep 6 # the driver lookups read the replica
not_denied() { [ "$1" != ACCESS_DENIED ] && echo yes || echo no; }
# Past the scope check: neither refused nor "no such person" (which would mean the lookup never ran).
past_scope() { [ "$1" != ACCESS_DENIED ] && [ "$1" != PERSON_DOES_NOT_EXIST ] && echo yes || echo no; }
ok200() { local out code; out=$(curl -s -m 30 -w '\n%{http_code}' "$1" -H "token: $TOKEN"); code=${out##*$'\n'}
  if [ "$code" = 200 ]; then echo yes; else echo no; echo "  DEBUG $1 -> $code ${out%$'\n'*}" | cut -c1-300 >&2; fi; }
check "driver info: a driver of the second fleet is not refused" "$(ok200 "$DRIVER/driver/info?personId=$DRV_B")" yes
check "driver info: a driver in no fleet is refused" "$(error_of "$DRIVER/driver/info?personId=$DRV_NONE")" ACCESS_DENIED
check "stats: a driver of the second fleet is not refused" "$(ok200 "$DRIVER/driver/stats?entityId=$DRV_B")" yes
check "stats: the second fleet itself is not refused" "$(past_scope "$(error_of "$DRIVER/driver/stats?entityId=$FLEET_B")")" yes
check "stats: a driver in no fleet is refused" "$(error_of "$DRIVER/driver/stats?entityId=$DRV_NONE")" ACCESS_DENIED
check "stats: with several fleets and none named, one has to be named" "$(message_of "$DRIVER/driver/stats")" "Choose a fleet first"
check "FY earnings: a driver of the second fleet is not refused" "$(ok200 "$DRIVER/driver/fyEarnings?financialYear=2026&entityId=$DRV_B")" yes
check "FY earnings: a driver in no fleet is refused" "$(error_of "$DRIVER/driver/fyEarnings?financialYear=2026&entityId=$DRV_NONE")" ACCESS_DENIED
check "identity info: a driver of the second fleet is not refused" "$(ok200 "$DRIVER/driver/$DRV_B/identityInfo")" yes
check "identity info: a driver in no fleet is refused" "$(error_of "$DRIVER/driver/$DRV_NONE/identityInfo")" ACCESS_DENIED
CHANGE='{"changeType":"ChangeDriverOperator","newOperatorCode":"no-such-code"}'
check "association change: a driver of the second fleet is not refused" "$(past_scope "$(post_error_of "$DRIVER/driver/$DRV_B/association/change" "$CHANGE" | cut -d' ' -f1)")" yes
check "association change: a driver in no fleet is refused" "$(post_error_of "$DRIVER/driver/$DRV_NONE/association/change" "$CHANGE" | cut -d' ' -f1)" ACCESS_DENIED
check "clear fee: a driver in no fleet is refused" "$(post_error_of "$DRIVER/driver/clearFee?driverId=$DRV_NONE" '{"serviceName":"YATRI_SUBSCRIPTION","feeType":"PAYOUT_REGISTRATION","platformFee":0,"sendManualLink":false}' | cut -d' ' -f1)" ACCESS_DENIED
check "refund by payout: a driver in no fleet is refused" "$(post_error_of "$DRIVER/driver/$DRV_NONE/refundByPayout" '{"serviceName":"YATRI_SUBSCRIPTION","refundAmountDeduction":0,"driverFeeType":"PAYOUT_REGISTRATION"}' | cut -d' ' -f1)" ACCESS_DENIED
check "permanent delete: a driver in no fleet is refused" "$(curl -s -m 30 -X DELETE "$DRIVER/driver/$DRV_NONE/permanentlyDelete" -H "token: $TOKEN" | json "d.get('errorCode', 'none')")" ACCESS_DENIED
check "invoices: the second fleet is not refused" "$(ok200 "$DRIVER/financeManagement/finance/invoice/list?fleetOwnerOrDriverId=$FLEET_B")" yes
check "invoices: a subject in no fleet is refused" "$(error_of "$DRIVER/financeManagement/finance/invoice/list?fleetOwnerOrDriverId=00000000-0000-4000-8000-000000000000")" ACCESS_DENIED
check "invoices: with several fleets and none named, one has to be named" "$(message_of "$DRIVER/financeManagement/finance/invoice/list")" "Choose a fleet first"
check "TDS status: with several fleets selected, one has to be named" "$(message_of "$DRIVER/financeManagement/tdsReimbursement/status?quarter=%22Q1%22&assessmentYear=2026-27")" "Choose a fleet first"
INTERNAL="http://localhost:8016/direct-dashboard/internal/auth"
lookup_ids() { curl -s -m 30 "$INTERNAL" -H "api-key: ae288466-2add-11ee-be56-0242ac120002" -H "token: $TOKEN" | json "','.join(sorted(d['driverIds']))"; }
check "the notification lookup returns both selected fleets" "$(lookup_ids)" "$SORTED_FLEETS"
check "select fleet A only again" "$(status -X POST "$SEL_URL?enable=true&onlyCurrent=true&fleetOwnerId=$FLEET_A" -H "token: $TOKEN")" 200
check "the counts refuse a fleet outside the selection" "$(status "$SUMMARY?fleetOwnerId=$FLEET_B" -H "token: $TOKEN")" 403
check "the notification lookup follows the selection" "$(lookup_ids)" "$FLEET_A"
# Both fleets are selected again for the ride checks (the selection above was narrowed to fleet A).
status -X POST "$SEL_URL?enable=true&fleetOwnerId=$FLEET_B" -H "token: $TOKEN" >/dev/null
# The fleet's contact: read and set on the fleet the request names (when it is in scope), and the person's
# own roles by scope.
CONTACT="$DRIVER/fleet/v2/contact"
contact_field() { curl -s -m 30 "$CONTACT?fleetOwnerId=$FLEET_B" -H "token: $TOKEN" | json "d.get('$1', d.get('errorCode','none'))"; }
check "contact: the second fleet's contact is readable" "$(status "$CONTACT?fleetOwnerId=$FLEET_B" -H "token: $TOKEN")" 200
check "contact: a fleet the caller does not hold is refused" "$(status "$CONTACT?fleetOwnerId=00000000-0000-4000-8000-000000000000" -H "token: $TOKEN")" 403
check "contact: with several fleets selected, one has to be named" "$(curl -s -m 30 "$CONTACT" -H "token: $TOKEN" | json "d.get('errorMessage','none')")" "Choose a fleet first"
set_contact() { curl -s -m 30 -X POST "$CONTACT" -H "token: $TOKEN" -H 'Content-Type: application/json' -d "$1" | json "d.get('errorCode', d.get('result','none'))"; }
check "contact: it is set on the named fleet" "$(set_contact "{\"fleetOwnerId\":\"$FLEET_B\",\"contactEmail\":\"ops-$$@example.com\",\"contactMobileCountryCode\":\"+91\",\"contactMobileNumber\":\"9000000$(printf '%03d' $((RANDOM % 1000)))\"}")" "Success"
sleep 4
check "contact: the new e-mail is read back" "$(contact_field contactEmail)" "ops-$$@example.com"
check "contact: and it is marked as the fleet's own" "$(contact_field contactSetOnFleet)" "True"
check "contact: a phone without its country code is refused" "$(set_contact "{\"fleetOwnerId\":\"$FLEET_B\",\"contactMobileNumber\":\"9000000001\"}")" "INVALID_REQUEST"
check "contact: setting one on a fleet the caller does not hold is refused" "$(set_contact '{"fleetOwnerId":"00000000-0000-4000-8000-000000000000","contactEmail":"x@example.com"}')" "ACCESS_DENIED"
check "contact: a fleet whose owner has no phone cannot be left without one" "$(set_contact "{\"fleetOwnerId\":\"$FLEET_B\"}")" "INVALID_REQUEST"
sleep 4
check "contact: and its contact is left as it was" "$(contact_field contactSetOnFleet)" "True"
# A fleet with no contact of its own is reached on its owner's phone. The backfill must not copy that phone onto
# the fleet, or a later change of the owner's phone would not reach it.
OLD_PHONE=$(mobile)
OLD_TOKEN=$(login_phone "$OLD_PHONE")
OLD_FLEET=$(person_of "$OLD_TOKEN")
sleep 8
old_contact() { curl -s -m 30 "$CONTACT?fleetOwnerId=$OLD_FLEET" -H "token: $OLD_TOKEN" | json "d.get('$1', d.get('errorCode','none'))"; }
check "contact backfill: an old fleet has no contact of its own to begin with" "$(old_contact contactSetOnFleet)" "False"
BACKFILL="http://localhost:8016/internal/$MERCHANT/fleetScopeBackfill"
INTERNAL_KEY=$(psql -d atlas_dev -tAc "select internal_api_key from atlas_driver_offer_bpp.merchant where short_id='$MERCHANT'")
backfill() { curl -s -m 120 -X POST "$BACKFILL?dryRun=$1" -H "token: $INTERNAL_KEY" | json "d.get('$2', d.get('errorCode','none'))"; }
check "contact backfill: the real run succeeds" "$(backfill false dryRun)" "False"
sleep 8
check "contact backfill: and copies no contact onto the fleet" "$(old_contact contactSetOnFleet)" "False"
check "contact backfill: the fleet is still reached on the owner's phone" "$(old_contact contactMobileNumber)" "$OLD_PHONE"
old_set_contact() { curl -s -m 30 -X POST "$CONTACT" -H "token: $OLD_TOKEN" -H 'Content-Type: application/json' -d "{\"fleetOwnerId\":\"$OLD_FLEET\"}" | json "d.get('errorCode', d.get('result','none'))"; }
check "contact: a fleet whose owner has a phone can be put back on it" "$(old_set_contact)" "Success"
sleep 8
check "contact: and is then on the owner's phone" "$(old_contact contactSetOnFleet)" "False"
OWN_ID=$(person_of "$TOKEN")
asg_status() { status "$DASH/user/assignments/$1" -H "token: $2"; }
check "assignments: a person reads their own roles by scope" "$(asg_status "$OWN_ID" "$TOKEN")" 200
check "assignments: and sees their fleet roles" "$(curl -s -m 20 "$DASH/user/assignments/$OWN_ID" -H "token: $TOKEN" | json "'FLEET' in {a['scopeType'] for a in d}")" "True"
check "assignments: a fleet owner cannot read another person's" "$(asg_status 00000000-0000-4000-8000-000000000000 "$TOKEN")" 403
check "assignments: an admin can read a person's" "$(asg_status "$OWN_ID" "$ADMIN_TOKEN")" 200
# Rides in two cities. A ride needs its booking, fare parameters and location mappings to load.
MID=$(psql -d atlas_dev -tAc "select id from atlas_driver_offer_bpp.merchant where short_id='$MERCHANT'")
mkride() { # mkride <fleet owner id, or empty> <city id>: prints the ride id
  local rid bid lid fo="NULL"
  rid=$(python3 -c 'import uuid;print(uuid.uuid4())'); bid=$(python3 -c 'import uuid;print(uuid.uuid4())'); lid=$(python3 -c 'import uuid;print(uuid.uuid4())')
  [ -n "$1" ] && fo="'$1'"
  psql -d atlas_dev -tA -v ON_ERROR_STOP=1 >/dev/null <<SQL
INSERT INTO atlas_driver_offer_bpp.location (id, lat, lon, merchant_id, merchant_operating_city_id) VALUES ('$lid', 12.9, 77.6, '$MID', '$2');
INSERT INTO atlas_driver_offer_bpp.location_mapping (id, tag, location_id, entity_id, "order", version, merchant_id, merchant_operating_city_id)
  VALUES (gen_random_uuid()::text, 'RIDE', '$lid', '$rid', 0, 'LATEST', '$MID', '$2'), (gen_random_uuid()::text, 'BOOKING', '$lid', '$bid', 0, 'LATEST', '$MID', '$2');
INSERT INTO atlas_driver_offer_bpp.fare_parameters (id, base_fare, fare_parameters_type) VALUES ('$bid', 100, 'Progressive');
INSERT INTO atlas_driver_offer_bpp.fare_parameters_progressive_details (fare_parameters_id, dead_km_fare) VALUES ('$bid', 0);
INSERT INTO atlas_driver_offer_bpp.rider_details (id, merchant_id, mobile_country_code, mobile_number_encrypted, mobile_number_hash, cancellation_dues, dispute_chances_used, has_taken_valid_ride, night_safety_checks)
  SELECT '$bid', '$MID', '+91', mobile_number_encrypted, decode(md5('$bid'),'hex'), 0, 0, true, false FROM atlas_driver_offer_bpp.person WHERE mobile_number_encrypted IS NOT NULL LIMIT 1;
INSERT INTO atlas_driver_offer_bpp.booking (id, bap_id, bap_uri, estimated_fare, fare_parameters_id, primary_exophone, provider_id, quote_id, start_time, status, transaction_id, vehicle_variant, merchant_operating_city_id, rider_id)
  VALUES ('$bid','b','http://b',100,'$bid','0','$MID','$bid', now(),'COMPLETED','$bid','SEDAN','$2','$bid');
INSERT INTO atlas_driver_offer_bpp.ride_details (id, driver_name, vehicle_number) VALUES ('$rid', 'Seeded driver', 'RIDE-TEST-1');
INSERT INTO atlas_driver_offer_bpp.ride (id, booking_id, driver_id, otp, short_id, status, tracking_url, fleet_owner_id, merchant_id, merchant_operating_city_id)
  VALUES ('$rid', '$bid', '$DRV_B', '1234', substr('$rid',1,30), 'COMPLETED', 'x', $fo, '$MID', '$2');
SQL
  echo "$rid"
}
R_BLR=$(mkride "$FLEET_B" "$BLR"); R_CHN=$(mkride "$FLEET_B" "$CHN"); R_CHN_NONE=$(mkride "" "$CHN")
sleep 3
RIDE_WINDOW="limit=50&offset=0&from=2026-10-01T00:00:00Z&to=2099-01-01T00:00:00Z&fleetOwnerId=$FLEET_B"
rides_of() { curl -s -m 30 "$DRIVER/ride/list?$RIDE_WINDOW$1" -H "token: $TOKEN" | json "','.join(sorted(r['rideId'] for r in d['rides'] if r['rideId'] in ['$R_BLR','$R_CHN','$R_CHN_NONE']))"; }
SORTED_BOTH=$(printf '%s\n' "$R_BLR" "$R_CHN" | sort | paste -sd, -)
check "rides: with no city named, the fleet's rides in every city are listed" "$(rides_of "")" "$SORTED_BOTH"
check "rides: one city can be asked for" "$(rides_of "&cities=Bangalore")" "$R_BLR"
check "rides: another city can be asked for" "$(rides_of "&cities=Chennai")" "$R_CHN"
check "rides: several cities can be asked for" "$(rides_of "&cities=Bangalore,Chennai")" "$SORTED_BOTH"
check "rides: each row says which city the ride was in" "$(curl -s -m 30 "$DRIVER/ride/list?$RIDE_WINDOW&cities=Bangalore,Chennai" -H "token: $TOKEN" | json "','.join(sorted(r['cityName'] for r in d['rides'] if r['rideId'] in ['$R_BLR','$R_CHN']))")" "Bangalore,Chennai"
check "rides: and carry the city id" "$(curl -s -m 30 "$DRIVER/ride/list?$RIDE_WINDOW&cities=Chennai" -H "token: $TOKEN" | json "[r['cityId'] for r in d['rides'] if r['rideId']=='$R_CHN'][0]")" "$CHN"
# One session serves every city its person holds: the admin signed in for Bangalore also holds Kolkata, not Chennai.
ADMIN_BASE="${DRIVER%/Bangalore}"
CITY_PROBE="ride/list?limit=1&offset=0&rideShortId=no-such-ride"
check "cities: a session gets the same answer in another city its person holds as in the one it was issued for" "$(status "$ADMIN_BASE/Kolkata/$CITY_PROBE" -H "token: $ADMIN_TOKEN")" "$(status "$ADMIN_BASE/Bangalore/$CITY_PROBE" -H "token: $ADMIN_TOKEN")"
check "cities: and is refused in a city its person does not hold" "$(status "$ADMIN_BASE/Chennai/$CITY_PROBE" -H "token: $ADMIN_TOKEN")" 403
SCHED="$DRIVER/driver/fleet/scheduledBooking/list?limit=5&offset=0&from=2026-10-13&to=2026-10-14"
check "scheduled bookings: the city in the URL is listed by default" "$(status "$SCHED" -H "token: $TOKEN")" 200
check "rides: a ride of no fleet is not listed" "$(rides_of "&cities=Chennai" | grep -c "$R_CHN_NONE")" 0
check "rides: a city the merchant does not operate in is refused" "$(status "$DRIVER/ride/list?$RIDE_WINDOW&cities=Atlantis" -H "token: $TOKEN")" 403
rides_v2_of() { curl -s -m 30 "$DRIVER/ride/listV2?$RIDE_WINDOW$1" -H "token: $TOKEN" | json "','.join(sorted(r['rideId'] for r in d['rides'] if r['rideId'] in ['$R_BLR','$R_CHN','$R_CHN_NONE']))"; }
check "ridesV2: with no city named, the fleet's rides in every city are listed" "$(rides_v2_of "")" "$SORTED_BOTH"
check "ridesV2: one city can be asked for" "$(rides_v2_of "&cities=Chennai")" "$R_CHN"
check "ridesV2: several cities can be asked for" "$(rides_v2_of "&cities=Bangalore,Chennai")" "$SORTED_BOTH"
check "ridesV2: a ride of no fleet is not listed" "$(rides_v2_of "&cities=Chennai" | grep -c "$R_CHN_NONE")" 0
check "ridesV2: a city the merchant does not operate in is refused" "$(status "$DRIVER/ride/listV2?$RIDE_WINDOW&cities=Atlantis" -H "token: $TOKEN")" 403
ride_info() { curl -s -m 30 "$DRIVER/ride/$1/info" -H "token: $TOKEN" | json "d.get('errorCode','none')"; }
check "ride info: a ride of the owner's fleet in another city is opened (its own city, not the URL's)" "$(ride_info "$R_CHN")" none
check "ride info: a ride of no fleet is refused" "$(ride_info "$R_CHN_NONE")" ACCESS_DENIED
psql -d atlas_dev -tA >/dev/null <<SQL
CREATE TEMP TABLE b_ids AS SELECT booking_id AS id FROM atlas_driver_offer_bpp.ride WHERE id IN ('$R_BLR','$R_CHN','$R_CHN_NONE');
DELETE FROM atlas_driver_offer_bpp.location_mapping WHERE entity_id IN ('$R_BLR','$R_CHN','$R_CHN_NONE') OR entity_id IN (SELECT id FROM b_ids);
DELETE FROM atlas_driver_offer_bpp.ride_details WHERE id IN ('$R_BLR','$R_CHN','$R_CHN_NONE');
DELETE FROM atlas_driver_offer_bpp.ride WHERE id IN ('$R_BLR','$R_CHN','$R_CHN_NONE');
DELETE FROM atlas_driver_offer_bpp.booking WHERE id IN (SELECT id FROM b_ids);
DELETE FROM atlas_driver_offer_bpp.rider_details WHERE id IN (SELECT id FROM b_ids);
DELETE FROM atlas_driver_offer_bpp.fare_parameters_progressive_details WHERE fare_parameters_id IN (SELECT id FROM b_ids);
DELETE FROM atlas_driver_offer_bpp.fare_parameters WHERE id IN (SELECT id FROM b_ids);
SQL
psql -d atlas_dev -tA >/dev/null -c "DELETE FROM atlas_driver_offer_bpp.fleet_driver_association WHERE driver_id IN ('$DRV_B', '$DRV_NONE'); DELETE FROM atlas_driver_offer_bpp.driver_stats WHERE driver_id IN ('$DRV_B', '$DRV_NONE'); DELETE FROM atlas_driver_offer_bpp.driver_information WHERE driver_id IN ('$DRV_B', '$DRV_NONE'); DELETE FROM atlas_driver_offer_bpp.person WHERE id IN ('$DRV_B', '$DRV_NONE')"
# Section 7 checks the same filter outside ENFORCE.
set_mode OFF
revoke_chennai # section 7 starts from an owner without Chennai access

echo "== 7. the cities filter on the proxied list (provider-dashboard -> /dashboard tree), outside ENFORCE"
# Outside ENFORCE the old lists read the fleet member association, as they always have: a fleet without a row is
# refused when it is named. The checks below give the owner the row such a fleet carries.
PDLIST="$DASH/bpp/driver-offer/$MERCHANT/Bangalore/driver/fleet/vehicleAssociation?Limit=10&Offset=0"
listed_pd() { curl -s -m 30 "$PDLIST$1" -H "token: $TOKEN" | json "','.join(sorted((i.get('cityName') or '-') for i in d['listItem']))"; }
listed_direct() { curl -s -m 30 "$LIST$1" -H "token: $TOKEN" | json "','.join(sorted((i.get('cityName') or '-') for i in d['listItem']))"; }
# Outside ENFORCE the cities a request names have to be ones the caller can reach, so the owner, who only has
# access to Bangalore, is refused Chennai on both trees until Chennai access is given.
set_mode OFF
check "OFF: a fleet with no member association is refused by the old list, as before" "$(curl -s -m 30 "$PDLIST&cities=Bangalore" -H "token: $TOKEN" | json "d.get('errorCode','none')")" PERSON_NOT_FOUND
psql -d atlas_dev -tA >/dev/null -c "insert into atlas_driver_offer_bpp.fleet_member_association (fleet_member_id, fleet_owner_id, enabled, is_fleet_owner) values ('$FLEET_A', '$FLEET_A', true, true)"
sleep 6
for MODE in OFF SHADOW; do
  set_mode $MODE
  check "$MODE: a city the caller has no access to is refused (proxied)" "$(status "$PDLIST&cities=Chennai" -H "token: $TOKEN")" 403
  check "$MODE: and on the direct tree" "$(status "$LIST&cities=Chennai" -H "token: $TOKEN")" 403
  check "$MODE: the city they do have is accepted" "$(status "$PDLIST&cities=Bangalore" -H "token: $TOKEN")" 200
done
grant_chennai
for MODE in OFF SHADOW; do
  set_mode $MODE
  check "$MODE: without cities the list covers the city in the URL" "$(listed_pd "")" "Bangalore"
  check "$MODE: cities=Chennai returns only Chennai vehicles" "$(listed_pd "&cities=Chennai")" "Chennai"
  check "$MODE: two cities return both" "$(listed_pd "&cities=Bangalore,Chennai")" "Bangalore,Chennai"
  check "$MODE: a city std code is read like its name" "$(listed_pd "&cities=std:044")" "Chennai"
  check "$MODE: a city the merchant does not operate in is refused" "$(status "$PDLIST&cities=Atlantis" -H "token: $TOKEN")" 403
  check "$MODE: the direct tree reads the same way" "$(listed_direct "&cities=Chennai")" "Chennai"
done
set_mode OFF
psql -d atlas_dev -tA >/dev/null -c "delete from atlas_driver_offer_bpp.fleet_member_association where fleet_member_id='$FLEET_A' and fleet_owner_id='$FLEET_A'"

echo "== 3. sessions end when someone who held only OTP-grade roles is given an admin role (and only then)"
# A dashboard user with a ready-made session, copied from the admin's rows. A fleet owner or operator cannot be
# moved to another role (those roles are synced with the driver-app), so the user starts in a role that is
# marked exempt from two-factor, which is what lets a phone OTP open it. The mark is put back at the end.
USER_ID=$(python3 -c 'import uuid; print(uuid.uuid4())')
USER_TOKEN=$(python3 -c 'import uuid; print(uuid.uuid4())')
ADMIN_ID=$(psql -d atlas_dev -tAc "select person_id from atlas_dashboard.registration_token where token='$ADMIN_TOKEN'")
FIRST_ROLE=$(psql -d atlas_dev -tAc "select id from atlas_dashboard.role where name='CUSTOMER_SERVICE'")
SECOND_ROLE=$(psql -d atlas_dev -tAc "select id from atlas_dashboard.role where name='JUSPAY_OPS'")
THIRD_ROLE=$(psql -d atlas_dev -tAc "select id from atlas_dashboard.role where name='CUSTOMER'")
psql -d atlas_dev -tAc "update atlas_dashboard.role set two_fa_exempt=true where id='$FIRST_ROLE'" >/dev/null
restore_role() { psql -d atlas_dev -tAc "update atlas_dashboard.role set two_fa_exempt=false where id='$FIRST_ROLE'" >/dev/null; }
new_session() { # a session for USER_ID, copied from the admin's
  psql -d atlas_dev -tA >/dev/null <<SQL
DELETE FROM atlas_dashboard.registration_token WHERE token='$USER_TOKEN';
INSERT INTO atlas_dashboard.registration_token SELECT r.* FROM atlas_dashboard.registration_token t
  CROSS JOIN LATERAL jsonb_populate_record(null::atlas_dashboard.registration_token,
    to_jsonb(t) || jsonb_build_object('id', gen_random_uuid()::text, 'token','$USER_TOKEN','person_id','$USER_ID')) r
  WHERE t.token='$ADMIN_TOKEN';
SQL
}
psql -d atlas_dev -tA >/dev/null <<SQL
INSERT INTO atlas_dashboard.person SELECT r.* FROM atlas_dashboard.person p
  CROSS JOIN LATERAL jsonb_populate_record(null::atlas_dashboard.person,
    to_jsonb(p) || jsonb_build_object('id','$USER_ID','role_id','$FIRST_ROLE','email_hash',null,'email_encrypted',null,'admin_tier','USER',
      'mobile_number_hash', '\\x' || md5(random()::text), 'first_name','extras','last_name','user')) r
  WHERE p.id='$ADMIN_ID';
INSERT INTO atlas_dashboard.merchant_access SELECT r.* FROM atlas_dashboard.merchant_access m
  CROSS JOIN LATERAL jsonb_populate_record(null::atlas_dashboard.merchant_access,
    to_jsonb(m) || jsonb_build_object('id', gen_random_uuid()::text, 'person_id','$USER_ID')) r
  WHERE m.person_id='$ADMIN_ID'
    AND (m.merchant_id, m.operating_city) = (SELECT merchant_id, operating_city FROM atlas_dashboard.registration_token WHERE token='$ADMIN_TOKEN');
SQL
new_session
check "the new dashboard user's session works" "$(status "$DASH/user/profile" -H "token: $USER_TOKEN")" 200
check "an admin gives them an admin role" "$(status -X POST "$DASH/admin/person/$USER_ID/assignRole/$SECOND_ROLE" -H "token: $ADMIN_TOKEN")" 200
AFTER=$(status "$DASH/user/profile" -H "token: $USER_TOKEN")
if [ "$AFTER" = 401 ] || [ "$AFTER" = 403 ]; then ok "their old session is refused ($AFTER)"; else bad "their old session is refused (got $AFTER)"; fi
check "their session row is gone" "$(psql -d atlas_dev -tAc "select count(*) from atlas_dashboard.registration_token where token='$USER_TOKEN'")" 0
check "the admin's own session is untouched" "$(status "$DASH/user/profile" -H "token: $ADMIN_TOKEN")" 200
# Someone who already holds an admin role keeps their sessions when moved to another admin role.
new_session
check "an already-admin user has a working session" "$(status "$DASH/user/profile" -H "token: $USER_TOKEN")" 200
check "an admin moves them to another admin role" "$(status -X POST "$DASH/admin/person/$USER_ID/assignRole/$THIRD_ROLE" -H "token: $ADMIN_TOKEN")" 200
check "their session is left alone" "$(status "$DASH/user/profile" -H "token: $USER_TOKEN")" 200
check "their session row is still there" "$(psql -d atlas_dev -tAc "select count(*) from atlas_dashboard.registration_token where token='$USER_TOKEN'")" 1
restore_role
psql -d atlas_dev -tA >/dev/null <<SQL
DELETE FROM atlas_dashboard.person_role_assignment WHERE person_id='$USER_ID';
DELETE FROM atlas_dashboard.registration_token WHERE person_id='$USER_ID';
DELETE FROM atlas_dashboard.merchant_access WHERE person_id='$USER_ID';
DELETE FROM atlas_dashboard.person WHERE id='$USER_ID';
SQL

echo "== 5. operator link: the consent OTP goes to the fleet's own contact phone, by fleet id"
OPERATOR_ROLE=$(psql -d atlas_dev -tAc "select id from atlas_dashboard.role where name='OPERATOR'")
OP_MOBILE=$(mobile)
OP_EMAIL="extras-op-$OP_MOBILE@dashboard.com"
check "an admin creates an operator" "$(status -X POST "$DRIVER/operator/dashboard/register" -H "token: $ADMIN_TOKEN" -H 'Content-Type: application/json' \
  -d "{\"firstName\":\"Extras\",\"lastName\":\"Operator\",\"roleId\":\"$OPERATOR_ROLE\",\"email\":\"$OP_EMAIL\",\"mobileNumber\":\"$OP_MOBILE\",\"mobileCountryCode\":\"+91\",\"password\":\"Extras@12345\",\"fleetAssignment\":null}")" 200
sleep 12
OP_TOKEN=$(curl -s -m 30 -X POST "$DASH/user/login" -H 'Content-Type: application/json' -d "{\"email\":\"$OP_EMAIL\",\"password\":\"Extras@12345\"}" | json "d['authToken']")
[ -n "$OP_TOKEN" ] && ok "the operator logs in" || bad "the operator logs in"
OWNER_PHONE=$(mobile)
OWNER_TOKEN=$(login_phone "$OWNER_PHONE")
CONTACT=$(mobile)
CONSENT_FLEET=$(curl -s -m 20 -X POST "$DRIVER/fleet/v2/create" -H "token: $OWNER_TOKEN" -H 'Content-Type: application/json' \
  -d "{\"fleetName\":\"Consent fleet\",\"fleetType\":\"NORMAL_FLEET\",\"contactMobileCountryCode\":\"+91\",\"contactMobileNumber\":\"$CONTACT\",\"contactEmail\":\"consent@example.com\"}" | json "d['fleetId']")
sleep 12
PERSONS_BEFORE=$(psql -d atlas_dev -tAc "select count(*) from atlas_dashboard.person")
check "the operator sends the consent OTP by fleet id alone" "$(status -X POST "$DRIVER/operator/fleet/link/sendOtp" -H "token: $OP_TOKEN" -H 'Content-Type: application/json' -d "{\"mobileNumber\":\"\",\"mobileCountryCode\":\"\",\"fleetId\":\"$CONSENT_FLEET\"}")" 200
check "the OTP is kept under the fleet's contact phone" "$(redis get "${REDIS_PREFIX}Fleet:Link:PhoneNo:+91$CONTACT")" '"'$OTP'"'
check "and not under the owner's own phone" "$(redis get "${REDIS_PREFIX}Fleet:Link:PhoneNo:+91$OWNER_PHONE")" ""
check "no dashboard person was made up for the fleet" "$(psql -d atlas_dev -tAc "select count(*) from atlas_dashboard.person where id='$CONSENT_FLEET'")" 0
check "no dashboard person was created at all" "$(psql -d atlas_dev -tAc "select count(*) from atlas_dashboard.person")" "$PERSONS_BEFORE"
check "the operator verifies the code and links the fleet" "$(status -X POST "$DRIVER/operator/fleet/link/verifyOtp" -H "token: $OP_TOKEN" -H 'Content-Type: application/json' -d "{\"fleetOwnerId\":\"$CONSENT_FLEET\",\"otp\":\"$OTP\"}")" 200
LINKS=0
for _ in $(seq 1 12); do # the link is written through Redis; wait for the drainer to copy it
  LINKS=$(psql -d atlas_dev -tAc "select count(*) from atlas_driver_offer_bpp.fleet_operator_association where fleet_owner_id='$CONSENT_FLEET'")
  [ "$LINKS" = 1 ] && break
  sleep 5
done
check "the link exists" "$LINKS" 1

# The same through provider-dashboard, which forwards to the driver-app.
PD_CONTACT=$(mobile)
PD_FLEET=$(curl -s -m 20 -X POST "$DRIVER/fleet/v2/create" -H "token: $OWNER_TOKEN" -H 'Content-Type: application/json' \
  -d "{\"fleetName\":\"Provider-dashboard consent fleet\",\"fleetType\":\"NORMAL_FLEET\",\"contactMobileCountryCode\":\"+91\",\"contactMobileNumber\":\"$PD_CONTACT\",\"contactEmail\":\"pd-consent@example.com\"}" | json "d['fleetId']")
sleep 15
PERSONS_BEFORE_PD=$(psql -d atlas_dev -tAc "select count(*) from atlas_dashboard.person")
check "provider-dashboard forwards the consent OTP by fleet id" "$(status -X POST "$DASH/bpp/driver-offer/$MERCHANT/Bangalore/operator/fleet/link/sendOtp" -H "token: $OP_TOKEN" -H 'Content-Type: application/json' -d "{\"mobileNumber\":\"\",\"mobileCountryCode\":\"\",\"fleetId\":\"$PD_FLEET\"}")" 200
sleep 12
check "that OTP is also kept under the fleet's contact phone" "$(redis get "${REDIS_PREFIX}Fleet:Link:PhoneNo:+91$PD_CONTACT")" '"'$OTP'"'
check "and no dashboard person is made up for the fleet" "$(psql -d atlas_dev -tAc "select count(*) from atlas_dashboard.person where id='$PD_FLEET'")" 0
check "nor any other dashboard person" "$(psql -d atlas_dev -tAc "select count(*) from atlas_dashboard.person")" "$PERSONS_BEFORE_PD"

# An operator links a member only to a fleet that is linked to them, and only while that link has not ended.
MEMBER_LINK="$DRIVER/operator/fleet/member/association/create"
MEMBER_ID="extras-member-$$"
member_link() { curl -s -m 30 -X POST "$MEMBER_LINK" -H "token: $OP_TOKEN" -H 'Content-Type: application/json' -d "{\"fleetMemberId\":\"$MEMBER_ID\",\"fleetOwnerId\":\"$1\",\"enabled\":true,\"isFleetOwner\":false}" | json "d.get('errorCode', d.get('result','none'))"; }
check "member link: an operator may link a member to a fleet linked to them" "$(member_link "$CONSENT_FLEET")" "Success"
check "member link: but not to a fleet that is not linked to them" "$(member_link "$PD_FLEET")" "ACCESS_DENIED"
# The app keeps the link in Redis as well as Postgres and reads it from there, so the end date is moved in both, as
# it would be by the passing of time.
psql -d atlas_dev -tA >/dev/null -c "UPDATE atlas_driver_offer_bpp.fleet_operator_association SET associated_till = now() - interval '1 day' WHERE fleet_owner_id='$CONSENT_FLEET'"
for port in 30001 30002 30003 30004 30005 30006; do
  for k in $(redis-cli -p $port --scan --pattern 'fleetOperatorAssociation_id_*' 2>/dev/null); do
    v=$(redis-cli -c -p $port get "$k" 2>/dev/null)
    case "$v" in *"$CONSENT_FLEET"*)
      redis-cli -c -p $port set "$k" "$(printf '%s' "$v" | sed -E 's/"associatedTill":"[^"]*"/"associatedTill":"2020-01-01T00:00:00.000Z"/')" KEEPTTL >/dev/null ;;
    esac
  done
done
sleep 3
check "member link: once the link has ended the operator can no longer link a member to the fleet" "$(member_link "$CONSENT_FLEET")" "ACCESS_DENIED"
psql -d atlas_dev -tA >/dev/null -c "DELETE FROM atlas_driver_offer_bpp.fleet_member_association WHERE fleet_member_id='$MEMBER_ID'"

echo "== 6. provider-dashboard OTP login uses the person the OTP was issued to"
PD_API="$DASH/bpp/driver-offer/NAMMA_YATRI_PARTNER/Bangalore"
OTHER_PHONE=$(mobile)
OTHER_TOKEN=$(login_phone "$OTHER_PHONE")
sleep 12
OTHER_PERSON=$(person_of "$OTHER_TOKEN")
ASKING_PHONE=$(mobile)
check "an OTP is requested for one phone through provider-dashboard" "$(status -X POST "$PD_API/fleet/v2/login/otp" -H 'Content-Type: application/json' -d "{\"mobileNumber\":\"$ASKING_PHONE\",\"mobileCountryCode\":\"+91\"}")" 200
sleep 4
BOUND_KEY="${REDIS_PREFIX}MobileNumberOtp:V2:mobileNumber-$ASKING_PHONE:personId"
[ -n "$(redis get "$BOUND_KEY")" ] && ok "the OTP is bound to the person it was issued to" || bad "the OTP is bound to the person it was issued to"
redis set "$BOUND_KEY" "\"$OTHER_PERSON\"" ex 600 >/dev/null
BOUND_TOKEN=$(curl -s -m 30 -X POST "$PD_API/fleet/v2/verify/otp" -H 'Content-Type: application/json' -d "{\"mobileNumber\":\"$ASKING_PHONE\",\"mobileCountryCode\":\"+91\",\"otp\":\"$OTP\"}" | json "d['authToken']")
sleep 4
check "the session goes to the bound person, not to the person the phone matches" "$(person_of "$BOUND_TOKEN")" "$OTHER_PERSON"
FALLBACK_PHONE=$(mobile)
status -X POST "$PD_API/fleet/v2/login/otp" -H 'Content-Type: application/json' -d "{\"mobileNumber\":\"$FALLBACK_PHONE\",\"mobileCountryCode\":\"+91\"}" >/dev/null
sleep 4
FALLBACK_KEY="${REDIS_PREFIX}MobileNumberOtp:V2:mobileNumber-$FALLBACK_PHONE:personId"
MATCHED=$(redis get "$FALLBACK_KEY" | tr -d '"')
redis del "$FALLBACK_KEY" >/dev/null
sleep 8
FALLBACK_TOKEN=$(curl -s -m 30 -X POST "$PD_API/fleet/v2/verify/otp" -H 'Content-Type: application/json' -d "{\"mobileNumber\":\"$FALLBACK_PHONE\",\"mobileCountryCode\":\"+91\",\"otp\":\"$OTP\"}" | json "d['authToken']")
sleep 4
check "with no stored person the session goes to the person the phone matches" "$(person_of "$FALLBACK_TOKEN")" "$MATCHED"

echo "== 8. a list over several fleets leaves out the ones it cannot show, in ENFORCE"
set_mode ENFORCE
PART_TOKEN=$(login)
PART_P=$(person_of "$PART_TOKEN")
sleep 3
PART_Q=$(curl -s -m 20 -X POST "$DRIVER/fleet/v2/create" -H "token: $PART_TOKEN" -H 'Content-Type: application/json' -d '{"fleetName":"Second of two","fleetType":"NORMAL_FLEET"}' | json "d['fleetId']")
# Straight after, while the new fleet's rows are still only in Redis.
check "a fleet created a moment ago does not break its owner's lists" \
  "$(curl -s -m 30 "$DRIVER/driver/fleet/driverAssociation?limit=10&offset=0" -H "token: $PART_TOKEN" | json "d.get('errorCode','none')")" none
for fleet in "$PART_P" "$PART_Q"; do wait_sql "select count(*) from atlas_driver_offer_bpp.fleet_owner_information where fleet_owner_person_id='$fleet'" 1 150 >/dev/null; done
DRV_P=$(python3 -c 'import uuid; print(uuid.uuid4())'); DRV_Q=$(python3 -c 'import uuid; print(uuid.uuid4())')
mkdriver "$DRV_P"; mkdriver "$DRV_Q"
for pair in "$DRV_P:$PART_P" "$DRV_Q:$PART_Q"; do
  psql -d atlas_dev -tA -v ON_ERROR_STOP=1 >/dev/null -c "INSERT INTO atlas_driver_offer_bpp.fleet_driver_association (id, driver_id, fleet_owner_id, is_active, associated_on, associated_till) VALUES (gen_random_uuid()::text, '${pair%%:*}', '${pair#*:}', true, now(), now() + interval '1 year')"
done
# The two checks, each switched on for this section and put back after: a fleet is verified at use (not at login),
# and the driver app checks that a fleet is enabled.
VERIFY_AT_LOGIN=$(psql -d atlas_dev -tAc "select coalesce(verify_fleet_while_login::text,'null') from atlas_dashboard.merchant where short_id='$MERCHANT'")
ENABLED_CHECK=$(psql -d atlas_dev -tAc "select coalesce(fleet_owner_enabled_check::text,'null') from atlas_driver_offer_bpp.merchant where short_id='$MERCHANT'")
flags() { # flags VERIFY_AT_LOGIN ENABLED_CHECK
  psql -d atlas_dev -tAc "update atlas_dashboard.merchant set verify_fleet_while_login=$1 where short_id='$MERCHANT'" >/dev/null
  psql -d atlas_dev -tAc "update atlas_driver_offer_bpp.merchant set fleet_owner_enabled_check=$2 where short_id='$MERCHANT'" >/dev/null
  set_mode ENFORCE
}
# The driver app keeps merchant settings in memory for an hour, so a change to the enabled check needs a restart.
driver_app_pid() { pgrep -f 'x/dynamic-offer-driver-app-exe/build/dynamic-offer-driver-app-exe/dynamic-offer-driver-app-exe$' | head -1; }
restart_driver_app() { # the local stack's supervisor starts it again
  local old new
  old=$(driver_app_pid)
  kill "$old"
  for _ in $(seq 1 120); do
    new=$(driver_app_pid)
    if [ -n "$new" ] && [ "$new" != "$old" ] && [ "$(status "http://localhost:8016/direct-dashboard/user/profile")" = 400 ]; then return 0; fi
    sleep 3
  done
  bad "the driver app came back after a restart"
}
RESTORE_APP=""
trap 'flags "$VERIFY_AT_LOGIN" "$ENABLED_CHECK"; if [ -n "$RESTORE_APP" ]; then restart_driver_app; fi; set_mode OFF; revoke_caps; cleanup' EXIT
fleet_state() { # fleet_state FLEET VERIFIED ENABLED: set in the database, and the app's cached row dropped
  psql -d atlas_dev -tAc "update atlas_driver_offer_bpp.fleet_owner_information set verified=$2, enabled=$3 where fleet_owner_person_id='$1'" >/dev/null
  local node key
  for node in 30001 30002 30003 30004 30005 30006; do
    for key in $(redis-cli -p "$node" --scan --pattern "*leetOwnerInformation*$1*" 2>/dev/null); do redis del "$key" >/dev/null; done
  done
  sleep 6
}
listed() { # which of the two drivers the list shows, or the reason it was refused
  curl -s -m 30 "$DRIVER/driver/fleet/driverAssociation?limit=10&offset=0$1" -H "token: $PART_TOKEN" |
    json "','.join(n for n, i in [('P','$DRV_P'),('Q','$DRV_Q')] if i in str(d.get('listItem'))) if 'listItem' in d else d.get('errorMessage')"
}
flags false false
fleet_state "$PART_P" true true; fleet_state "$PART_Q" true true
check "both fleets usable: both are listed" "$(listed "")" "P,Q"
fleet_state "$PART_Q" false true
check "one unverified: the other is still listed" "$(listed "")" "P"
check "naming the unverified one says why it is refused" "$(listed "&fleetOwnerId=$PART_Q")" "Fleet owner is not verified"
check "a fleet outside the scope is refused whatever its state" "$(listed "&fleetOwnerId=$DRV_P")" "You have no access to this operation."
fleet_state "$PART_P" false true
check "all unverified: refused, with the reason" "$(listed "")" "Fleet owner is not verified"
flags false true
RESTORE_APP=yes
restart_driver_app
fleet_state "$PART_P" true true; fleet_state "$PART_Q" true false
check "one disabled: the other is still listed" "$(listed "")" "P"
check "naming the disabled one says why it is refused" "$(listed "&fleetOwnerId=$PART_Q")" "Fleet owner is not enabled"
fleet_state "$PART_P" true false
check "all disabled: refused, with the reason" "$(listed "")" "Fleet owner is not enabled"
flags "$VERIFY_AT_LOGIN" "$ENABLED_CHECK"
restart_driver_app
RESTORE_APP=""
set_mode OFF
psql -d atlas_dev -tA >/dev/null -c "DELETE FROM atlas_driver_offer_bpp.fleet_driver_association WHERE driver_id IN ('$DRV_P', '$DRV_Q'); DELETE FROM atlas_driver_offer_bpp.driver_stats WHERE driver_id IN ('$DRV_P', '$DRV_Q'); DELETE FROM atlas_driver_offer_bpp.driver_information WHERE driver_id IN ('$DRV_P', '$DRV_Q'); DELETE FROM atlas_driver_offer_bpp.person WHERE id IN ('$DRV_P', '$DRV_Q')"

echo
echo "passed: $PASS, failed: $FAIL"
[ "$FAIL" = 0 ]
