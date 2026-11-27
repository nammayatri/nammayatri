#!/usr/bin/env bash
# What an operator, and a fleet owner reading documents, may reach once fleet scope is enforced:
#   1. operator lists (drivers, vehicles, counts, fleet owners) cover only the fleets linked to them
#   2. operator profile lookup, live map and rides: their fleets and drivers, nothing else
#   3. documents: the list and a single image, for operators and fleet owners
#   4. an operator cannot be invited to a fleet
#   5. an operator onboards a fleet: it is linked to them, they invite its owner and hand it over
#   6. outside ENFORCE the same calls behave as before
#
#   PATH=~/fleet-scope-test-tools/bin:$PATH Backend/dev/integration-tests/fleet-scope-operator.sh
#
# Needs the same local stack as fleet-scope-extras.sh, with the location tracking service running.
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/fleet-scope-lib.sh"

db() { psql -d atlas_dev -tAc "$1"; }
new_uuid() { python3 -c 'import uuid; print(uuid.uuid4())'; }
body() { curl -s -m 40 "$1" -H "token: $2"; }
code_of() { body "$1" "$2" | json "d.get('errorCode','none') if isinstance(d, dict) else 'none'"; }
message_of() { body "$1" "$2" | json "d.get('errorMessage','none') if isinstance(d, dict) else 'none'"; }
denied() { [ "$(code_of "$1" "$2")" = ACCESS_DENIED ] && echo refused || echo "$(code_of "$1" "$2")"; }
# Past the ownership check: whatever else the route then says, it did not refuse the caller.
allowed() { local c; c=$(code_of "$1" "$2"); [ "$c" != ACCESS_DENIED ] && [ -n "$c" ] && echo yes || echo "$c"; }
has() { printf '%s' "$1" | grep -c "$2" | awk '{print ($1 > 0) ? "yes" : "no"}'; }

# ---- permissions, lent for the run (the real roles hold them; the local ones do not) ----
OPERATOR_ROLE=$(db "select id from atlas_dashboard.role where name='OPERATOR'")
OWNER_ROLE=$(db "select id from atlas_dashboard.role where dashboard_access_type='FLEET_OWNER' order by name limit 1")
FLEET_ROLE=$(db "select id from atlas_dashboard.role where name='FLEET'")
GRANTED=""
lend() { # lend ROLE CAPABILITY
  if [ "$(db "select count(*) from atlas_dashboard.role_capability where role_id='$1' and capability_id='$2'")" = 0 ]; then
    db "insert into atlas_dashboard.role_capability (role_id, capability_id) values ('$1', '$2')" >/dev/null
    GRANTED="$GRANTED $1:$2"
  fi
}
take_back() { local pair; for pair in $(echo "$GRANTED"); do db "delete from atlas_dashboard.role_capability where role_id='${pair%%:*}' and capability_id='${pair#*:}'" >/dev/null; done; }

DRIVERS=""
RIDES=""
IMAGES=""
remove_seeded() {
  local rides images drivers
  rides=$(printf "'%s'," $(echo "$RIDES") | sed 's/,$//')
  images=$(printf "'%s'," $(echo "$IMAGES") | sed 's/,$//')
  drivers=$(printf "'%s'," $(echo "$DRIVERS") | sed 's/,$//')
  psql -d atlas_dev -tA >/dev/null <<SQL
CREATE TEMP TABLE b_ids AS SELECT booking_id AS id FROM atlas_driver_offer_bpp.ride WHERE id IN (${rides:-''});
DELETE FROM atlas_driver_offer_bpp.location_mapping WHERE entity_id IN (${rides:-''}) OR entity_id IN (SELECT id FROM b_ids);
DELETE FROM atlas_driver_offer_bpp.ride_details WHERE id IN (${rides:-''});
DELETE FROM atlas_driver_offer_bpp.ride WHERE id IN (${rides:-''});
DELETE FROM atlas_driver_offer_bpp.booking WHERE id IN (SELECT id FROM b_ids);
DELETE FROM atlas_driver_offer_bpp.rider_details WHERE id IN (SELECT id FROM b_ids);
DELETE FROM atlas_driver_offer_bpp.fare_parameters_progressive_details WHERE fare_parameters_id IN (SELECT id FROM b_ids);
DELETE FROM atlas_driver_offer_bpp.fare_parameters WHERE id IN (SELECT id FROM b_ids);
DELETE FROM atlas_driver_offer_bpp.image WHERE id IN (${images:-''});
DELETE FROM atlas_driver_offer_bpp.driver_operator_association WHERE driver_id IN (${drivers:-''});
DELETE FROM atlas_driver_offer_bpp.fleet_driver_association WHERE driver_id IN (${drivers:-''});
DELETE FROM atlas_driver_offer_bpp.driver_stats WHERE driver_id IN (${drivers:-''});
DELETE FROM atlas_driver_offer_bpp.driver_information WHERE driver_id IN (${drivers:-''});
SQL
}
STORE=""
trap 'if [ -n "$STORE" ]; then rm -rf "$STORE/fleet-scope-test"; fi; remove_seeded; take_back; set_mode OFF; cleanup' EXIT

for pattern in '%/DRIVER/GET_DRIVER_FLEET_DRIVER_ASSOCIATION' '%/DRIVER/GET_DRIVER_FLEET_VEHICLE_ASSOCIATION' '%/DRIVER/GET_DRIVER_FLEET_STATUS_SUMMARY' \
  '%/DRIVER/GET_DRIVER_FLEET_OWNER_INFO' '%/DRIVER/GET_DRIVER_FLEET_OWNER_LIST' '%/LIVE_MAP/GET_LIVE_MAP_DRIVERS' \
  'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_LIST' 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_LIST_V2' 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_INFO' \
  '%/GET_DRIVER_REGISTRATION_DOCUMENTS_LIST' '%/GET_DRIVER_REGISTRATION_GET_DOCUMENT' '%/POST_REGISTRATION_V2_CREATE%'; do
  for cap in $(db "select distinct capability_id from atlas_dashboard.capability_endpoint where endpoint_id like '$pattern'"); do
    lend "$OPERATOR_ROLE" "$cap"
    lend "$OWNER_ROLE" "$cap"
    lend "$FLEET_ROLE" "$cap"
  done
done

echo "== setup: an operator, a second operator with nothing, fleet A linked to the first, fleet B not"
set_mode ENFORCE
new_operator() { # prints "email token"
  local m email token
  m=$(mobile)
  email="scope-op-$m@dashboard.com"
  curl -s -m 30 -o /dev/null -X POST "$DRIVER/operator/dashboard/register" -H "token: $ADMIN_TOKEN" -H 'Content-Type: application/json' \
    -d "{\"firstName\":\"Scope\",\"lastName\":\"Operator\",\"roleId\":\"$OPERATOR_ROLE\",\"email\":\"$email\",\"mobileNumber\":\"$m\",\"mobileCountryCode\":\"+91\",\"password\":\"Extras@12345\",\"fleetAssignment\":null}"
  sleep 12
  token=$(curl -s -m 30 -X POST "$DASH/user/login" -H 'Content-Type: application/json' -d "{\"email\":\"$email\",\"password\":\"Extras@12345\"}" | json "d['authToken']")
  echo "$email $token"
}
read -r OP_EMAIL OP <<<"$(new_operator)"
read -r _ EMPTY_OP <<<"$(new_operator)"
[ -n "$OP" ] && [ -n "$EMPTY_OP" ] && ok "both operators log in" || bad "both operators log in"
OP_ID=$(person_of "$OP")
A_PHONE=$(mobile)
A_TOKEN=$(login_phone "$A_PHONE")
B_TOKEN=$(login_phone "$(mobile)")
A=$(person_of "$A_TOKEN")
B=$(person_of "$B_TOKEN")
sleep 12
db "update atlas_driver_offer_bpp.fleet_owner_information set verified=true, enabled=true where fleet_owner_person_id in ('$A','$B')" >/dev/null
curl -s -m 20 -o /dev/null -X POST "$DRIVER/operator/fleet/link/sendOtp" -H "token: $OP" -H 'Content-Type: application/json' -d "{\"mobileNumber\":\"\",\"mobileCountryCode\":\"\",\"fleetId\":\"$A\"}"
sleep 3
check "the operator links fleet A" "$(status -X POST "$DRIVER/operator/fleet/link/verifyOtp" -H "token: $OP" -H 'Content-Type: application/json' -d "{\"fleetOwnerId\":\"$A\",\"otp\":\"$OTP\"}")" 200

MID=$(db "select id from atlas_driver_offer_bpp.merchant where short_id='$MERCHANT'")
CITY=$(db "select id from atlas_driver_offer_bpp.merchant_operating_city where merchant_short_id='$MERCHANT' and city='Bangalore'")
SOURCE_DRIVER=$(db "select p.id from atlas_driver_offer_bpp.person p join atlas_driver_offer_bpp.driver_information i on i.driver_id = p.id where p.role='DRIVER' and p.merchant_operating_city_id='$CITY' order by p.id limit 1")
mkdriver() { # mkdriver ID
  psql -d atlas_dev -tA -v ON_ERROR_STOP=1 >/dev/null <<SQL
INSERT INTO atlas_driver_offer_bpp.person SELECT r.* FROM (SELECT * FROM atlas_driver_offer_bpp.person WHERE id='$SOURCE_DRIVER') p
  CROSS JOIN LATERAL jsonb_populate_record(null::atlas_driver_offer_bpp.person,
    to_jsonb(p) || jsonb_build_object('id','$1','mobile_number_hash','\\x' || md5(random()::text),'created_at', to_char(now(), 'YYYY-MM-DD"T"HH24:MI:SS"Z"'))) r;
INSERT INTO atlas_driver_offer_bpp.driver_information SELECT r.* FROM (SELECT * FROM atlas_driver_offer_bpp.driver_information WHERE driver_id='$SOURCE_DRIVER') d
  CROSS JOIN LATERAL jsonb_populate_record(null::atlas_driver_offer_bpp.driver_information, to_jsonb(d) || jsonb_build_object('driver_id','$1')) r;
INSERT INTO atlas_driver_offer_bpp.driver_stats SELECT r.* FROM (SELECT * FROM atlas_driver_offer_bpp.driver_stats WHERE driver_id='$SOURCE_DRIVER' LIMIT 1) d
  CROSS JOIN LATERAL jsonb_populate_record(null::atlas_driver_offer_bpp.driver_stats, to_jsonb(d) || jsonb_build_object('driver_id','$1')) r;
SQL
}
DA=$(new_uuid) DB_=$(new_uuid) DD=$(new_uuid) DX=$(new_uuid)
DRIVERS="$DA $DB_ $DD $DX"
for d in $(echo "$DRIVERS"); do mkdriver "$d"; done
for pair in "$DA:$A" "$DB_:$B"; do
  db "insert into atlas_driver_offer_bpp.fleet_driver_association (id, driver_id, fleet_owner_id, is_active, associated_on, associated_till) values (gen_random_uuid()::text, '${pair%%:*}', '${pair#*:}', true, now(), now() + interval '1 year')" >/dev/null
done
# DD is linked to the operator directly, with no fleet.
psql -d atlas_dev -tA -v ON_ERROR_STOP=1 >/dev/null -c "insert into atlas_driver_offer_bpp.driver_operator_association (id, driver_id, operator_id, is_active, associated_on, associated_till, merchant_id, merchant_operating_city_id, created_at, updated_at) values (gen_random_uuid()::text, '$DD', '$OP_ID', true, now(), now() + interval '1 year', '$MID', '$CITY', now(), now())"

mkrc() { # mkrc FLEET: prints the certificate id
  local id plate
  id=$(new_uuid)
  plate="OP-$(python3 -c 'import random;print(random.randint(100000,999999))')"
  psql -d atlas_dev -tA -v ON_ERROR_STOP=1 >/dev/null <<SQL
INSERT INTO atlas_driver_offer_bpp.vehicle_registration_certificate SELECT r.* FROM (SELECT * FROM atlas_driver_offer_bpp.vehicle_registration_certificate LIMIT 1) v
  CROSS JOIN LATERAL jsonb_populate_record(null::atlas_driver_offer_bpp.vehicle_registration_certificate,
    to_jsonb(v) || jsonb_build_object('id','$id','fleet_owner_id','$1','unencrypted_certificate_number','$plate','certificate_number_hash','\\x' || md5(random()::text),
      'merchant_id','$MID','merchant_operating_city_id','$CITY','created_at', to_char(now(), 'YYYY-MM-DD"T"HH24:MI:SS"Z"'))) r;
SQL
  echo "$id"
}
RC_A=$(mkrc "$A")
RC_B=$(mkrc "$B")

mkimage() { # mkimage PERSON [RC]: prints the image id
  local id rc="NULL"
  id=$(new_uuid)
  [ -n "${2:-}" ] && rc="'$2'"
  psql -d atlas_dev -tA -v ON_ERROR_STOP=1 >/dev/null -c "insert into atlas_driver_offer_bpp.image (id, person_id, merchant_id, merchant_operating_city_id, s3_path, image_type, rc_id, created_at, updated_at) values ('$id', '$1', '$MID', '$CITY', 'no/such/file', 'DriverLicense', $rc, now(), now())"
  echo "$id"
}
IMG_A=$(mkimage "$DA")
IMG_B=$(mkimage "$DB_")
IMG_B_VEHICLE=$(mkimage "$DB_" "$RC_B")
IMAGES="$IMG_A $IMG_B $IMG_B_VEHICLE"
# Only the image of driver A has a file. The others have none, so they can be refused only by a check that runs
# before the file is looked for. The local app keeps its files under Backend/s3.
STORE="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)/s3/local/test-bucket"
mkdir -p "$STORE/fleet-scope-test"
printf 'contents-of-driver-A-document' >"$STORE/fleet-scope-test/$IMG_A"
db "update atlas_driver_offer_bpp.image set s3_path='fleet-scope-test/$IMG_A' where id='$IMG_A'" >/dev/null
image_of() { body "$DRIVER/driver/getDocument/$1" "$2" | json "d.get('imageBase64') or d.get('errorCode','none')"; }

mkride() { # mkride FLEET_OR_EMPTY DRIVER ID
  local rid=$3 bid lid fo="NULL"
  bid=$(new_uuid)
  lid=$(new_uuid)
  [ -n "$1" ] && fo="'$1'"
  psql -d atlas_dev -tA -v ON_ERROR_STOP=1 >/dev/null <<SQL
INSERT INTO atlas_driver_offer_bpp.location (id, lat, lon, merchant_id, merchant_operating_city_id) VALUES ('$lid', 12.9, 77.6, '$MID', '$CITY');
INSERT INTO atlas_driver_offer_bpp.location_mapping (id, tag, location_id, entity_id, "order", version, merchant_id, merchant_operating_city_id)
  VALUES (gen_random_uuid()::text, 'RIDE', '$lid', '$rid', 0, 'LATEST', '$MID', '$CITY'), (gen_random_uuid()::text, 'BOOKING', '$lid', '$bid', 0, 'LATEST', '$MID', '$CITY');
INSERT INTO atlas_driver_offer_bpp.fare_parameters (id, base_fare, fare_parameters_type) VALUES ('$bid', 100, 'Progressive');
INSERT INTO atlas_driver_offer_bpp.fare_parameters_progressive_details (fare_parameters_id, dead_km_fare) VALUES ('$bid', 0);
INSERT INTO atlas_driver_offer_bpp.rider_details (id, merchant_id, mobile_country_code, mobile_number_encrypted, mobile_number_hash, cancellation_dues, dispute_chances_used, has_taken_valid_ride, night_safety_checks)
  SELECT '$bid', '$MID', '+91', mobile_number_encrypted, decode(md5('$bid'),'hex'), 0, 0, true, false FROM atlas_driver_offer_bpp.person WHERE mobile_number_encrypted IS NOT NULL LIMIT 1;
INSERT INTO atlas_driver_offer_bpp.booking (id, bap_id, bap_uri, estimated_fare, fare_parameters_id, primary_exophone, provider_id, quote_id, start_time, status, transaction_id, vehicle_variant, merchant_operating_city_id, rider_id)
  VALUES ('$bid','b','http://b',100,'$bid','0','$MID','$bid', now(),'COMPLETED','$bid','SEDAN','$CITY','$bid');
INSERT INTO atlas_driver_offer_bpp.ride_details (id, driver_name, vehicle_number) VALUES ('$rid', 'Seeded driver', 'RIDE-TEST-1');
INSERT INTO atlas_driver_offer_bpp.ride (id, booking_id, driver_id, otp, short_id, status, tracking_url, fleet_owner_id, merchant_id, merchant_operating_city_id)
  VALUES ('$rid', '$bid', '$2', '1234', substr('$rid',1,30), 'COMPLETED', 'x', $fo, '$MID', '$CITY');
SQL
}
R_A=$(new_uuid) R_B=$(new_uuid) R_D=$(new_uuid)
RIDES="$R_A $R_B $R_D"
mkride "$A" "$DA" "$R_A"
mkride "$B" "$DB_" "$R_B"
mkride "" "$DD" "$R_D"
sleep 8 # lookups read the replica
# Wait until each owner sees their own driver and vehicle. Only then does "another fleet's is not listed" say anything.
# A vehicle row is told apart by its certificate id.
in_place() {
  printf '%s%s%s%s' \
    "$(has "$(body "$DRIVER/driver/fleet/driverAssociation?limit=10&offset=0" "$A_TOKEN")" "$DA")" \
    "$(has "$(body "$DRIVER/driver/fleet/driverAssociation?limit=10&offset=0" "$B_TOKEN")" "$DB_")" \
    "$(has "$(body "$DRIVER/driver/fleet/vehicleAssociation?Limit=10&Offset=0" "$A_TOKEN")" "$RC_A")" \
    "$(has "$(body "$DRIVER/driver/fleet/vehicleAssociation?Limit=10&Offset=0" "$B_TOKEN")" "$RC_B")"
}
for _ in $(seq 1 30); do [ "$(in_place)" = yesyesyesyes ] && break; sleep 3; done
check "each owner sees their own driver and vehicle (the test data is in place)" "$(in_place)" yesyesyesyes

echo "== 1. operator lists cover only linked fleets"
DRIVER_LIST="$DRIVER/driver/fleet/driverAssociation?limit=10&offset=0"
LISTED=$(body "$DRIVER_LIST" "$OP")
check "drivers: the linked fleet's driver is listed" "$(has "$LISTED" "$DA")" yes
check "drivers: another fleet's driver is not" "$(has "$LISTED" "$DB_")" no
check "drivers: naming the linked fleet works" "$(has "$(body "$DRIVER_LIST&fleetOwnerId=$A" "$OP")" "$DA")" yes
check "drivers: naming another fleet is refused" "$([ "$(status "$DRIVER_LIST&fleetOwnerId=$B" -H "token: $OP")" != 200 ] && echo refused)" refused
check "drivers: an operator with no fleet gets an empty page, not everyone" "$(body "$DRIVER_LIST" "$EMPTY_OP" | json "len(d['listItem'])")" 0
VEHICLE_LIST="$DRIVER/driver/fleet/vehicleAssociation?Limit=10&Offset=0"
VEHICLES=$(body "$VEHICLE_LIST" "$OP")
check "vehicles: the linked fleet's vehicle is listed" "$(has "$VEHICLES" "$RC_A")" yes
check "vehicles: another fleet's is not" "$(has "$VEHICLES" "$RC_B")" no
check "vehicles: an operator with no fleet gets an empty page" "$(body "$VEHICLE_LIST" "$EMPTY_OP" | json "len(d['listItem'])")" 0
COUNTS="$DRIVER/driver/fleet/%22DRIVER_STATUS%22/statusSummary"
check "counts: an operator with no fleet counts nothing" "$(body "$COUNTS" "$EMPTY_OP" | json "d.get('total', d.get('errorCode'))")" 0
check "counts: naming another fleet is refused" "$(denied "$COUNTS?fleetOwnerId=$B" "$OP")" refused
check "fleet owners: another fleet is not listed" "$(has "$(body "$DRIVER/driver/fleet/ownerList?limit=100&offset=0" "$OP")" "$B")" no

echo "== 2. operator profile lookup, live map and rides"
check "profile: a linked fleet's can be read" "$(allowed "$DRIVER/driver/$A/fleetOwnerInfo" "$OP")" yes
check "profile: another fleet's is refused" "$(denied "$DRIVER/driver/$B/fleetOwnerInfo" "$OP")" refused
check "profile: the operator's own can be read" "$(allowed "$DRIVER/driver/$OP_ID/fleetOwnerInfo" "$OP")" yes
MAP="$DRIVER/liveMap/drivers/5000"
AT="point=12.97,77.59"
check "map: all the operator's drivers" "$(status "$MAP?$AT" -H "token: $OP")" 200
check "map: on behalf of a linked fleet" "$(status "$MAP?$AT&fleetOwnerId=$A" -H "token: $OP")" 200
check "map: on behalf of another fleet is refused" "$(denied "$MAP?$AT&fleetOwnerId=$B" "$OP")" refused
check "map: centred on a driver of a linked fleet" "$(allowed "$MAP?driverIdForRadius=$DA" "$OP")" yes
check "map: centred on a driver of the operator's own" "$(allowed "$MAP?driverIdForRadius=$DD" "$OP")" yes
check "map: centred on someone else's driver is refused" "$(denied "$MAP?driverIdForRadius=$DB_" "$OP")" refused
check "map: a fleet owner cannot centre it on another fleet's driver" "$(denied "$MAP?driverIdForRadius=$DB_" "$A_TOKEN")" refused
WINDOW="limit=50&offset=0&from=2026-10-01T00:00:00Z&to=2099-01-01T00:00:00Z"
ours="['$R_A','$R_B','$R_D']"
rides_of() { body "$DRIVER/ride/$1?$WINDOW$2" "${3:-$OP}" | json "','.join(sorted(r['rideId'] for r in d['rides'] if r['rideId'] in $ours)) if 'rides' in d else d.get('errorMessage', d.get('errorCode'))"; }
short() { echo "${1:0:30}"; }
for list in list listV2; do
  check "$list: a linked fleet's rides" "$(rides_of $list "&fleetOwnerId=$A")" "$R_A"
  check "$list: another fleet's rides are refused" "$([ "$(status "$DRIVER/ride/$list?$WINDOW&fleetOwnerId=$B" -H "token: $OP")" != 200 ] && echo refused)" refused
  check "$list: neither a fleet nor a driver named" "$(rides_of $list "&bookingStatus=%22COMPLETED%22" | grep -c "Choose a fleet")" 1
  check "$list: the rides of a linked fleet's driver" "$(rides_of $list "&driverId=$DA")" "$R_A"
  check "$list: the rides of the operator's own driver" "$(rides_of $list "&driverId=$DD")" "$R_D"
  check "$list: someone else's driver is refused" "$(denied "$DRIVER/ride/$list?$WINDOW&driverId=$DB_" "$OP")" refused
  check "$list: a driver of no one is refused" "$(denied "$DRIVER/ride/$list?$WINDOW&driverId=$DX" "$OP")" refused
  check "$list: own driver plus a customer phone is refused" "$(rides_of $list "&driverId=$DD&customerPhoneNo=9999999999" | grep -c "needs a fleet")" 1
  check "$list: another fleet's ride cannot be fetched by short id under a linked fleet" "$(rides_of $list "&fleetOwnerId=$A&rideShortId=$(short "$R_B")" | grep -c "$R_B")" 0
  check "$list: nor under the operator's own driver" "$(rides_of $list "&driverId=$DD&rideShortId=$(short "$R_B")" | grep -c "$R_B")" 0
  check "$list: nor by a fleet owner" "$(rides_of $list "&fleetOwnerId=$A&rideShortId=$(short "$R_B")" "$A_TOKEN" | grep -c "$R_B")" 0
done
check "ride details: a linked fleet's ride opens" "$(code_of "$DRIVER/ride/$R_A/info" "$OP")" none
check "ride details: the operator's own driver's ride opens" "$(code_of "$DRIVER/ride/$R_D/info" "$OP")" none
check "ride details: another fleet's is refused" "$(denied "$DRIVER/ride/$R_B/info" "$OP")" refused

echo "== 3. documents"
for who in "operator:$OP" "fleet owner:$A_TOKEN"; do
  label=${who%%:*}
  token=${who#*:}
  check "$label: lists their own driver's documents" "$(status "$DRIVER/driver/$DA/documents/list" -H "token: $token")" 200
  check "$label: another fleet's driver's list is refused" "$(denied "$DRIVER/driver/$DB_/documents/list" "$token")" refused
  check "$label: the other fleet's own list is refused" "$(denied "$DRIVER/driver/$B/documents/list" "$token")" refused
  check "$label: their own vehicle can be named" "$(status "$DRIVER/driver/$DA/documents/list?rcId=$RC_A" -H "token: $token")" 200
  check "$label: another fleet's vehicle cannot" "$(denied "$DRIVER/driver/$DA/documents/list?rcId=$RC_B" "$token")" refused
  check "$label: their own driver's image is returned" "$(image_of "$IMG_A" "$token")" contents-of-driver-A-document
  check "$label: another fleet's driver's image is refused" "$(denied "$DRIVER/driver/getDocument/$IMG_B" "$token")" refused
  check "$label: another fleet's vehicle image is refused" "$(denied "$DRIVER/driver/getDocument/$IMG_B_VEHICLE" "$token")" refused
done
check "an admin still lists any driver's documents" "$(status "$DRIVER/driver/$DB_/documents/list" -H "token: $ADMIN_TOKEN")" 200
check "an admin is still given any image" "$(image_of "$IMG_A" "$ADMIN_TOKEN")" contents-of-driver-A-document

echo "== 4. an operator cannot be invited to a fleet"
INVITE=$(curl -s -m 30 -X POST "$DASH/user/fleetAssignment/$A/invite" -H "token: $A_TOKEN" -H 'Content-Type: application/json' -d "{\"email\":\"$OP_EMAIL\",\"handover\":false}")
check "the invitation is refused, saying why" "$(echo "$INVITE" | json "d.get('errorMessage','none').startswith('An operator cannot be invited')")" True
check "no assignment was written" "$(db "select count(*) from atlas_dashboard.person_role_assignment where person_id='$OP_ID' and scope_type='FLEET'")" 0

echo "== 5. an operator onboards a fleet, invites its owner and hands it over"
NEW_FLEET=$(curl -s -m 40 -X POST "$DRIVER/fleet/v2/create" -H "token: $OP" -H 'Content-Type: application/json' -d '{"fleetName":"Onboarded by operator","fleetType":"NORMAL_FLEET"}' | json "d.get('fleetId', d.get('errorMessage', d.get('errorCode')))")
check "the operator creates a fleet" "$([ ${#NEW_FLEET} = 36 ] && echo yes || echo "$NEW_FLEET")" yes
check "it is linked to the operator straight away: its profile can be read" "$(allowed "$DRIVER/driver/$NEW_FLEET/fleetOwnerInfo" "$OP")" yes
check "the link is stored" "$(wait_sql "select count(*) from atlas_driver_offer_bpp.fleet_operator_association where fleet_owner_id='$NEW_FLEET' and operator_id='$OP_ID' and is_active" 1 150)" 1
check "the other operator cannot read it" "$(denied "$DRIVER/driver/$NEW_FLEET/fleetOwnerInfo" "$EMPTY_OP")" refused
role_of() { db "select coalesce((select status from atlas_dashboard.person_role_assignment where person_id='$1' and scope_type='FLEET' and scope_id='$NEW_FLEET'), 'none')"; }
check "the operator holds the role that lets them invite" "$(role_of "$OP_ID")" ACTIVE
# The owner signed in with a phone, so the invitation needs an e-mail on their account.
OWNER_EMAIL="scope-owner-$(mobile)@dashboard.com"
check "the owner gets an e-mail on their account" "$(status -X POST "${DRIVER%/NAMMA_YATRI_PARTNER/Bangalore}/admin/person/change/email/$B" -H "token: $ADMIN_TOKEN" -H 'Content-Type: application/json' -d "{\"newEmail\":\"$OWNER_EMAIL\"}")" 200
INVITE_BODY="{\"email\":\"$OWNER_EMAIL\",\"handover\":true}"
OFFER=$(curl -s -m 30 -X POST "$DASH/user/fleetAssignment/$NEW_FLEET/invite" -H "token: $OP" -H 'Content-Type: application/json' -d "$INVITE_BODY" | json "d.get('id', d.get('errorMessage'))")
check "the operator invites the owner with hand over" "$([ ${#OFFER} = 36 ] && echo yes || echo "$OFFER")" yes
check "until the owner accepts, the operator keeps the role" "$(role_of "$OP_ID"),$(role_of "$B")" ACTIVE,INVITED
check "the owner accepts" "$(status -X POST "$DASH/user/fleetAssignment/accept/$OFFER" -H "token: $B_TOKEN")" 200
check "handed over: the owner is active and the operator's role is revoked" "$(role_of "$OP_ID"),$(role_of "$B")" REVOKED,ACTIVE
check "the operator can no longer manage the fleet's access" "$(status "$DASH/user/fleetAssignment/$NEW_FLEET" -H "token: $OP")" 403
check "the owner can" "$(status "$DASH/user/fleetAssignment/$NEW_FLEET" -H "token: $B_TOKEN")" 200
check "the operator still reaches the fleet through the link" "$(allowed "$DRIVER/driver/$NEW_FLEET/fleetOwnerInfo" "$OP")" yes

echo "== 6. outside ENFORCE the same calls behave as before"
set_mode OFF
sleep 3
check "OFF: an operator reads another fleet's profile, as before" "$(allowed "$DRIVER/driver/$B/fleetOwnerInfo" "$OP")" yes
check "OFF: a document list is not limited" "$(status "$DRIVER/driver/$DB_/documents/list" -H "token: $A_TOKEN")" 200
check "OFF: nor is an image" "$(image_of "$IMG_A" "$B_TOKEN")" contents-of-driver-A-document
check "OFF: an operator's ride list by driver is refused, as before" "$(denied "$DRIVER/ride/list?$WINDOW&driverId=$DA" "$OP")" refused
echo "  note OFF operator live map with no fleet answers: $(status "$MAP?$AT" -H "token: $OP") $(message_of "$MAP?$AT" "$OP")"

echo
echo "passed: $PASS, failed: $FAIL"
[ "$FAIL" = 0 ]
