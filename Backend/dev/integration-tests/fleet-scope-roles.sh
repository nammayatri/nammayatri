#!/usr/bin/env bash
# Checks for:
#   1. grantRole / revokeRole: a role granted on one city adds that role's API access there, in ENFORCE only,
#      and is taken away by revoking it
#   2. sessions: logging out or disabling an account ends the session on both apps, which share only the
#      dashboard database, and a disabled account cannot sign in again until re-enabled
#   3. the `cities` parameter of the driver info lookup (direct tree), which provider-dashboard refuses
#   4. inviting a person back to a fleet after their access was revoked, or after they handed the fleet over
#   5. a person's second fleet: the names in its access list, editing its driver, adding its vehicle
#
#   PATH=~/fleet-scope-test-tools/bin:$PATH Backend/dev/integration-tests/fleet-scope-roles.sh
#
# Needs the same local stack as fleet-scope-extras.sh. The local admin has Bangalore and Kolkata.
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/fleet-scope-lib.sh"
ROOT=http://localhost:8016/direct-dashboard

db() { psql -d atlas_dev -tAc "$1"; }
new_uuid() { python3 -c 'import uuid; print(uuid.uuid4())'; }
error_code() { curl -s -m 30 "$@" | json "d.get('errorCode','none')"; }

ADMIN_ID=$(db "select person_id from atlas_dashboard.registration_token where token='$ADMIN_TOKEN'")
PLAIN_ROLE=$(db "select id from atlas_dashboard.role where name='CUSTOMER'")           # grants nothing
CS_ROLE=$(db "select id from atlas_dashboard.role where name='CUSTOMER_SERVICE'")     # may read rides
FLEET_ROLE=$(db "select id from atlas_dashboard.role where name='FLEET'")
USER_ID=$(new_uuid)
USER_TOKEN=$(new_uuid)

remove_user() {
  psql -d atlas_dev -tA >/dev/null <<SQL
DELETE FROM atlas_dashboard.person_role_assignment WHERE person_id='$USER_ID';
DELETE FROM atlas_dashboard.registration_token WHERE person_id='$USER_ID';
DELETE FROM atlas_dashboard.merchant_access WHERE person_id='$USER_ID';
DELETE FROM atlas_dashboard.person WHERE id='$USER_ID';
SQL
}
trap 'remove_user; set_mode OFF; cleanup' EXIT

# A dashboard user holding a role that grants nothing, with the admin's cities and a ready-made session.
psql -d atlas_dev -tA -v ON_ERROR_STOP=1 >/dev/null <<SQL
INSERT INTO atlas_dashboard.person SELECT r.* FROM atlas_dashboard.person p
  CROSS JOIN LATERAL jsonb_populate_record(null::atlas_dashboard.person,
    to_jsonb(p) || jsonb_build_object('id','$USER_ID','role_id','$PLAIN_ROLE','email_hash',null,'email_encrypted',null,'admin_tier','USER',
      'mobile_number_hash', '\\x' || md5(random()::text), 'first_name','roles','last_name','user')) r
  WHERE p.id='$ADMIN_ID';
INSERT INTO atlas_dashboard.merchant_access SELECT r.* FROM atlas_dashboard.merchant_access m
  CROSS JOIN LATERAL jsonb_populate_record(null::atlas_dashboard.merchant_access,
    to_jsonb(m) || jsonb_build_object('id', gen_random_uuid()::text, 'person_id','$USER_ID')) r
  WHERE m.person_id='$ADMIN_ID' AND m.merchant_id=(SELECT merchant_id FROM atlas_dashboard.registration_token WHERE token='$ADMIN_TOKEN');
INSERT INTO atlas_dashboard.registration_token SELECT r.* FROM atlas_dashboard.registration_token t
  CROSS JOIN LATERAL jsonb_populate_record(null::atlas_dashboard.registration_token,
    to_jsonb(t) || jsonb_build_object('id', gen_random_uuid()::text, 'token','$USER_TOKEN','person_id','$USER_ID')) r
  WHERE t.token='$ADMIN_TOKEN';
SQL

# The ride list wants a filter and a date range.
rides() { status "$ROOT/$MERCHANT/$1/ride/list?limit=1&bookingStatus=%22COMPLETED%22&from=2026-01-01T00:00:00Z&to=2099-01-01T00:00:00Z" -H "token: $USER_TOKEN"; }
can_read_rides() { curl -s -m 30 "$ROOT/user/capabilities" -H "token: $USER_TOKEN" | json "'city-operations.ride.read' in d['capabilities']"; }
grant() { # grant PERSON ROLE SCOPE CITY_JSON: prints the HTTP status
  status -X POST "$ROOT/admin/person/$1/grantRole" -H "token: $ADMIN_TOKEN" -H 'Content-Type: application/json' \
    -d "{\"roleId\":\"$2\",\"scopeType\":\"$3\",\"operatingCity\":$4}"
}
assignment() { db "select id from atlas_dashboard.person_role_assignment where person_id='$USER_ID' and role_id='$CS_ROLE' and scope_type='$1' and scope_id='$2'"; }
row() { db "select status || ',' || manually_granted from atlas_dashboard.person_role_assignment where person_id='$USER_ID' and role_id='$CS_ROLE' and scope_type='CITY' and scope_id='Kolkata'"; }

echo "== 1. grantRole and revokeRole"
set_mode ENFORCE
check "before any grant: no rides in Bangalore" "$(rides Bangalore)" 403
check "before any grant: no rides in Kolkata" "$(rides Kolkata)" 403
check "a city the person has no access to cannot be granted" "$(grant "$USER_ID" "$CS_ROLE" CITY '"Chennai"')" 400
check "a city role needs a city" "$(grant "$USER_ID" "$CS_ROLE" CITY null)" 400
check "a fleet role cannot be granted on a city" "$(grant "$USER_ID" "$FLEET_ROLE" CITY '"Kolkata"')" 400
check "an admin cannot grant a role to themselves" "$(grant "$ADMIN_ID" "$CS_ROLE" CITY '"Kolkata"')" 400
check "a user with no admin rights cannot grant" "$(status -X POST "$ROOT/admin/person/$ADMIN_ID/grantRole" -H "token: $USER_TOKEN" -H 'Content-Type: application/json' -d "{\"roleId\":\"$CS_ROLE\",\"scopeType\":\"CITY\",\"operatingCity\":\"Kolkata\"}")" 403
check "an admin grants a role on Kolkata" "$(grant "$USER_ID" "$CS_ROLE" CITY '"Kolkata"')" 200
check "db: the row is active and marked as granted by hand" "$(row)" "ACTIVE,true"
check "the same grant again is refused" "$(grant "$USER_ID" "$CS_ROLE" CITY '"Kolkata"')" 400
check "ENFORCE: rides in Kolkata are now allowed" "$(rides Kolkata)" 200
check "ENFORCE: rides in Bangalore are still refused" "$(rides Bangalore)" 403
# The list is for the city the session is signed into, which is Bangalore here.
check "the capability list leaves out a role held only on another city" "$(can_read_rides)" False
set_mode OFF
check "OFF: the grant adds nothing in Kolkata" "$(rides Kolkata)" 403
check "OFF: nor in Bangalore" "$(rides Bangalore)" 403
set_mode ENFORCE
SYNCED=$(db "select id from atlas_dashboard.person_role_assignment where person_id='$USER_ID' and manually_granted=false limit 1")
if [ -n "$SYNCED" ]; then
  check "a role the sync manages cannot be revoked here" "$(status -X POST "$ROOT/admin/person/$USER_ID/revokeRole/$SYNCED" -H "token: $ADMIN_TOKEN")" 400
else
  echo "  note no synced row exists for this user, so revoking one is not checked"
fi
GRANT_ID=$(assignment CITY Kolkata)
check "the grant cannot be revoked through another person" "$(status -X POST "$ROOT/admin/person/$ADMIN_ID/revokeRole/$GRANT_ID" -H "token: $ADMIN_TOKEN")" 400
check "an admin revokes it" "$(status -X POST "$ROOT/admin/person/$USER_ID/revokeRole/$GRANT_ID" -H "token: $ADMIN_TOKEN")" 200
check "db: the row is revoked" "$(db "select status from atlas_dashboard.person_role_assignment where id='$GRANT_ID'")" REVOKED
check "ENFORCE: rides in Kolkata are refused again" "$(rides Kolkata)" 403
check "it can be granted again" "$(grant "$USER_ID" "$CS_ROLE" CITY '"Kolkata"')" 200
check "and works again" "$(rides Kolkata)" 200
# A merchant-wide grant covers every city the person has access to, and no others.
check "an admin grants the role on the whole merchant" "$(grant "$USER_ID" "$CS_ROLE" MERCHANT null)" 200
check "ENFORCE: rides in Bangalore are now allowed" "$(rides Bangalore)" 200
check "the capability list now includes the merchant-wide role's" "$(can_read_rides)" True
check "ENFORCE: a city the person has no access to stays refused" "$(rides Chennai)" 403
set_mode OFF

echo "== 2. ending sessions, across provider-dashboard and the driver app"
OPERATOR_ROLE=$(db "select id from atlas_dashboard.role where name='OPERATOR'")
for MODE in OFF ENFORCE; do
  set_mode $MODE
  OP_PHONE=$(mobile)
  OP_EMAIL="roles-op-$OP_PHONE@dashboard.com"
  check "$MODE: an admin creates an operator" "$(status -X POST "$DRIVER/operator/dashboard/register" -H "token: $ADMIN_TOKEN" -H 'Content-Type: application/json' \
    -d "{\"firstName\":\"Roles\",\"lastName\":\"Operator\",\"roleId\":\"$OPERATOR_ROLE\",\"email\":\"$OP_EMAIL\",\"mobileNumber\":\"$OP_PHONE\",\"mobileCountryCode\":\"+91\",\"password\":\"Roles@12345\",\"fleetAssignment\":null}")" 200
  sleep 8
  LOGIN="{\"email\":\"$OP_EMAIL\",\"password\":\"Roles@12345\"}"
  sign_in() { curl -s -m 30 -X POST "$1/user/login" -H 'Content-Type: application/json' -d "$LOGIN" | json "d['authToken']"; }
  on_pd() { status "$DASH/user/profile" -H "token: $1"; }
  on_direct() { status "$ROOT/user/profile" -H "token: $1"; }
  denied() { case "$2" in 401 | 403 | 400) ok "$1 ($2)" ;; *) bad "$1 (got $2)" ;; esac; }

  T=$(sign_in "$DASH")
  check "$MODE: the session works on provider-dashboard" "$(on_pd "$T")" 200
  check "$MODE: and on the direct tree" "$(on_direct "$T")" 200
  check "$MODE: logging out on provider-dashboard succeeds" "$(status -X POST "$DASH/user/logout" -H "token: $T")" 200
  denied "$MODE: after logout the direct tree refuses it at once" "$(on_direct "$T")"
  denied "$MODE: and so does provider-dashboard" "$(on_pd "$T")"

  T=$(sign_in "$ROOT")
  check "$MODE: a session from the direct tree works on both" "$(on_direct "$T")$(on_pd "$T")" 200200
  OP_ID=$(person_of "$T")
  check "$MODE: an admin disables the account on provider-dashboard" "$(status -X POST "$DASH/admin/person/changeEnabledStatus/$OP_ID" -H "token: $ADMIN_TOKEN" -H 'Content-Type: application/json' -d '{"enabled":false}')" 200
  denied "$MODE: the direct tree refuses the disabled account at once" "$(on_direct "$T")"
  denied "$MODE: and so does provider-dashboard" "$(on_pd "$T")"
  check "$MODE: a disabled account cannot sign in on the direct tree" "$(error_code -X POST "$ROOT/user/login" -H 'Content-Type: application/json' -d "$LOGIN")" USER_DISABLED
  check "$MODE: nor on provider-dashboard" "$(error_code -X POST "$DASH/user/login" -H 'Content-Type: application/json' -d "$LOGIN")" USER_DISABLED
  check "$MODE: the account is enabled again" "$(status -X POST "$DASH/admin/person/changeEnabledStatus/$OP_ID" -H "token: $ADMIN_TOKEN" -H 'Content-Type: application/json' -d '{"enabled":true}')" 200
  T=$(sign_in "$ROOT")
  check "$MODE: and can sign in and use both" "$(on_direct "$T")$(on_pd "$T")" 200200
done
set_mode OFF

echo "== 3. driver info: the cities parameter"
BLR=$(db "select id from atlas_driver_offer_bpp.merchant_operating_city where merchant_short_id='$MERCHANT' and city='Bangalore'")
DRIVER_ID=$(db "select p.id from atlas_driver_offer_bpp.person p join atlas_driver_offer_bpp.driver_information i on i.driver_id=p.id join atlas_driver_offer_bpp.driver_stats s on s.driver_id=p.id where p.role='DRIVER' and p.merchant_operating_city_id='$BLR' limit 1")
info() { status "$DRIVER/driver/info?personId=$DRIVER_ID$1" -H "token: $ADMIN_TOKEN"; }
if [ -z "$DRIVER_ID" ]; then
  bad "a Bangalore driver exists to look up"
else
  for MODE in OFF ENFORCE; do
    set_mode $MODE
    check "$MODE: without cities the URL's city is searched" "$(info "")" 200
    check "$MODE: naming the driver's city finds them" "$(info "&cities=Bangalore")" 200
    check "$MODE: two cities the caller holds, one of them the driver's" "$(info "&cities=Kolkata,Bangalore")" 200
    case "$(info "&cities=Kolkata")" in 400 | 404) ok "$MODE: only another city does not find them" ;; *) bad "$MODE: only another city does not find them ($(info "&cities=Kolkata"))" ;; esac
    check "$MODE: a city the caller has no access to is refused" "$(info "&cities=Chennai")" 403
    case "$(info "&cities=Atlantis")" in 400 | 403) ok "$MODE: an unknown city is refused" ;; *) bad "$MODE: an unknown city is refused ($(info "&cities=Atlantis"))" ;; esac
    check "$MODE: provider-dashboard says cities is not supported there" \
      "$(error_code "$DASH/bpp/driver-offer/$MERCHANT/Bangalore/driver/info?personId=$DRIVER_ID&cities=Bangalore" -H "token: $ADMIN_TOKEN")" INVALID_REQUEST
  done
fi

echo "== 4. inviting a person back to a fleet"
set_mode ENFORCE
OWNER_TOKEN=$(login)
OWNER_ID=$(person_of "$OWNER_TOKEN")
sleep 8
# The person invited is another fleet owner, given an e-mail so that an invitation can name them. An operator cannot
# be the one invited; the operator section 2 made is only used to check that.
GUEST_TOKEN=$(login)
GUEST_ID=$(person_of "$GUEST_TOKEN")
GUEST_EMAIL="roles-guest-$(mobile)@dashboard.com"
sleep 8
check "an admin gives the person to invite an e-mail" "$(status -X POST "$ROOT/admin/person/change/email/$GUEST_ID" -H "token: $ADMIN_TOKEN" -H 'Content-Type: application/json' -d "{\"newEmail\":\"$GUEST_EMAIL\"}")" 200
invite() { # invite TOKEN EMAIL HANDOVER: prints the HTTP status
  status -X POST "$DASH/user/fleetAssignment/$OWNER_ID/invite" -H "token: $1" -H 'Content-Type: application/json' -d "{\"email\":\"$2\",\"handover\":$3}"
}
of_person() { db "select $2 from atlas_dashboard.person_role_assignment where person_id='$1' and scope_type='FLEET' and scope_id='$OWNER_ID'"; }
accept() { status -X POST "$DASH/user/fleetAssignment/accept/$(of_person "$GUEST_ID" id)" -H "token: $GUEST_TOKEN"; }
revoke() { status -X POST "$DASH/user/fleetAssignment/revoke/$(of_person "$1" id)" -H "token: $2"; }
check "an operator cannot be invited" "$(invite "$OWNER_TOKEN" "$OP_EMAIL" false)" 400
check "the owner invites a person" "$(invite "$OWNER_TOKEN" "$GUEST_EMAIL" false)" 200
FIRST_ID=$(of_person "$GUEST_ID" id)
check "inviting them twice is refused" "$(invite "$OWNER_TOKEN" "$GUEST_EMAIL" false)" 400
check "they accept" "$(accept)" 200
check "inviting someone who is already on the fleet is refused" "$(invite "$OWNER_TOKEN" "$GUEST_EMAIL" false)" 400
check "the owner revokes them" "$(revoke "$GUEST_ID" "$OWNER_TOKEN")" 200
check "a revoked person can be invited back" "$(invite "$OWNER_TOKEN" "$GUEST_EMAIL" false)" 200
check "db: it is the same row, waiting for an answer, with no handover" "$(of_person "$GUEST_ID" "id || ',' || status || ',' || (handover_from is null)")" "$FIRST_ID,INVITED,true"
check "they accept again" "$(accept)" 200
check "db: active again" "$(of_person "$GUEST_ID" status)" ACTIVE
check "revoked once more" "$(revoke "$GUEST_ID" "$OWNER_TOKEN")" 200
check "and invited back as a handover" "$(invite "$OWNER_TOKEN" "$GUEST_EMAIL" true)" 200
check "db: the new offer carries the handover" "$(of_person "$GUEST_ID" "status || ',' || handover_from")" "INVITED,$OWNER_ID"
check "they accept the handover" "$(accept)" 200
check "db: the previous owner is revoked" "$(of_person "$OWNER_ID" status)" REVOKED
# The previous owner signs in with a phone, so the invitation needs an e-mail on their account.
OWNER_EMAIL="roles-owner-$(mobile)@dashboard.com"
check "an admin gives the previous owner an e-mail" "$(status -X POST "$ROOT/admin/person/change/email/$OWNER_ID" -H "token: $ADMIN_TOKEN" -H 'Content-Type: application/json' -d "{\"newEmail\":\"$OWNER_EMAIL\"}")" 200
OWNER_ROW=$(of_person "$OWNER_ID" id)
check "after a handover, the new admin can invite the previous owner back" "$(invite "$GUEST_TOKEN" "$OWNER_EMAIL" false)" 200
check "db: their row is waiting again" "$(of_person "$OWNER_ID" "id || ',' || status")" "$OWNER_ROW,INVITED"
set_mode OFF

echo "== 5. a person's second fleet, made with Create fleet (it has no phone of its own)"
set_mode ENFORCE
PIXEL="iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg=="
post() { curl -s -m 60 -X POST "$1" -H "token: $2" -H 'Content-Type: application/json' -d "$3"; }
post_code() { status -X POST "$1" -H "token: $2" -H 'Content-Type: application/json' -d "$3"; }
OWN_PHONE=$(mobile)
OWN_TOKEN=$(login_phone "$OWN_PHONE")
FIRST_FLEET=$(person_of "$OWN_TOKEN")
sleep 8
SECOND_FLEET=$(post "$DRIVER/fleet/v2/create" "$OWN_TOKEN" "{\"fleetName\":\"Roles second fleet\",\"fleetType\":\"NORMAL_FLEET\",\"contactMobileCountryCode\":\"+91\",\"contactMobileNumber\":\"$(mobile)\"}" | json "d['fleetId']")
STRANGER=$(person_of "$(login)")
[ -n "$SECOND_FLEET" ] && ok "the owner creates a second fleet" || bad "the owner creates a second fleet"
sleep 10
# The list of everyone on a fleet names people and roles.
check "the fleet's access list gives names and role names" \
  "$(curl -s -m 30 "$DASH/user/fleetAssignment/$SECOND_FLEET" -H "token: $OWN_TOKEN" | json "all(a.get('personName') is not None and a.get('roleName') for a in d) and len(d) == 1")" True

DRIVER_PHONE=$(mobile)
AUTH=$(post "$DRIVER/driver/fleet/sendJoiningOtp?fleetOwnerId=$SECOND_FLEET" "$OWN_TOKEN" "{\"mobileNumber\":\"$DRIVER_PHONE\",\"mobileCountryCode\":\"+91\",\"merchantId\":\"$MERCHANT\",\"deviceToken\":\"roles-$DRIVER_PHONE\"}" | json "d['authId']")
sleep 8
check "a driver joins the second fleet" "$(post_code "$DRIVER/driver/fleet/verifyJoiningOtp?fleetOwnerId=$SECOND_FLEET&authId=$AUTH" "$OWN_TOKEN" "{\"mobileNumber\":\"$DRIVER_PHONE\",\"mobileCountryCode\":\"+91\",\"otp\":\"$OTP\",\"deviceToken\":\"roles-$DRIVER_PHONE\"}")" 200
NEW_DRIVER=$(wait_sql "select count(*) from atlas_driver_offer_bpp.fleet_driver_association where fleet_owner_id='$SECOND_FLEET' and is_active" 1 150 >/dev/null; db "select driver_id from atlas_driver_offer_bpp.fleet_driver_association where fleet_owner_id='$SECOND_FLEET' and is_active limit 1")
check "the owner edits that driver while signed in as the first fleet's owner" "$(post_code "$DRIVER/driver/fleet/driver/$NEW_DRIVER/update" "$OWN_TOKEN" '{"firstName":"Secondfleet"}')" 200
check "db: the new name is saved" "$(wait_sql "select first_name from atlas_driver_offer_bpp.person where id='$NEW_DRIVER'" Secondfleet 150)" Secondfleet
check "the city still forbids a fleet changing the driver's phone" "$(post_code "$DRIVER/driver/fleet/driver/$NEW_DRIVER/update" "$OWN_TOKEN" '{"mobileNo":"9000000001","mobileCountryCode":"+91"}')" 403
OTHER_TOKEN=$(login)
sleep 8
check "another owner cannot edit that driver" "$(post_code "$DRIVER/driver/fleet/driver/$NEW_DRIVER/update" "$OTHER_TOKEN" '{"firstName":"Nope"}')" 403

IMAGE=$(post "$DRIVER/driver/$SECOND_FLEET/document/upload" "$ADMIN_TOKEN" "{\"imageBase64\":\"$PIXEL\",\"imageType\":\"VehicleRegistrationCertificate\"}" | json "d['imageId']")
wait_sql "select count(*) from atlas_driver_offer_bpp.image where id='$IMAGE'" 1 150 >/dev/null
db "update atlas_driver_offer_bpp.image set verification_status='VALID' where id='$IMAGE'" >/dev/null
curl -s -m 10 -X POST "$MOCK/mock/redis/del" -H 'Content-Type: application/json' -d "{\"pattern\":\"*${IMAGE}*\",\"target\":\"cluster\"}" >/dev/null
PLATE="KA51EZ$(python3 -c 'import random;print(random.randint(1000,9999))')"
vehicle() { echo "{\"imageId\":\"$IMAGE\",\"registrationNo\":\"$PLATE\",\"vehicleClass\":\"3WT\",\"vehicleCategory\":\"AUTO_CATEGORY\",\"colour\":\"Yellow\",\"make\":\"Bajaj\",\"model\":\"RE\",\"energyType\":\"\",\"driverName\":\"Owner\",\"operatingCity\":\"Bangalore\",\"multipleRC\":true}"; }
ADD="$DRIVER/driver/$OWN_PHONE/fleet/addVehicle?mbRole=%22FLEET%22&fleetOwnerId"
check "naming a fleet the caller does not hold is refused" "$(post_code "$ADD=$STRANGER" "$OWN_TOKEN" "$(vehicle)")" 403
check "the owner's number adds a vehicle to the second fleet" "$(post_code "$ADD=$SECOND_FLEET" "$OWN_TOKEN" "$(vehicle)")" 200
check "db: the certificate belongs to the second fleet" "$(wait_sql "select fleet_owner_id from atlas_driver_offer_bpp.vehicle_registration_certificate where document_image_id='$IMAGE'" "$SECOND_FLEET" 150)" "$SECOND_FLEET"
set_mode OFF

echo
echo "passed: $PASS, failed: $FAIL"
[ "$FAIL" = 0 ]
