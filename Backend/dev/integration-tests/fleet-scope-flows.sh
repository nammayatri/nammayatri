#!/usr/bin/env bash
# End-to-end flows for a fleet owner and an operator, run once per fleet-scope mode and once per tree
# (the driver-app's direct routes, and provider-dashboard forwarding to the driver-app):
#
#   fleet owner: OTP register/login, profile, v2 register, view/update info, document upload/list/reject,
#                account approve/reject, password login, add/list/update driver, add/list vehicle,
#                link/unlink, remove vehicle, remove driver
#   operator:    admin creates the operator, password login, create/register/list fleet, link by OTP and unlink,
#                add driver, add vehicle for a linked fleet, hub request create/fetch/respond
#   ENFORCE:     a second owner and a second operator are refused the first one's resources
#
# Every step checks the response, and the writes are also checked in Postgres (the drainer is waited for).
# OTPs are the local fixed code and external verifications go to the local mock server.
#
#   PATH=~/fleet-scope-test-tools/bin:$PATH Backend/dev/integration-tests/fleet-scope-flows.sh [OFF ENFORCE ...]
#
# TREES="direct proxy" picks the trees and FLOWS="fleet operator" the flows. Needs the same local stack as fleet-scope-extras.sh.
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/fleet-scope-lib.sh"

MODES=${*:-OFF ENFORCE}
TREES=${TREES:-direct proxy}
ROOT=http://localhost:8016/direct-dashboard
# A 1x1 PNG.
PIXEL="iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg=="

db() { psql -d atlas_dev -tAc "$1"; }
# How long to wait for the local drainer to copy a write from Redis to Postgres (seconds; it has taken up to 98).
DB_WAIT=${DB_WAIT:-150}
uuid() { python3 -c 'import uuid; print(uuid.uuid4())'; }

call() { # call METHOD URL TOKEN [BODY]: sets CODE and BODY
  local out args=(-s -m 40 -X "$1" "$2" -w $'\n%{http_code}')
  [ -n "$3" ] && args+=(-H "token: $3")
  [ -n "${4:-}" ] && args+=(-H 'Content-Type: application/json' -d "$4")
  out=$(curl "${args[@]}")
  CODE=${out##*$'\n'}
  BODY=${out%$'\n'*}
}

expect() { # expect "label" CODE
  if [ "$CODE" = "$2" ]; then ok "$LABEL $1"; else bad "$LABEL $1 (HTTP $CODE, wanted $2): $(echo "$BODY" | head -c 300)"; fi
}

refused() { # refused "label": any 4xx counts, and the status and code are shown
  case "$CODE" in
    4*) ok "$LABEL $1 ($CODE $(echo "$BODY" | json "d.get('errorCode','')"))" ;;
    *) bad "$LABEL $1 (HTTP $CODE, wanted a refusal): $(echo "$BODY" | head -c 300)" ;;
  esac
}

wait_any() { # wait_any "<query>": prints the first non-empty answer, waiting for the drainer
  local got="" i
  for i in $(seq 1 "$DB_WAIT"); do
    got=$(db "$1")
    [ -n "$got" ] && break
    sleep 1
  done
  echo "$got"
}

# The local fleet-owner role is not seeded with the document capabilities (they are city-operations ones), so a
# fleet owner's own upload and list are refused here. They are granted for the run, before any login (capabilities
# are cached once read), and only the ones added here are taken back.
FLEET_ROLE=$(db "select id from atlas_dashboard.role where name='FLEET'")
GRANTED=""
for cap in city-operations.onboarding.read city-operations.onboarding.write; do
  if [ "$(db "select count(*) from atlas_dashboard.role_capability where role_id='$FLEET_ROLE' and capability_id='$cap'")" = 0 ]; then
    db "insert into atlas_dashboard.role_capability (role_id, capability_id) values ('$FLEET_ROLE', '$cap')" >/dev/null
    GRANTED="$GRANTED $cap"
  fi
done
revoke_caps() {
  for cap in $GRANTED; do db "delete from atlas_dashboard.role_capability where role_id='$FLEET_ROLE' and capability_id='$cap'" >/dev/null; done
}
SMS_CITY=$(db "select id from atlas_driver_offer_bpp.merchant_operating_city where merchant_short_id='$MERCHANT' and city='Bangalore'")
SMS_BEFORE=$(db "select sms_providers_priority_list from atlas_driver_offer_bpp.merchant_service_usage_config where merchant_operating_city_id='$SMS_CITY'")
sms_providers() { # sms_providers LIST: sets the city's SMS providers and drops the app's cached copy
  db "update atlas_driver_offer_bpp.merchant_service_usage_config set sms_providers_priority_list='$1' where merchant_operating_city_id='$SMS_CITY'" >/dev/null
  curl -s -m 10 -o /dev/null -X POST "$MOCK/mock/redis/del" -H 'Content-Type: application/json' -d "{\"pattern\":\"*MerchantServiceUsageConfig*\",\"target\":\"cluster\"}"
  curl -s -m 10 -o /dev/null -X POST "$MOCK/mock/redis/del" -H 'Content-Type: application/json' -d "{\"pattern\":\"*$SMS_CITY*\",\"target\":\"cluster\"}"
}
trap 'revoke_caps; sms_providers "$SMS_BEFORE"; set_mode OFF; cleanup' EXIT
sms_providers '{PinbixSms}'

drop_keys() { # drop_keys ID...: removes the app's Redis entries whose key mentions any of the ids
  local port k id
  for port in 30001 30002 30003 30004 30005 30006; do
    for id in "$@"; do
      [ -n "$id" ] || continue
      for k in $(redis-cli -p $port --scan --pattern "*$id*" 2>/dev/null); do redis-cli -c -p $port del "$k" >/dev/null 2>&1; done
    done
  done
}

upload() { # upload PERSON_ID TYPE TOKEN: sets IMAGE (and CODE, BODY)
  call POST "$API/driver/$1/document/upload" "$3" "{\"imageBase64\":\"$PIXEL\",\"imageType\":\"$2\"}"
  IMAGE=$(field "d['imageId']")
}

image_valid() { # image_valid IMAGE_ID: stands in for the outside image check
  [ -n "$1" ] || return
  wait_any "select id from atlas_driver_offer_bpp.image where id='$1'" >/dev/null
  db "update atlas_driver_offer_bpp.image set verification_status='VALID' where id='$1'" >/dev/null
  drop_keys "$1"
}

# v2 registration needs a validated PAN and Aadhaar. The image check is an outside service that is not run
# locally (an upload stays MANUAL_VERIFICATION_REQUIRED), so the images are marked VALID in Postgres; the PAN and
# Aadhaar verification itself goes through the app to the mock server.
verified_documents() { # verified_documents FLEET_ID TOKEN
  local pan front back pan_no aadhaar_no
  pan_no="ABCPF$(python3 -c 'import random;print(random.randint(1000,9999))')K"
  aadhaar_no="9$(python3 -c 'import random;print(random.randint(10000000000,99999999999))')"
  curl -s -m 10 -o /dev/null -X POST "$MOCK/idfy/configure" -H 'Content-Type: application/json' -d "{\"aadhaarNumber\":\"$aadhaar_no\",\"panNumber\":\"$pan_no\"}"
  upload "$1" PanCard "$2"; expect "the PAN image is uploaded" 200
  pan=$IMAGE
  PAN_IMAGE=$IMAGE
  upload "$1" AadhaarCard "$2"; front=$IMAGE
  upload "$1" AadhaarCard "$2"; expect "the Aadhaar images are uploaded" 200
  back=$IMAGE
  saved "the images are stored for the owner" "select count(*) from atlas_driver_offer_bpp.image where person_id='$1'" 3
  db "update atlas_driver_offer_bpp.image set verification_status='VALID' where person_id='$1'" >/dev/null
  drop_keys "$pan" "$front" "$back"
  call POST "$API/onboarding/verify/%22VERIFY_PAN%22" "$2" "{\"driverId\":\"$1\",\"identifierNumber\":\"$pan_no\",\"imageId\":\"$pan\"}"
  expect "the PAN is verified" 200
  call POST "$API/onboarding/verify/%22VERIFY_AADHAAR%22" "$2" "{\"driverId\":\"$1\",\"identifierNumber\":\"$aadhaar_no\",\"imageId\":\"$front\",\"optionalImageId\":\"$back\"}"
  expect "the Aadhaar is verified" 200
  saved "PAN and Aadhaar are VALID" "select (select verification_status from atlas_driver_offer_bpp.driver_pan_card where driver_id='$1' limit 1) || ',' || (select verification_status from atlas_driver_offer_bpp.aadhaar_card where driver_id='$1' limit 1)" "VALID,VALID"
}

GAPS=0
gap() { GAPS=$((GAPS + 1)); echo "  GAP $1"; }

field() { echo "$BODY" | json "$1"; }
has() { case "$BODY" in *"$1"*) echo yes ;; *) echo no ;; esac; }

listed_has() { # listed_has "label" URL TOKEN NEEDLE: lists are read from Postgres, so ask until the new row has drained
  local i
  for i in $(seq 1 $((DB_WAIT / 5))); do
    call GET "$2" "$3"
    [ "$CODE" = 200 ] && [ "$(has "$4")" = yes ] && break
    sleep 5
  done
  expect "$1: the list answers" 200
  check "$LABEL $1" "$(has "$4")" yes
}

saved() { # saved "label" "<query>" "<wanted>"
  check "$LABEL db: $1" "$(wait_sql "$2" "$3" "$DB_WAIT")" "$3"
}

# The two trees: $API is the merchant/city prefix, $USER_API the login tree.
use_tree() {
  if [ "$1" = proxy ]; then
    API="$DASH/bpp/driver-offer/$MERCHANT/Bangalore"
    USER_API="$DASH"
  else
    API="$DRIVER"
    USER_API="$ROOT"
  fi
}

new_owner() { # new_owner: sets OWNER_PHONE OWNER_TOKEN OWNER_ID, logging in on the current tree
  local attempt
  OWNER_PHONE=$(mobile)
  OWNER_TOKEN=""
  call POST "$API/fleet/v2/login/otp" "" "{\"mobileNumber\":\"$OWNER_PHONE\",\"mobileCountryCode\":\"+91\"}"
  LOGIN_CODE=$CODE
  for attempt in 1 2 3 4; do
    sleep 3
    call POST "$API/fleet/v2/verify/otp" "" "{\"mobileNumber\":\"$OWNER_PHONE\",\"mobileCountryCode\":\"+91\",\"otp\":\"$OTP\"}"
    OWNER_TOKEN=$(field "d['authToken']")
    [ -n "$OWNER_TOKEN" ] && break
  done
  OWNER_ID=$(person_of "$OWNER_TOKEN")
}

register_body() { # register_body PERSON_ID PHONE EMAIL
  echo "{\"firstName\":\"Flow\",\"lastName\":\"Owner\",\"fleetName\":\"Flow fleet $2\",\"mobileNumber\":\"$2\",\"mobileCountryCode\":\"+91\",\"city\":\"Bangalore\",\"email\":\"$3\",\"merchantId\":\"$MERCHANT\",\"personId\":\"$1\",\"fleetType\":\"NORMAL_FLEET\",\"address\":\"Bangalore\",\"addressState\":\"Karnataka\",\"addressDocumentType\":\"Passport\"}"
}

joined() { # a joining-OTP verification has to succeed without waiting on anything
  if [ "$CODE" = 200 ]; then ok "$LABEL $1"; else bad "$LABEL $1 (HTTP $CODE): $(echo "$BODY" | head -c 300)"; fi
}

# ---------------------------------------------------------------------------------------------------------------
fleet_flow() {
  echo "== [$LABEL] fleet owner: register, login, profile"
  new_owner
  check "$LABEL an OTP is requested" "$LOGIN_CODE" 200
  if [ -z "$OWNER_TOKEN" ]; then bad "$LABEL the OTP is verified and a session returned: $(echo "$BODY" | head -c 300)"; return; fi
  ok "$LABEL the OTP is verified and a session returned"
  F=$OWNER_ID F_TOKEN=$OWNER_TOKEN F_PHONE=$OWNER_PHONE
  F_EMAIL="flow-fleet-$F_PHONE@example.com"
  saved "the driver-app person is a fleet owner" "select role from atlas_driver_offer_bpp.person where id='$F'" FLEET_OWNER
  saved "the fleet row exists" "select count(*) from atlas_driver_offer_bpp.fleet_owner_information where fleet_owner_person_id='$F'" 1
  saved "the dashboard person exists" "select count(*) from atlas_dashboard.person where id='$F'" 1

  call POST "$API/fleet/v2/register" "$F_TOKEN" "$(register_body "$F" "$F_PHONE" "$F_EMAIL")"
  refused "registration without documents is refused"
  verified_documents "$F" "$F_TOKEN"
  PAN_IMAGE_F=$PAN_IMAGE
  call POST "$API/fleet/v2/register" "$F_TOKEN" "$(register_body "$F" "$F_PHONE" "$F_EMAIL")"
  expect "registration is completed" 200
  saved "the fleet name is kept" "select fleet_name from atlas_driver_offer_bpp.fleet_owner_information where fleet_owner_person_id='$F'" "Flow fleet $F_PHONE"

  call POST "$API/fleet/v2/login/otp" "" "{\"mobileNumber\":\"$F_PHONE\",\"mobileCountryCode\":\"+91\"}"
  expect "a registered owner asks for a login OTP" 200
  sleep 4
  call POST "$API/fleet/v2/verify/otp" "" "{\"mobileNumber\":\"$F_PHONE\",\"mobileCountryCode\":\"+91\",\"otp\":\"$OTP\"}"
  expect "and logs in with it" 200
  check "$LABEL the login is for the same person" "$(person_of "$(field "d['authToken']")")" "$F"
  call POST "$API/fleet/v2/verify/otp" "" "{\"mobileNumber\":\"$F_PHONE\",\"mobileCountryCode\":\"+91\",\"otp\":\"0000\"}"
  refused "a wrong OTP is refused"

  call GET "$USER_API/user/profile" "$F_TOKEN"
  expect "the profile is shown" 200
  check "$LABEL the profile is the owner's" "$(field "d['id']")" "$F"

  call GET "$API/driver/$F/fleetOwnerInfo" "$F_TOKEN"
  expect "fleet info is shown" 200
  call POST "$API/driver/$F/updateFleetOwnerInfo" "$F_TOKEN" '{"firstName":"Renamed"}'
  expect "fleet info is updated" 200
  saved "the new first name is kept" "select first_name from atlas_driver_offer_bpp.person where id='$F'" Renamed
  call POST "$API/driver/fleet/driver/$F/update" "$F_TOKEN" '{"lastName":"SelfUpdated"}'
  expect "the owner updates their own record on the fleet driver update route" 200
  saved "that change is kept" "select last_name from atlas_driver_offer_bpp.person where id='$F'" SelfUpdated

  echo "== [$LABEL] fleet owner: documents and account approval"
  call POST "$API/driver/$F/document/upload" "$F_TOKEN" "{\"imageBase64\":\"$PIXEL\",\"imageType\":\"AadhaarCard\"}"
  expect "a document is uploaded" 200
  IMAGE_ID=$(field "d['imageId']")
  [ -n "$IMAGE_ID" ] || IMAGE_ID=missing-image
  saved "the image row exists" "select count(*) from atlas_driver_offer_bpp.image where id='$IMAGE_ID' and person_id='$F'" 1
  call GET "$API/driver/$F/documents/list" "$F_TOKEN"
  expect "the owner lists their documents" 200
  call GET "$API/driver/$F/documents/list" "$ADMIN_TOKEN"
  expect "an admin lists the owner's documents" 200
  check "$LABEL the list holds the upload" "$(has "$IMAGE_ID")" yes
  call POST "$API/driver/documents/update" "$ADMIN_TOKEN" "{\"tag\":\"Reject\",\"contents\":{\"tag\":\"ImageDocuments\",\"contents\":{\"reason\":\"flow test\",\"documentImageId\":\"$IMAGE_ID\"}}}"
  expect "an admin rejects the document" 200
  saved "the image is marked invalid" "select verification_status from atlas_driver_offer_bpp.image where id='$IMAGE_ID'" INVALID

  call POST "$API/account/verifyAccount" "$ADMIN_TOKEN" "{\"status\":\"Approved\",\"fleetOwnerId\":\"$F\"}"
  expect "an admin approves the account" 200
  saved "the dashboard person is verified" "select verified from atlas_dashboard.person where id='$F'" t

  echo "== [$LABEL] fleet owner: password login"
  local password="Flow@12345xyz"
  call POST "$USER_API/admin/person/password/$F" "$ADMIN_TOKEN" "{\"newPassword\":\"$password\"}"
  expect "an admin sets a password" 200
  sleep 3
  call GET "$USER_API/user/profile" "$F_TOKEN"
  refused "the reset ends the owner's earlier session"
  call POST "$USER_API/user/login" "" "{\"email\":\"$F_EMAIL\",\"password\":\"$password\"}"
  check "$LABEL the temporary password has to be changed first" "$(field "d['errorCode']")" PASSWORD_CHANGE_REQUIRED
  local own_password="Flow@67890xyz"
  call POST "$USER_API/user/changePasswordAfterExpiry" "" "{\"email\":\"$F_EMAIL\",\"oldPassword\":\"$password\",\"newPassword\":\"$own_password\"}"
  expect "the owner sets their own password" 200
  password=$own_password
  call POST "$USER_API/user/login" "" "{\"email\":\"$F_EMAIL\",\"password\":\"$password\"}"
  expect "the owner logs in with the password" 200
  local pw_token
  pw_token=$(field "d['authToken']")
  F_TOKEN=$pw_token
  call GET "$API/driver/$F/fleetOwnerInfo" "$pw_token"
  expect "that session can read the fleet" 200
  call POST "$USER_API/user/login" "" "{\"email\":\"$F_EMAIL\",\"password\":\"wrong-Password@1\"}"
  refused "a wrong password is refused"

  echo "== [$LABEL] fleet owner: add, list, update a driver"
  # An owner's own lists: in ENFORCE the fleet may be named. Outside it the fleet is left out, as naming a fleet
  # with no member association has always been refused (fleet-scope-extras.sh section 7 checks that refusal).
  local own=""
  if [ "$MODE" = ENFORCE ]; then
    own="&fleetOwnerId=$F"
  else
    # Outside ENFORCE a fleet owner's own lists look the owner up as a named fleet, as before this change, which
    # needs a member-association row that registration does not create. Show the refusal, then add the row.
    call GET "$API/driver/fleet/driverAssociation?limit=10&offset=0" "$F_TOKEN"
    check "$LABEL as before, the owner's list needs a member association" "$(field "d['errorCode']")" PERSON_NOT_FOUND
    call POST "$API/operator/fleet/member/association/create" "$ADMIN_TOKEN" "{\"fleetMemberId\":\"$F\",\"fleetOwnerId\":\"$F\",\"enabled\":true,\"isFleetOwner\":true}"
    expect "an admin creates the owner's member association" 200
    saved "the member association exists" "select count(*) from atlas_driver_offer_bpp.fleet_member_association where fleet_member_id='$F' and fleet_owner_id='$F'" 1
  fi
  DRIVER_PHONE=$(mobile)
  call POST "$API/driver/fleet/sendJoiningOtp?fleetOwnerId=$F" "$F_TOKEN" "{\"mobileNumber\":\"$DRIVER_PHONE\",\"mobileCountryCode\":\"+91\",\"merchantId\":\"$MERCHANT\",\"deviceToken\":\"flow-$DRIVER_PHONE\"}"
  expect "a joining OTP is sent to a driver" 200
  local auth_id
  auth_id=$(field "d['authId']")
  sleep 4
  call POST "$API/driver/fleet/verifyJoiningOtp?fleetOwnerId=$F&authId=$auth_id" "$F_TOKEN" "{\"mobileNumber\":\"$DRIVER_PHONE\",\"mobileCountryCode\":\"+91\",\"otp\":\"$OTP\",\"deviceToken\":\"flow-$DRIVER_PHONE\"}"
  joined "the joining OTP is verified"
  D_ID=$(wait_any "select driver_id from atlas_driver_offer_bpp.fleet_driver_association where fleet_owner_id='$F' and is_active and associated_till > now() limit 1")
  [ -n "$D_ID" ] && ok "$LABEL db: the driver is on the fleet" || { bad "$LABEL db: the driver is on the fleet"; return; }
  listed_has "the new driver is listed" "$API/driver/fleet/driverAssociation?mbSearchString=$DRIVER_PHONE&limit=10&offset=0$own" "$F_TOKEN" "$D_ID"
  call GET "$API/driver/fleet/getAllDriver?mblimit=20&mboffset=0$own" "$F_TOKEN"
  expect "the all-drivers list answers" 200
  call POST "$API/driver/fleet/driver/$D_ID/update" "$F_TOKEN" '{"firstName":"FlowDriver"}'
  expect "the owner updates the driver" 200
  saved "the driver's new name is kept" "select first_name from atlas_driver_offer_bpp.person where id='$D_ID'" FlowDriver

  echo "== [$LABEL] fleet owner: add and list a vehicle"
  RC_NO="KA01FL$(python3 -c 'import random;print(random.randint(1000,9999))')"
  upload "$F" VehicleRegistrationCertificate "$F_TOKEN"; expect "the RC image is uploaded" 200
  RC_IMAGE=$IMAGE
  image_valid "$RC_IMAGE"
  call POST "$API/driver/$F_PHONE/fleet/addVehicle?mbRole=%22FLEET%22&fleetOwnerId=$F" "$F_TOKEN" "$(vehicle_body "$RC_NO")"
  expect "the owner adds a vehicle" 200
  [ -z "${DEBUG_FLOWS:-}" ] || echo "    add vehicle $RC_NO answered: $(echo "$BODY" | head -c 400)"
  RC_ID=$(wait_any "select id from atlas_driver_offer_bpp.vehicle_registration_certificate where fleet_owner_id='$F' limit 1")
  [ -n "$RC_ID" ] && ok "$LABEL db: the vehicle belongs to the fleet" || bad "$LABEL db: the vehicle belongs to the fleet"
  listed_has "the new vehicle is listed" "$API/driver/fleet/vehicleAssociation?Limit=10&Offset=0$own" "$F_TOKEN" "$RC_NO"
  call GET "$API/driver/fleet/getAllVehicle?mblimit=20&mboffset=0$own" "$F_TOKEN"
  expect "the all-vehicles list answers" 200

  existing_driver_flow

  if [ "$MODE" = ENFORCE ] && [ "$TREE" = direct ]; then
    other_owner_checks
  elif [ "$MODE" = ENFORCE ]; then
    # provider-dashboard forwards to the old routes, which add no per-fleet check; clients use the direct ones.
    echo "  note $LABEL another-owner checks are run on the direct tree only"
  fi

  echo "== [$LABEL] fleet owner: link, unlink, remove"
  make_linkable "$D_ID" "$RC_ID"
  call POST "$API/driver/fleet/linkRCWithDriver?fleetOwnerId=$F" "$F_TOKEN" "{\"driverMobileNumber\":\"$DRIVER_PHONE\",\"driverMobileCountryCode\":\"+91\",\"vehicleRegistrationNumber\":\"$RC_NO\"}"
  expect "the vehicle is linked to the driver" 200
  saved "an active driver-vehicle link exists" "select count(*) from atlas_driver_offer_bpp.driver_rc_association where driver_id='$D_ID' and rc_id='$RC_ID' and associated_till > now()" 1
  call POST "$API/driver/$D_ID/$RC_NO/fleet/unlink" "$F_TOKEN"
  expect "the vehicle is unlinked" 200
  saved "no active link remains" "select count(*) from atlas_driver_offer_bpp.driver_rc_association where driver_id='$D_ID' and rc_id='$RC_ID' and associated_till > now()" 0
  sleep 5
  call POST "$API/driver/$RC_NO/fleet/remove/vehicle?fleetOwnerId=$F" "$F_TOKEN"
  expect "the vehicle is removed from the fleet" 200
  saved "the vehicle no longer belongs to the fleet" "select count(*) from atlas_driver_offer_bpp.vehicle_registration_certificate where id='$RC_ID' and fleet_owner_id='$F'" 0
  call POST "$API/driver/$D_ID/fleet/remove/driver?fleetOwnerId=$F" "$F_TOKEN"
  expect "the driver is removed from the fleet" 200
  saved "the driver is no longer on the fleet" "select count(*) from atlas_driver_offer_bpp.fleet_driver_association where fleet_owner_id='$F' and driver_id='$D_ID' and is_active and associated_till > now()" 0

  echo "== [$LABEL] fleet owner: account rejection"
  call POST "$API/account/verifyAccount" "$ADMIN_TOKEN" "{\"status\":\"Rejected\",\"reason\":\"flow test\",\"fleetOwnerId\":\"$F\"}"
  expect "an admin rejects the account" 200
  saved "the rejection is recorded" "select verified from atlas_dashboard.person where id='$F'" f
}

# A driver who signed up in the driver app first. sendJoiningOtp answers without a real authId, and the
# verification is then sent without one: the other path through the handler, which takes the lock once.
existing_driver_flow() {
  echo "== [$LABEL] fleet owner: add a driver who already has an account"
  local phone auth merchant_id driver taken
  phone=$(mobile)
  merchant_id=$(db "select id from atlas_driver_offer_bpp.merchant where short_id='$MERCHANT'")
  call POST "http://localhost:8016/ui/auth" "" "{\"mobileNumber\":\"$phone\",\"mobileCountryCode\":\"+91\",\"merchantId\":\"$merchant_id\"}"
  expect "the driver starts signing up in the driver app" 200
  auth=$(field "d['authId']")
  call POST "http://localhost:8016/ui/auth/$auth/verify" "" "{\"otp\":\"$OTP\",\"deviceToken\":\"flow-$phone\"}"
  expect "and finishes" 200
  driver=$(wait_any "select entity_id from atlas_driver_offer_bpp.registration_token where id='$auth'")
  wait_any "select id from atlas_driver_offer_bpp.person where id='$driver'" >/dev/null
  call POST "$API/driver/fleet/sendJoiningOtp?fleetOwnerId=$F" "$F_TOKEN" "{\"mobileNumber\":\"$phone\",\"mobileCountryCode\":\"+91\",\"merchantId\":\"$MERCHANT\",\"deviceToken\":\"flow-$phone\"}"
  expect "the fleet sends the joining OTP" 200
  check "$LABEL it is told the driver already uses the app" "$(field "d['authId']")" ALREADY_USING_APPLICATION
  sleep 3
  taken=$(date +%s)
  call POST "$API/driver/fleet/verifyJoiningOtp?fleetOwnerId=$F" "$F_TOKEN" "{\"mobileNumber\":\"$phone\",\"mobileCountryCode\":\"+91\",\"otp\":\"$OTP\"}"
  taken=$(($(date +%s) - taken))
  expect "the joining OTP is verified" 200
  [ "$taken" -le 5 ] && ok "$LABEL and answers promptly (${taken}s)" || bad "$LABEL and answers promptly (took ${taken}s)"
  saved "the driver is on the fleet" "select count(*) from atlas_driver_offer_bpp.fleet_driver_association where fleet_owner_id='$F' and driver_id='$driver' and is_active and associated_till > now()" 1
  call POST "$API/driver/$driver/fleet/remove/driver?fleetOwnerId=$F" "$F_TOKEN"
  expect "and can be removed again" 200
  saved "the driver is off the fleet again" "select count(*) from atlas_driver_offer_bpp.fleet_driver_association where fleet_owner_id='$F' and driver_id='$driver' and is_active and associated_till > now()" 0
}

vehicle_body() { # vehicle_body REG_NO
  echo "{\"imageId\":\"${2:-$RC_IMAGE}\",\"registrationNo\":\"$1\",\"vehicleClass\":\"3WT\",\"vehicleCategory\":\"AUTO_CATEGORY\",\"colour\":\"Yellow\",\"make\":\"Bajaj\",\"model\":\"RE\",\"energyType\":\"\",\"driverName\":\"Flow\",\"operatingCity\":\"Bangalore\",\"multipleRC\":true}"
}

# Linking needs an approved fleet, an enabled and verified driver and a valid RC. Locally the document checks that
# would get them there need outside services, so the three flags are set in Postgres, and the KV copies the app
# reads first are dropped so it reads those rows.
make_linkable() {
  sleep 12
  db "update atlas_driver_offer_bpp.driver_information set enabled=true, verified=true, blocked=false where driver_id='$1'" >/dev/null
  db "update atlas_driver_offer_bpp.vehicle_registration_certificate set verification_status='VALID', approved=true where id='$2'" >/dev/null
  db "update atlas_driver_offer_bpp.fleet_owner_information set verified=true, enabled=true where fleet_owner_person_id='$F'" >/dev/null
  drop_kv "driverInformation_driverId_{$1}" "$1" "$2" "$F"
}

drop_kv() { # drop_kv ID...: removes the app's KV entries that mention any of the ids
  local port k v id
  for port in 30001 30002 30003 30004 30005 30006; do
    for k in $(redis-cli -p $port --scan --pattern 'driverInformation_*' 2>/dev/null; redis-cli -p $port --scan --pattern 'vehicleRegistrationCertificate_id_*' 2>/dev/null; redis-cli -p $port --scan --pattern 'fleetOwnerInformation_*' 2>/dev/null); do
      for id in "$@"; do
        case "$k" in *"$id"*) redis-cli -c -p $port del "$k" >/dev/null 2>&1 ;; esac
      done
    done
  done
}

# In ENFORCE a second owner gets nothing of the first one's: F, D_ID and RC_NO are still in place here.
other_owner_checks() {
  echo "== [$LABEL] another fleet owner is refused"
  new_owner
  local other=$OWNER_TOKEN other_id=$OWNER_ID
  sleep 8
  # A registered, working owner, so that what follows is refused for being another fleet's and for nothing else.
  verified_documents "$other_id" "$other"
  call POST "$API/fleet/v2/register" "$other" "$(register_body "$other_id" "$OWNER_PHONE" "flow-other-$OWNER_PHONE@example.com")"
  expect "the other owner registers" 200
  call GET "$API/driver/$other_id/fleetOwnerInfo" "$other"
  expect "and can read their own fleet" 200
  call POST "$API/driver/$F/document/upload" "$other" "{\"imageBase64\":\"$PIXEL\",\"imageType\":\"AadhaarCard\"}"
  refused "uploading a document as the fleet"
  call POST "$API/driver/$F/unlink/document/%22PanCard%22" "$other"
  refused "unlinking the fleet owner's PAN"
  call POST "$API/onboarding/verify/%22VERIFY_PAN%22" "$other" "{\"driverId\":\"$F\",\"identifierNumber\":\"ABCPF9999K\",\"imageId\":\"$PAN_IMAGE_F\"}"
  refused "verifying a PAN for the fleet owner"
  call POST "$API/driver/fleet/driver/$F/update" "$other" '{"firstName":"Taken"}'
  refused "updating the fleet owner on the fleet driver update route"
  call GET "$API/driver/$F/fleetOwnerInfo" "$other"
  refused "reading the fleet's info"
  call POST "$API/driver/$F/updateFleetOwnerInfo" "$other" '{"firstName":"Taken"}'
  refused "updating the fleet's info"
  call GET "$API/driver/$F/documents/list" "$other"
  case "$CODE" in
    200) gap "$LABEL another owner can list the fleet owner's documents: the route has no per-fleet check, only its capability (granted here for the run)" ;;
    *) refused "listing the fleet owner's documents" ;;
  esac
  call GET "$API/driver/fleet/driverAssociation?fleetOwnerId=$F&limit=10&offset=0" "$other"
  refused "listing the fleet's drivers"
  call GET "$API/driver/fleet/vehicleAssociation?Limit=10&Offset=0&fleetOwnerId=$F" "$other"
  refused "listing the fleet's vehicles"
  call POST "$API/driver/fleet/driver/$D_ID/update" "$other" '{"firstName":"Taken"}'
  refused "updating the fleet's driver"
  call POST "$API/driver/$OWNER_PHONE/fleet/addVehicle?mbRole=%22FLEET%22&fleetOwnerId=$F" "$other" "$(vehicle_body "KA01XX$(python3 -c 'import random;print(random.randint(1000,9999))')")"
  refused "adding a vehicle to the fleet"
  call POST "$API/driver/fleet/linkRCWithDriver?fleetOwnerId=$F" "$other" "{\"driverMobileNumber\":\"$DRIVER_PHONE\",\"driverMobileCountryCode\":\"+91\",\"vehicleRegistrationNumber\":\"$RC_NO\"}"
  refused "linking the fleet's vehicle"
  call POST "$API/driver/$D_ID/$RC_NO/fleet/unlink" "$other"
  refused "unlinking the fleet's vehicle"
  call POST "$API/driver/$RC_NO/fleet/remove/vehicle?fleetOwnerId=$F" "$other"
  refused "removing the fleet's vehicle"
  call POST "$API/driver/$D_ID/fleet/remove/driver?fleetOwnerId=$F" "$other"
  refused "removing the fleet's driver"
  sleep 90
  check "$LABEL db: the first fleet is unchanged (name, vehicle, driver)" \
    "$(db "select (select first_name from atlas_driver_offer_bpp.person where id='$F') || ',' || (select count(*) from atlas_driver_offer_bpp.vehicle_registration_certificate where fleet_owner_id='$F') || ',' || (select count(*) from atlas_driver_offer_bpp.fleet_driver_association where fleet_owner_id='$F' and is_active and associated_till > now()) || ',' || (select first_name from atlas_driver_offer_bpp.person where id='$D_ID')")" \
    "Renamed,1,1,FlowDriver"
}

# ---------------------------------------------------------------------------------------------------------------
new_operator() { # new_operator: an admin creates one and it logs in. Sets OP_ID OP_TOKEN OP_PHONE OP_EMAIL
  local password="Operator@12345"
  OP_PHONE=$(mobile)
  OP_EMAIL="flow-op-$OP_PHONE@dashboard.com"
  OP_TOKEN="" OP_ID=""
  call POST "$API/operator/dashboard/register" "$ADMIN_TOKEN" "{\"firstName\":\"Flow\",\"lastName\":\"Operator\",\"roleId\":\"$OPERATOR_ROLE\",\"email\":\"$OP_EMAIL\",\"mobileNumber\":\"$OP_PHONE\",\"mobileCountryCode\":\"+91\",\"password\":\"$password\",\"fleetAssignment\":null}"
  expect "an admin creates an operator" 200
  sleep 6
  call POST "$USER_API/user/login" "" "{\"email\":\"$OP_EMAIL\",\"password\":\"$password\"}"
  expect "the operator logs in with the password" 200
  OP_TOKEN=$(field "d['authToken']")
  OP_ID=$(person_of "$OP_TOKEN")
}

links() { # links FLEET_ID OPERATOR_ID: SQL counting the live links
  echo "select count(*) from atlas_driver_offer_bpp.fleet_operator_association where fleet_owner_id='$1' and operator_id='$2' and is_active and associated_till > now()"
}

operator_flow() {
  echo "== [$LABEL] operator: create, log in, profile"
  new_operator
  [ -n "$OP_TOKEN" ] || return
  local op=$OP_ID op_token=$OP_TOKEN op_phone=$OP_PHONE
  saved "the driver-app person is an operator" "select role from atlas_driver_offer_bpp.person where id='$op'" OPERATOR
  saved "the dashboard person has the operator role" "select r.name from atlas_dashboard.person p join atlas_dashboard.role r on r.id=p.role_id where p.id='$op'" OPERATOR
  call POST "$USER_API/user/login" "" "{\"email\":\"$OP_EMAIL\",\"password\":\"Wrong@Password1\"}"
  refused "a wrong password is refused"
  call GET "$USER_API/user/profile" "$op_token"
  expect "the profile is shown" 200
  check "$LABEL the profile is the operator's" "$(field "d['id']")" "$op"

  echo "== [$LABEL] operator: create and register a fleet"
  local before fleet_phone new_fleet
  before=$(db "select now()")
  fleet_phone=$(mobile)
  call POST "$API/operator/fleet/create" "$op_token" "{\"mobileNumber\":\"$fleet_phone\",\"mobileCountryCode\":\"+91\"}"
  expect "the operator creates a fleet" 200
  new_fleet=$(wait_any "select fleet_owner_person_id from atlas_driver_offer_bpp.fleet_owner_information where created_at > '$before' order by created_at desc limit 1")
  [ -n "$new_fleet" ] && ok "$LABEL db: the new fleet exists" || { bad "$LABEL db: the new fleet exists"; return; }
  sleep 6
  verified_documents "$new_fleet" "$ADMIN_TOKEN"
  call POST "$API/operator/fleet/register" "$op_token" "$(register_body "$new_fleet" "$fleet_phone" "flow-opfleet-$fleet_phone@example.com")"
  expect "the operator registers that fleet" 200
  saved "its name is kept" "select fleet_name from atlas_driver_offer_bpp.fleet_owner_information where fleet_owner_person_id='$new_fleet'" "Flow fleet $fleet_phone"
  saved "it is linked to the operator" "$(links "$new_fleet" "$op")" 1
  call GET "$API/operator/fleets?limit=20&offset=0" "$op_token"
  expect "the operator lists fleets" 200
  check "$LABEL the new fleet is listed" "$(has "$new_fleet")" yes

  echo "== [$LABEL] operator: link an existing fleet by OTP, then unlink it"
  new_owner
  local own_fleet=$OWNER_ID own_token=$OWNER_TOKEN own_phone=$OWNER_PHONE
  sleep 8
  verified_documents "$own_fleet" "$own_token"
  call POST "$API/fleet/v2/register" "$own_token" "$(register_body "$own_fleet" "$own_phone" "flow-linked-$own_phone@example.com")"
  expect "a second fleet registers on its own" 200
  call POST "$API/account/verifyAccount" "$ADMIN_TOKEN" "{\"status\":\"Approved\",\"fleetOwnerId\":\"$own_fleet\"}"
  expect "and is approved" 200
  sleep 8
  call POST "$API/operator/fleet/link/sendOtp" "$op_token" "{\"mobileNumber\":\"$own_phone\",\"mobileCountryCode\":\"+91\"}"
  expect "the operator sends the link OTP to the fleet's phone" 200
  sleep 4
  call POST "$API/operator/fleet/link/verifyOtp" "$op_token" "{\"fleetOwnerId\":\"$own_fleet\",\"otp\":\"0000\"}"
  refused "a wrong link OTP is refused"
  call POST "$API/operator/fleet/link/verifyOtp" "$op_token" "{\"fleetOwnerId\":\"$own_fleet\",\"otp\":\"$OTP\"}"
  expect "the operator verifies the OTP" 200
  saved "the fleet is linked" "$(links "$own_fleet" "$op")" 1
  sleep 6
  call GET "$API/operator/fleets?limit=20&offset=0" "$op_token"
  check "$LABEL the linked fleet is listed" "$(has "$own_fleet")" yes

  # What an operator may do to a linked fleet through the fleet routes is a matter of its role's grants. The local
  # operator role can read fleet drivers and vehicles but not write them.
  call GET "$API/driver/fleet/driverAssociation?limit=10&offset=0&fleetOwnerId=$new_fleet" "$op_token"
  operator_read "the operator reads a linked fleet's drivers"
  call POST "$API/driver/fleet/sendJoiningOtp?fleetOwnerId=$new_fleet" "$op_token" "{\"mobileNumber\":\"$(mobile)\",\"mobileCountryCode\":\"+91\",\"merchantId\":\"$MERCHANT\",\"deviceToken\":\"flow-op\"}"
  refused "adding a driver to a fleet is not granted to the local operator role"

  if [ "$MODE" = ENFORCE ] && [ "$TREE" = direct ]; then other_operator_checks; fi

  call POST "$API/operator/fleet/$own_fleet/unlink" "$op_token"
  expect "the operator unlinks the fleet" 200
  saved "the link has ended" "$(links "$own_fleet" "$op")" 0
  sleep 6
  call GET "$API/operator/fleets?isActive=true&limit=20&offset=0" "$op_token"
  check "$LABEL the unlinked fleet is no longer listed as active" "$(has "$own_fleet")" no

  echo "== [$LABEL] operator: own driver and an inspection request"
  local d_phone auth_id driver
  d_phone=$(mobile)
  call POST "$API/driver/operator/sendJoiningOtp" "$op_token" "{\"mobileNumber\":\"$d_phone\",\"mobileCountryCode\":\"+91\",\"merchantId\":\"$MERCHANT\",\"deviceToken\":\"flow-$d_phone\"}"
  expect "the operator sends a joining OTP to a driver" 200
  auth_id=$(field "d['authId']")
  sleep 4
  call POST "$API/driver/operator/verifyJoiningOtp?authId=$auth_id" "$op_token" "{\"mobileNumber\":\"$d_phone\",\"mobileCountryCode\":\"+91\",\"otp\":\"$OTP\",\"deviceToken\":\"flow-$d_phone\"}"
  joined "the operator verifies the driver's joining OTP"
  driver=$(wait_any "select driver_id from atlas_driver_offer_bpp.driver_operator_association where operator_id='$op' and is_active and associated_till > now() limit 1")
  [ -n "$driver" ] && ok "$LABEL db: the driver is linked to the operator" || { bad "$LABEL db: the driver is linked to the operator"; return; }
  listed_has "the driver is listed" "$API/driver/operator/list?limit=20&offset=0" "$op_token" "$driver"

  call GET "$API/driver/operation/getAllHubs" "$op_token"
  expect "operation hubs are listed" 200
  local hub request
  hub=$(field "d[0]['id']")
  call POST "$API/driver/operator/createRequest" "$op_token" "{\"creatorId\":\"$op\",\"driverId\":\"$driver\",\"operationHubId\":\"$hub\",\"requestType\":\"DRIVER_ONBOARDING_INSPECTION\"}"
  expect "a hub request is created" 200
  request=$(wait_any "select id from atlas_driver_offer_bpp.operation_hub_requests where driver_id='$driver' order by created_at desc limit 1")
  [ -n "$request" ] && ok "$LABEL db: the request is pending" || { bad "$LABEL db: the request is stored"; return; }
  listed_has "the request is fetched by the driver's phone" "$API/driver/operator/fetch/hubRequests?mbLimit=10&mbOffset=0&mbStatus=%22PENDING%22&mbReqType=%22DRIVER_ONBOARDING_INSPECTION%22&mbDriverMobileNumber=$d_phone" "$op_token" "$request"
  # Rejection, since approval depends on the city's inspection documents being in place.
  call POST "$API/driver/operator/respond/hubRequest" "$op_token" "{\"operationHubRequestId\":\"$request\",\"operatorId\":\"$op\",\"status\":\"REJECTED\",\"remarks\":\"flow test\"}"
  expect "the operator responds to the request" 200
  saved "the response is recorded" "select request_status from atlas_driver_offer_bpp.operation_hub_requests where id='$request'" REJECTED

  echo "== [$LABEL] operator: register an operator person by phone"
  local extra_phone
  extra_phone=$(mobile)
  before=$(db "select now()")
  call POST "$API/operator/register" "$ADMIN_TOKEN" "{\"firstName\":\"Flow\",\"lastName\":\"Second\",\"mobileNumber\":\"$extra_phone\",\"mobileCountryCode\":\"+91\"}"
  expect "an admin registers an operator by phone" 200
}

operator_read() {
  case "$CODE" in
    200) ok "$LABEL $1" ;;
    *) if [ "$MODE" != ENFORCE ] && [ "$(field "d['errorCode']")" = PERSON_NOT_FOUND ]; then
      ok "$LABEL as before, naming a fleet with no member association is refused outside ENFORCE"
    else
      bad "$LABEL $1 (HTTP $CODE): $(echo "$BODY" | head -c 300)"
    fi ;;
  esac
}

# In ENFORCE an operator that is not linked gets nothing of either fleet; own_fleet and new_fleet belong to `op`.
other_operator_checks() {
  echo "== [$LABEL] an unrelated operator is refused"
  new_operator
  local other=$OP_TOKEN
  [ -n "$other" ] || return
  call GET "$API/operator/fleets?limit=50&offset=0" "$other"
  check "$LABEL its fleet list holds neither fleet" "$(has "$own_fleet")$(has "$new_fleet")" nono
  call GET "$API/driver/fleet/driverAssociation?limit=10&offset=0&fleetOwnerId=$own_fleet" "$other"
  refused "listing a fleet's drivers"
  call GET "$API/driver/fleet/vehicleAssociation?Limit=10&Offset=0&fleetOwnerId=$own_fleet" "$other"
  refused "listing a fleet's vehicles"
  call POST "$API/operator/fleet/$own_fleet/unlink" "$other"
  refused "unlinking a fleet that is not its own"
  call GET "$API/driver/$own_fleet/fleetOwnerInfo" "$other"
  refused "reading a fleet's info"
  call POST "$API/operator/fleet/member/association/create" "$other" "{\"fleetMemberId\":\"$OP_ID\",\"fleetOwnerId\":\"$own_fleet\",\"enabled\":true,\"isFleetOwner\":false}"
  refused "linking a member to a fleet that is not its own"
  call POST "$API/driver/fleet/driver/$own_fleet/update" "$other" '{"firstName":"Taken"}'
  refused "updating a fleet owner"
  sleep 8
  check "$LABEL db: the first operator's link is untouched" "$(db "$(links "$own_fleet" "$op")")" 1
}

OPERATOR_ROLE=$(db "select id from atlas_dashboard.role where name='OPERATOR'")

for MODE in $MODES; do
  for TREE in $TREES; do
    set_mode "$MODE"
    use_tree "$TREE"
    LABEL="$MODE/$TREE"
    for flow in ${FLOWS:-fleet operator}; do "${flow}_flow"; done
  done
done
set_mode OFF

echo
echo "passed: $PASS, failed: $FAIL, access gaps seen: $GAPS"
[ "$FAIL" = 0 ]
