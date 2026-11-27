#!/usr/bin/env bash
# Checks for the joining-OTP routes (fleet and operator):
#   - adding a new driver answers promptly (it used to wait out the driver's own onboarding lock) and links them
#   - only a caller with the code sent to that driver changes anything: a retry, a wrong or made-up code, and a
#     code issued for another number all leave the driver's links as they are
#   - the standalone verify route still links a driver for a fleet owner
#
#   PATH=~/fleet-scope-test-tools/bin:$PATH Backend/dev/integration-tests/fleet-scope-join.sh
#
# The refusals matter most for a merchant whose overwriteAssociation is on, where joining ends the driver's
# current links. The app caches the merchant in memory for an hour, so to cover that case set
# atlas_driver_offer_bpp.merchant.overwrite_association and restart the driver app first. The script prints what
# the database says. Needs the same local stack as fleet-scope-extras.sh.
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/fleet-scope-lib.sh"

db() { psql -d atlas_dev -tAc "$1"; }
call() { # call URL TOKEN BODY: sets CODE, BODY and TOOK (whole seconds)
  local out start
  start=$(date +%s)
  out=$(curl -s -m 70 -w $'\n%{http_code}' -X POST "$1" -H "token: $2" -H 'Content-Type: application/json' -d "$3")
  TOOK=$(($(date +%s) - start))
  CODE=${out##*$'\n'}
  BODY=${out%$'\n'*}
}
field() { echo "$BODY" | json "$1"; }
refused_with() { # refused_with "label" ERROR_CODE
  if [ "$CODE" = 400 ] || [ "$CODE" = 404 ]; then check "$1 ($CODE $(field "d['errorCode']"))" "$(field "d['errorCode']")" "$2"; else bad "$1 (HTTP $CODE): $(echo "$BODY" | head -c 200)"; fi
}
joined_promptly() { # joined_promptly "label"
  if [ "$CODE" = 200 ] && [ "$TOOK" -le 10 ]; then ok "$1 (${TOOK}s)"; else bad "$1 (HTTP $CODE after ${TOOK}s): $(echo "$BODY" | head -c 200)"; fi
}
send_body() { echo "{\"mobileNumber\":\"$1\",\"mobileCountryCode\":\"+91\",\"merchantId\":\"$MERCHANT\",\"deviceToken\":\"join-$1\"}"; }
verify_body() { echo "{\"mobileNumber\":\"$1\",\"mobileCountryCode\":\"+91\",\"otp\":\"$2\",\"deviceToken\":\"join-$1\"}"; }
made_up() { python3 -c 'import uuid; print(uuid.uuid4())'; }
links() { # links TABLE COLUMN ID: the state of every link row for that fleet or operator, oldest first
  echo "select coalesce((select string_agg(case when is_active and associated_till > now() then 'ACTIVE' else 'ENDED' end, ',' order by created_at) from atlas_driver_offer_bpp.$1 where $2='$3'), 'none')"
}
fleet_links() { links fleet_driver_association fleet_owner_id "$1"; }
operator_links() { links driver_operator_association operator_id "$1"; }
SETTLE=${SETTLE:-100} # the local drainer has taken up to 98 s to copy a write to Postgres

echo "merchant overwriteAssociation in the database: $(db "select overwrite_association from atlas_driver_offer_bpp.merchant where short_id='$MERCHANT'")"
set_mode OFF

echo "== fleet: adding a new driver"
T1=$(login); T2=$(login); sleep 12
F1=$(person_of "$T1"); F2=$(person_of "$T2")
D=$(mobile)
call "$DRIVER/driver/fleet/sendJoiningOtp?fleetOwnerId=$F1" "$T1" "$(send_body "$D")"
AUTH=$(field "d['authId']")
sleep 8
call "$DRIVER/driver/fleet/verifyJoiningOtp?fleetOwnerId=$F1&authId=$AUTH" "$T1" "$(verify_body "$D" "$OTP")"
joined_promptly "the driver is added, without waiting on a lock"
check "db: the driver is on the fleet" "$(wait_sql "$(fleet_links "$F1")" ACTIVE 150)" ACTIVE
check "db: and is marked a fleet driver" "$(wait_sql "select onboarding_as from atlas_driver_offer_bpp.driver_information i join atlas_driver_offer_bpp.fleet_driver_association a on a.driver_id=i.driver_id where a.fleet_owner_id='$F1' limit 1" FLEET_DRIVER 150)" FLEET_DRIVER

echo "== fleet: requests that must change nothing"
call "$DRIVER/driver/fleet/verifyJoiningOtp?fleetOwnerId=$F1&authId=$AUTH" "$T1" "$(verify_body "$D" "$OTP")"
refused_with "the same request again is refused" AUTH_BLOCKED
call "$DRIVER/driver/fleet/verifyJoiningOtp?fleetOwnerId=$F2&authId=$(made_up)" "$T2" "$(verify_body "$D" 0000)"
refused_with "another fleet with a made-up authId and a wrong code" TOKEN_NOT_FOUND
# the driver asks for a login code in their own app, so a real, unused authId exists for them
call "http://localhost:8016/ui/auth" "" "{\"mobileNumber\":\"$D\",\"mobileCountryCode\":\"+91\",\"merchantId\":\"$(db "select id from atlas_driver_offer_bpp.merchant where short_id='$MERCHANT'")\"}"
PENDING=$(field "d['authId']")
sleep 3
call "$DRIVER/driver/fleet/verifyJoiningOtp?fleetOwnerId=$F2&authId=$PENDING" "$T2" "$(verify_body "$D" 0000)"
refused_with "another fleet with the driver's real authId and a wrong code" INVALID_AUTH_DATA
OTHER=$(mobile)
call "$DRIVER/driver/fleet/sendJoiningOtp?fleetOwnerId=$F2" "$T2" "$(send_body "$OTHER")"
OTHER_AUTH=$(field "d['authId']")
sleep 8
call "$DRIVER/driver/fleet/verifyJoiningOtp?fleetOwnerId=$F2&authId=$OTHER_AUTH" "$T2" "$(verify_body "$D" "$OTP")"
refused_with "another fleet using a code issued for a different number" INVALID_REQUEST
sleep "$SETTLE"
check "db: the driver is still on the first fleet, with no other link row" "$(db "$(fleet_links "$F1")")" ACTIVE
check "db: the second fleet got nobody" "$(db "$(fleet_links "$F2")")" none

echo "== operator: adding a new driver"
ROLE=$(db "select id from atlas_dashboard.role where name='OPERATOR'")
new_operator() { # prints a session token
  local p e
  p=$(mobile)
  e="join-op-$p@dashboard.com"
  curl -s -m 40 -o /dev/null -X POST "$DRIVER/operator/dashboard/register" -H "token: $ADMIN_TOKEN" -H 'Content-Type: application/json' \
    -d "{\"firstName\":\"Join\",\"lastName\":\"Operator\",\"roleId\":\"$ROLE\",\"email\":\"$e\",\"mobileNumber\":\"$p\",\"mobileCountryCode\":\"+91\",\"password\":\"Join@123456\",\"fleetAssignment\":null}"
  sleep 8
  curl -s -m 40 -X POST "$DASH/user/login" -H 'Content-Type: application/json' -d "{\"email\":\"$e\",\"password\":\"Join@123456\"}" | json "d['authToken']"
}
O1T=$(new_operator); O2T=$(new_operator); sleep 12
O1=$(person_of "$O1T"); O2=$(person_of "$O2T")
E=$(mobile)
call "$DRIVER/driver/operator/sendJoiningOtp" "$O1T" "$(send_body "$E")"
AUTH=$(field "d['authId']")
sleep 8
call "$DRIVER/driver/operator/verifyJoiningOtp?authId=$AUTH" "$O1T" "$(verify_body "$E" "$OTP")"
# The route texts the driver an app link last, so it answers 200 only where the city's SMS provider is reachable.
joined_promptly "the driver is added"
check "db: the driver is linked to the operator" "$(wait_sql "$(operator_links "$O1")" ACTIVE 150)" ACTIVE

echo "== operator: requests that must change nothing"
call "$DRIVER/driver/operator/verifyJoiningOtp?authId=$AUTH" "$O1T" "$(verify_body "$E" "$OTP")"
refused_with "the same request again is refused" AUTH_BLOCKED
call "$DRIVER/driver/operator/verifyJoiningOtp?authId=$(made_up)" "$O2T" "$(verify_body "$E" 0000)"
refused_with "another operator with a made-up authId and a wrong code" TOKEN_NOT_FOUND
OTHER=$(mobile)
call "$DRIVER/driver/operator/sendJoiningOtp" "$O2T" "$(send_body "$OTHER")"
OTHER_AUTH=$(field "d['authId']")
sleep 8
call "$DRIVER/driver/operator/verifyJoiningOtp?authId=$OTHER_AUTH" "$O2T" "$(verify_body "$E" "$OTP")"
refused_with "another operator using a code issued for a different number" INVALID_REQUEST
sleep "$SETTLE"
check "db: the driver is still linked to the first operator, with no other link row" "$(db "$(operator_links "$O1")")" ACTIVE
check "db: the second operator got nobody" "$(db "$(operator_links "$O2")")" none

echo "== the standalone verify route, for a fleet owner"
# The local fleet-owner role is not seeded with this route's capability, so it is granted for this part.
FLEET_ROLE=$(db "select id from atlas_dashboard.role where name='FLEET'")
CAP=city-operations.booth_customer.write
GRANTED=$(db "insert into atlas_dashboard.role_capability (role_id, capability_id) select '$FLEET_ROLE', '$CAP' where not exists (select 1 from atlas_dashboard.role_capability where role_id='$FLEET_ROLE' and capability_id='$CAP') returning 1" | grep -c '^1$')
revoke_cap() { [ "$GRANTED" = 1 ] && db "delete from atlas_dashboard.role_capability where role_id='$FLEET_ROLE' and capability_id='$CAP'" >/dev/null; }
trap 'revoke_cap; cleanup' EXIT
T3=$(login); sleep 12
F3=$(person_of "$T3")
S=$(mobile)
call "$DRIVER/driver/auth" "$T3" "$(send_body "$S")"
AUTH=$(field "d['authId']")
check "a code is sent ($CODE)" "$CODE" 200
sleep 8
call "$DRIVER/driver/$AUTH/verify" "$T3" "{\"otp\":\"$OTP\",\"deviceToken\":\"join-$S\"}"
joined_promptly "the standalone route verifies the driver"
check "db: the driver is on that fleet" "$(wait_sql "$(fleet_links "$F3")" ACTIVE 150)" ACTIVE
check "db: and is marked a fleet driver" "$(wait_sql "select onboarding_as from atlas_driver_offer_bpp.driver_information i join atlas_driver_offer_bpp.fleet_driver_association a on a.driver_id=i.driver_id where a.fleet_owner_id='$F3' limit 1" FLEET_DRIVER 150)" FLEET_DRIVER

echo
echo "passed: $PASS, failed: $FAIL"
[ "$FAIL" = 0 ]
