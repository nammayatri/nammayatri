#!/usr/bin/env bash
# The new flow, pushed harder than the extras script does, and the old flow checked for what must not change:
#
#   8.  rejecting and approving a fleet, on the direct tree and on the proxied one
#   9.  an owner holding two fleets: what the lists show for no selection, for each selection, for a named fleet
#   10. pages, with and without a city filter
#   11. the city filter's edge cases, and that a city the caller cannot reach stays out of reach outside ENFORCE
#   12. backward compatibility: requests as an old client sends them, in OFF
#
# Needs the local stack up (driver-app on 8016, provider-dashboard on 8018, mock-server on 8080) and the
# fleet-scope migrations applied. The script switches the merchant's mode as it goes, restores it afterwards,
# and removes the fleets and persons it created. Run it from anywhere:
#
#   PATH=~/fleet-scope-test-tools/bin:$PATH bash Backend/dev/integration-tests/fleet-scope-review.sh
#
# Needs: curl, python3, psql (the wrapper above) and redis-cli (inside `nix develop .#backend`).
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/fleet-scope-lib.sh"

DASH_MERCHANT=$(psql -d atlas_dev -tAc "select id from atlas_dashboard.merchant where short_id='$MERCHANT'")
FLEET_ROLE=$(psql -d atlas_dev -tAc "select id from atlas_dashboard.role where name='FLEET'")
# The driver-registration routes are mapped to a capability the seeded fleet roles do not hold, and an ungranted
# route is refused before the handler runs: once by the person's own role and once by the role on the assignment.
# Grant it to both for this run, before any owner logs in (a person's capabilities are cached once read), and take
# back only what was added here.
ONBOARDING_CAP=city-operations.onboarding.write
OWNER_ROLE=$(psql -d atlas_dev -tAc "select id from atlas_dashboard.role where dashboard_access_type='FLEET_OWNER'")
ADDED_ONBOARDING_ROLES=""
for role in "$OWNER_ROLE" "$FLEET_ROLE"; do
  if [ "$(psql -d atlas_dev -tAc "select count(*) from atlas_dashboard.role_capability where role_id='$role' and capability_id='$ONBOARDING_CAP'")" = 0 ]; then
    psql -d atlas_dev -tA >/dev/null -c "insert into atlas_dashboard.role_capability (role_id, capability_id) values ('$role', '$ONBOARDING_CAP')"
    ADDED_ONBOARDING_ROLES="$ADDED_ONBOARDING_ROLES $role"
  fi
done
revoke_onboarding_cap() { for role in $ADDED_ONBOARDING_ROLES; do psql -d atlas_dev -tA >/dev/null -c "delete from atlas_dashboard.role_capability where role_id='$role' and capability_id='$ONBOARDING_CAP'"; done; }
trap 'revoke_onboarding_cap; cleanup' EXIT
ADMIN_ID=$(psql -d atlas_dev -tAc "select person_id from atlas_dashboard.registration_token where token='$ADMIN_TOKEN'")
BLR=$(psql -d atlas_dev -tAc "select id from atlas_driver_offer_bpp.merchant_operating_city where merchant_short_id='$MERCHANT' and city='Bangalore'")
CHN=$(psql -d atlas_dev -tAc "select id from atlas_driver_offer_bpp.merchant_operating_city where merchant_short_id='$MERCHANT' and city='Chennai'")
LEGACY_TOKEN=${LEGACY_TOKEN:-some-secret-dashboard-token-for-driver-offer-bpp}
LEGACY=http://localhost:8016/dashboard/$MERCHANT/Bangalore
PD_API=$DASH/bpp/driver-offer/$MERCHANT/Bangalore

# A vehicle for a fleet in a city, copied from an existing certificate (their encrypted numbers decrypt).
# $3 is how many days ago it was made, so that lists have a stable order.
add_vehicle() {
  psql -d atlas_dev -tA >/dev/null <<SQL
INSERT INTO atlas_driver_offer_bpp.vehicle_registration_certificate SELECT r.* FROM
  (SELECT * FROM atlas_driver_offer_bpp.vehicle_registration_certificate WHERE fleet_owner_id IS NULL ORDER BY id LIMIT 1) v
  CROSS JOIN LATERAL jsonb_populate_record(null::atlas_driver_offer_bpp.vehicle_registration_certificate,
    to_jsonb(v) || jsonb_build_object('id', gen_random_uuid()::text, 'fleet_owner_id','$1', 'merchant_operating_city_id','$2',
      'certificate_number_hash', '\\x' || md5(random()::text), 'verification_status','VALID',
      'created_at', to_char(now() - interval '$3 days', 'YYYY-MM-DD"T"HH24:MI:SS"Z"'))) r;
SQL
}

# The rows of a list as "fleet:city" pairs, sorted, with the fleets named by a letter (A, B, and C for the other owner).
rows_of() { # rows_of <url> <token>  ->  e.g. A:Bangalore,A:Chennai,B:Bangalore
  curl -s -m 30 "$1" -H "token: $2" | FLEETMAP="$FLEET_A=A,${FLEET_B:-b}=B,${OTHER_ID:-c}=C" json "','.join(sorted(
    dict(p.split('=') for p in __import__('os').environ['FLEETMAP'].split(',')).get(i.get('fleetOwnerId'), '?') + ':' + (i.get('cityName') or '-')
    for i in d['listItem']))"
}

# Outside ENFORCE the old lookups read the fleet member association, as on main. This gives a fleet the row that
# main needs to find it, and takes it away again.
add_member_row() { psql -d atlas_dev -tA >/dev/null -c "insert into atlas_driver_offer_bpp.fleet_member_association (fleet_member_id, fleet_owner_id, enabled, is_fleet_owner) values ('$1', '$1', true, true)"; sleep 6; }
drop_member_row() { psql -d atlas_dev -tA >/dev/null -c "delete from atlas_driver_offer_bpp.fleet_member_association where fleet_member_id='$1' and fleet_owner_id='$1'"; sleep 6; }

reject_body() { echo "{\"status\":\"Rejected\",\"reason\":\"review script\",\"fleetOwnerId\":\"$1\"}"; }
approve_body() { echo "{\"status\":\"Approved\",\"fleetOwnerId\":\"$1\"}"; }

echo "== 8. rejecting and approving a fleet"
for TREE in direct proxied; do
  if [ "$TREE" = direct ]; then REVIEW="$DRIVER/account/verifyAccount"; else REVIEW="$PD_API/account/verifyAccount"; fi
  OWNER_PHONE=$(mobile)
  OWNER_TOKEN=$(login_phone "$OWNER_PHONE")
  OWNER_ID=$(person_of "$OWNER_TOKEN")
  sleep 12
  # The admin is given a role on the fleet too, so that closing the fleet's access can be seen.
  psql -d atlas_dev -tA >/dev/null <<SQL
INSERT INTO atlas_dashboard.person_role_assignment (id, person_id, role_id, scope_type, scope_id, merchant_id, status)
VALUES (gen_random_uuid()::text, '$ADMIN_ID', '$FLEET_ROLE', 'FLEET', '$OWNER_ID', '$DASH_MERCHANT', 'ACTIVE');
SQL
  check "$TREE: the owner's session works" "$(status "$DASH/user/profile" -H "token: $OWNER_TOKEN")" 200
  check "$TREE: the owner holds their own fleet" "$(psql -d atlas_dev -tAc "select count(*) from atlas_dashboard.person_role_assignment where scope_id='$OWNER_ID' and person_id='$OWNER_ID' and status='ACTIVE'")" 1
  check "$TREE: reject the fleet" "$(status -X POST "$REVIEW" -H "token: $ADMIN_TOKEN" -H 'Content-Type: application/json' -d "$(reject_body "$OWNER_ID")")" 200
  AFTER=$(status "$DASH/user/profile" -H "token: $OWNER_TOKEN")
  if [ "$AFTER" = 401 ] || [ "$AFTER" = 403 ]; then ok "$TREE: the owner's session is refused ($AFTER)"; else bad "$TREE: the owner's session is refused (got $AFTER)"; fi
  check "$TREE: the owner is marked unverified" "$(psql -d atlas_dev -tAc "select verified from atlas_dashboard.person where id='$OWNER_ID'")" f
  check "$TREE: the reason is kept" "$(psql -d atlas_dev -tAc "select rejection_reason from atlas_dashboard.person where id='$OWNER_ID'")" "review script"
  check "$TREE: who rejected is kept" "$(psql -d atlas_dev -tAc "select rejected_by from atlas_dashboard.person where id='$OWNER_ID'")" "$ADMIN_ID"
  check "$TREE: when it happened is kept" "$(psql -d atlas_dev -tAc "select rejected_at is not null from atlas_dashboard.person where id='$OWNER_ID'")" t
  check "$TREE: the account is not deleted" "$(psql -d atlas_dev -tAc "select count(*) from atlas_dashboard.person where id='$OWNER_ID'")" 1
  check "$TREE: no session is left" "$(psql -d atlas_dev -tAc "select count(*) from atlas_dashboard.registration_token where person_id='$OWNER_ID'")" 0
  check "$TREE: every role on the fleet is revoked" "$(psql -d atlas_dev -tAc "select count(*) from atlas_dashboard.person_role_assignment where scope_id='$OWNER_ID' and status <> 'REVOKED'")" 0
  check "$TREE: rejecting again is harmless" "$(status -X POST "$REVIEW" -H "token: $ADMIN_TOKEN" -H 'Content-Type: application/json' -d "$(reject_body "$OWNER_ID")")" 200
  check "$TREE: and nothing comes back to life" "$(psql -d atlas_dev -tAc "select count(*) from atlas_dashboard.person_role_assignment where scope_id='$OWNER_ID' and status <> 'REVOKED'")" 0
  check "$TREE: the fleet is disabled in the driver-app" "$(wait_sql "select enabled from atlas_driver_offer_bpp.fleet_owner_information where fleet_owner_person_id='$OWNER_ID'" f 90)" f
  # The rejected owner coming back with a phone OTP gets a session, but the fleet's data stays closed to them.
  BACK_TOKEN=$(login_phone "$OWNER_PHONE")
  [ -n "$BACK_TOKEN" ] && ok "$TREE: the rejected owner can still open a session" || bad "$TREE: the rejected owner can still open a session"
  set_mode ENFORCE
  check "$TREE: but the fleet's data is closed to them (ENFORCE)" "$(status "$DRIVER/driver/fleet/vehicleAssociation?Limit=10&Offset=0" -H "token: $BACK_TOKEN")" 403
  set_mode OFF
  check "$TREE: approve the fleet again" "$(status -X POST "$REVIEW" -H "token: $ADMIN_TOKEN" -H 'Content-Type: application/json' -d "$(approve_body "$OWNER_ID")")" 200
  check "$TREE: the owner is verified again" "$(psql -d atlas_dev -tAc "select verified from atlas_dashboard.person where id='$OWNER_ID'")" t
  check "$TREE: approving again is harmless" "$(status -X POST "$REVIEW" -H "token: $ADMIN_TOKEN" -H 'Content-Type: application/json' -d "$(approve_body "$OWNER_ID")")" 200
  check "$TREE: who approved is kept" "$(psql -d atlas_dev -tAc "select approved_by from atlas_dashboard.person where id='$OWNER_ID'")" "$ADMIN_ID"
  check "$TREE: the owner has their own fleet back" "$(psql -d atlas_dev -tAc "select count(*) from atlas_dashboard.person_role_assignment where scope_id='$OWNER_ID' and person_id='$OWNER_ID' and status='ACTIVE'")" 1
  check "$TREE: someone else's role on the fleet stays revoked until they are invited again" "$(psql -d atlas_dev -tAc "select count(*) from atlas_dashboard.person_role_assignment where scope_id='$OWNER_ID' and person_id='$ADMIN_ID' and status='ACTIVE'")" 0
  set_mode ENFORCE
  check "$TREE: and can reach their data again (ENFORCE)" "$(status "$DRIVER/driver/fleet/vehicleAssociation?Limit=10&Offset=0" -H "token: $BACK_TOKEN")" 200
  set_mode OFF
  # A person from another merchant, or no person at all, cannot be reviewed.
  check "$TREE: a person that does not exist is refused" "$(status -X POST "$REVIEW" -H "token: $ADMIN_TOKEN" -H 'Content-Type: application/json' -d "$(reject_body 00000000-0000-4000-8000-000000000000)")" 400
done

echo "== 9. an owner holding two fleets, in ENFORCE"
OWNER_PHONE=$(mobile)
OWNER_TOKEN=$(login_phone "$OWNER_PHONE")
FLEET_A=$(person_of "$OWNER_TOKEN")
FLEET_B=$(curl -s -m 20 -X POST "$DRIVER/fleet/v2/create" -H "token: $OWNER_TOKEN" -H 'Content-Type: application/json' \
  -d "{\"fleetName\":\"Review fleet B\",\"fleetType\":\"NORMAL_FLEET\",\"contactMobileCountryCode\":\"+91\",\"contactMobileNumber\":\"$(mobile)\",\"contactEmail\":\"review-b@example.com\"}" | json "d['fleetId']")
OTHER_TOKEN=$(login)
OTHER_ID=$(person_of "$OTHER_TOKEN")
[ -n "$FLEET_A" ] && [ -n "$FLEET_B" ] && [ -n "$OTHER_ID" ] && ok "two owners, one holding fleets $FLEET_A and $FLEET_B" || { bad "set up the owners"; exit 1; }
sleep 12
add_vehicle "$FLEET_A" "$BLR" 1
add_vehicle "$FLEET_A" "$CHN" 2
add_vehicle "$FLEET_B" "$BLR" 3
add_vehicle "$OTHER_ID" "$BLR" 4
# A fleet-bound caller sees only cities they have access to, so the owner is given Chennai for the ENFORCE checks.
psql -d atlas_dev -tA >/dev/null <<SQL
INSERT INTO atlas_dashboard.merchant_access SELECT r.* FROM atlas_dashboard.merchant_access m
  CROSS JOIN LATERAL jsonb_populate_record(null::atlas_dashboard.merchant_access,
    to_jsonb(m) || jsonb_build_object('id', gen_random_uuid()::text, 'operating_city', 'Chennai')) r
  WHERE m.person_id='$FLEET_A' AND m.operating_city='Bangalore';
SQL
set_mode ENFORCE
LIST="$DRIVER/driver/fleet/vehicleAssociation?Limit=10&Offset=0"
check "with no selection the list covers every fleet held, in every city" "$(rows_of "$LIST" "$OWNER_TOKEN")" "A:Bangalore,A:Chennai,B:Bangalore"
check "a city narrows all of them" "$(rows_of "$LIST&cities=Bangalore" "$OWNER_TOKEN")" "A:Bangalore,B:Bangalore"
SEL="$DRIVER/driver/fleet/v2/access/select"
status -X POST "$SEL?enable=true&onlyCurrent=true&fleetOwnerId=$FLEET_A" -H "token: $OWNER_TOKEN" >/dev/null
check "with fleet A selected only A shows" "$(rows_of "$LIST" "$OWNER_TOKEN")" "A:Bangalore,A:Chennai"
status -X POST "$SEL?enable=true&onlyCurrent=true&fleetOwnerId=$FLEET_B" -H "token: $OWNER_TOKEN" >/dev/null
check "with fleet B selected only B shows" "$(rows_of "$LIST" "$OWNER_TOKEN")" "B:Bangalore"
check "a selected fleet and a city together" "$(rows_of "$LIST&cities=Chennai" "$OWNER_TOKEN")" ""
check "a held fleet that is not selected cannot be named" "$(status "$LIST&fleetOwnerId=$FLEET_A" -H "token: $OWNER_TOKEN")" 403
status -X POST "$SEL?enable=true&fleetOwnerId=$FLEET_A" -H "token: $OWNER_TOKEN" >/dev/null
check "with both selected, naming one shows that fleet" "$(rows_of "$LIST&fleetOwnerId=$FLEET_A" "$OWNER_TOKEN")" "A:Bangalore,A:Chennai"
status -X POST "$SEL?enable=true&onlyCurrent=true&fleetOwnerId=$FLEET_B" -H "token: $OWNER_TOKEN" >/dev/null
check "naming a fleet that is not held is refused" "$(status "$LIST&fleetOwnerId=$OTHER_ID" -H "token: $OWNER_TOKEN")" 403
check "the other owner cannot name this owner's fleet" "$(status "$LIST&fleetOwnerId=$FLEET_A" -H "token: $OTHER_TOKEN")" 403
check "the other owner sees only their own vehicles" "$(curl -s -m 30 "$LIST" -H "token: $OTHER_TOKEN" | json "len(d['listItem'])")" 1
check "an admin can name the fleet; they see the city in the URL, as before (direct tree, ENFORCE)" "$(rows_of "$LIST&fleetOwnerId=$FLEET_A" "$ADMIN_TOKEN")" "A:Bangalore"
check "an admin's city filter is held to the cities they can reach" "$(status "$LIST&fleetOwnerId=$FLEET_A&cities=Atlantis" -H "token: $ADMIN_TOKEN")" 403
SPOOF="mbRequestorId=$FLEET_A&isRequestorFleerOwner=true&hasFleetMemberHierarchy=false"
check "the requestor fields a client sends are not believed" "$(rows_of "$LIST&$SPOOF" "$OTHER_TOKEN")" "C:Bangalore"
check "not even with the owner's fleet named too" "$(status "$LIST&$SPOOF&fleetOwnerId=$FLEET_A" -H "token: $OTHER_TOKEN")" 403

echo "== 10. pages"
status -X POST "$SEL?enable=true&fleetOwnerId=$FLEET_A" -H "token: $OWNER_TOKEN" >/dev/null # A and B selected: all held
PAGE() { curl -s -m 30 "$DRIVER/driver/fleet/vehicleAssociation?Limit=1&Offset=$1$2" -H "token: $OWNER_TOKEN" | json "';'.join(i['fleetOwnerId'] + ':' + (i.get('cityName') or '-') for i in d['listItem'])"; }
P0=$(PAGE 0 ""); P1=$(PAGE 1 ""); P2=$(PAGE 2 ""); P3=$(PAGE 3 "")
[ -n "$P0" ] && [ -n "$P1" ] && [ -n "$P2" ] && ok "three pages of one row each" || bad "three pages of one row each ($P0 | $P1 | $P2)"
check "a page past the end is empty" "$P3" ""
check "the pages together are the whole list" "$(printf '%s\n' "$P0" "$P1" "$P2" | sort | uniq | wc -l | tr -d ' ')" 3
Q0=$(PAGE 0 "&cities=Bangalore"); Q1=$(PAGE 1 "&cities=Bangalore"); Q2=$(PAGE 2 "&cities=Bangalore")
[ -n "$Q0" ] && [ -n "$Q1" ] && [ "$Q0" != "$Q1" ] && ok "two pages with a city filter, different rows" || bad "two pages with a city filter ($Q0 | $Q1)"
check "and no third page" "$Q2" ""

echo "== 11. the city filter, at its edges"
set_mode ENFORCE
rowsC() { rows_of "$LIST$1" "$OWNER_TOKEN"; }
ALL="A:Bangalore,A:Chennai,B:Bangalore"
check "an empty list of cities is the default" "$(rowsC "&cities=")" "$ALL"
check "a list of only commas is the default too" "$(rowsC "&cities=,,")" "$ALL"
check "a trailing comma is ignored" "$(rowsC "&cities=Chennai,")" "A:Chennai"
check "spaces around a name are ignored" "$(rowsC "&cities=%20Chennai%20")" "A:Chennai"
check "a city named twice is listed once" "$(rowsC "&cities=Chennai,Chennai")" "A:Chennai"
check "a name and a std code can be mixed" "$(rowsC "&cities=Chennai,std:080")" "A:Bangalore,A:Chennai,B:Bangalore"
check "a name in other case is read" "$(rowsC "&cities=chennai")" "A:Chennai"
check "a city with no vehicles gives an empty list, not an error" "$(rowsC "&cities=Hyderabad")" ""
check "a city the merchant does not operate in is refused" "$(status "$LIST&cities=Atlantis" -H "token: $OWNER_TOKEN")" 403
check "one unknown city among known ones refuses the lot" "$(status "$LIST&cities=Chennai,Atlantis" -H "token: $OWNER_TOKEN")" 403
# Outside ENFORCE a city is reachable only if the caller has access to it.
psql -d atlas_dev -tA >/dev/null -c "delete from atlas_dashboard.merchant_access where person_id='$FLEET_A' and operating_city='Chennai'"
set_mode OFF
LEGACY_LIST="$PD_API/driver/fleet/vehicleAssociation?Limit=10&Offset=0"
check "a fleet made the new way has no member association" "$(psql -d atlas_dev -tAc "select count(*) from atlas_driver_offer_bpp.fleet_member_association where fleet_owner_id='$FLEET_A'")" 0
check "OFF: without a member association the old list refuses the fleet, as before" "$(curl -s -m 30 "$LEGACY_LIST&cities=Bangalore" -H "token: $OWNER_TOKEN" | json "d.get('errorCode','none')")" PERSON_NOT_FOUND
add_member_row "$FLEET_A"
check "OFF: the owner's own city is accepted (proxied)" "$(status "$LEGACY_LIST&cities=Bangalore" -H "token: $OWNER_TOKEN")" 200
check "OFF: a city they have no access to is refused (proxied)" "$(status "$LEGACY_LIST&cities=Chennai" -H "token: $OWNER_TOKEN")" 403
check "OFF: a mix with one such city is refused (proxied)" "$(status "$LEGACY_LIST&cities=Bangalore,Chennai" -H "token: $OWNER_TOKEN")" 403
check "OFF: a city they have no access to is refused (direct)" "$(status "$LIST&cities=Chennai" -H "token: $OWNER_TOKEN")" 403
check "OFF: an admin with access to the city may name it" "$(status "$PD_API/driver/fleet/vehicleAssociation?Limit=10&Offset=0&fleetOwnerId=$FLEET_A&cities=Bangalore" -H "token: $ADMIN_TOKEN")" 200

echo "== 12. an old client, in OFF"
set_mode OFF
check "a list with none of the new parameters still works (proxied)" "$(rows_of "$LEGACY_LIST" "$OWNER_TOKEN")" "A:Bangalore"
check "the fleet owner list answers with its old fields" "$(curl -s -m 30 "$DRIVER/driver/fleet/access/list?fleetMemberId=$FLEET_A" -H "token: $OWNER_TOKEN" | json "'ownersList' in d")" True
check "the old select toggle works with just the old parameters" "$(status -X POST "$DRIVER/driver/fleet/access/$FLEET_A/select?enable=true" -H "token: $OWNER_TOKEN")" 200
LEGACY_PHONE=$(mobile)
curl -s -m 20 -X POST "$LEGACY/fleet/v2/login/otp?dashboardPersonId=&enabled=false" -H "token: $LEGACY_TOKEN" -H 'Content-Type: application/json' \
  -d "{\"mobileNumber\":\"$LEGACY_PHONE\",\"mobileCountryCode\":\"+91\"}" >/dev/null
sleep 2
VERIFY_BODY=$(curl -s -m 20 -X POST "$LEGACY/fleet/v2/verify/otp" -H "token: $LEGACY_TOKEN" -H 'Content-Type: application/json' \
  -d "{\"mobileNumber\":\"$LEGACY_PHONE\",\"mobileCountryCode\":\"+91\",\"otp\":\"$OTP\"}")
check "the app server's verify answer still says Success" "$(echo "$VERIFY_BODY" | json "d['result']")" Success
check "and carries the person field the new dashboard reads" "$(echo "$VERIFY_BODY" | json "'personId' in d")" True
OP_ROLE=$(psql -d atlas_dev -tAc "select id from atlas_dashboard.role where name='OPERATOR'")
OP_PHONE=$(mobile); OP_EMAIL="review-op-$OP_PHONE@dashboard.com"
status -X POST "$DRIVER/operator/dashboard/register" -H "token: $ADMIN_TOKEN" -H 'Content-Type: application/json' \
  -d "{\"firstName\":\"Review\",\"lastName\":\"Operator\",\"roleId\":\"$OP_ROLE\",\"email\":\"$OP_EMAIL\",\"mobileNumber\":\"$OP_PHONE\",\"mobileCountryCode\":\"+91\",\"password\":\"Review@12345\",\"fleetAssignment\":null}" >/dev/null
sleep 12
OP_TOKEN=$(curl -s -m 30 -X POST "$DASH/user/login" -H 'Content-Type: application/json' -d "{\"email\":\"$OP_EMAIL\",\"password\":\"Review@12345\"}" | json "d['authToken']")
check "an operator sends the consent OTP the old way, by the owner's phone" "$(status -X POST "$DRIVER/operator/fleet/link/sendOtp" -H "token: $OP_TOKEN" -H 'Content-Type: application/json' -d "{\"mobileNumber\":\"$OWNER_PHONE\",\"mobileCountryCode\":\"+91\"}")" 200
check "the OTP is kept under that phone" "$(redis get "${REDIS_PREFIX}Fleet:Link:PhoneNo:+91$OWNER_PHONE")" '"'$OTP'"'
drop_member_row "$FLEET_A"

echo "== 13. the member a request acts for is the caller, never one it names (direct tree)"
for MODE in ENFORCE OFF; do
  set_mode $MODE
  OTHER_LIST=$(curl -s -m 30 "$DRIVER/driver/fleet/access/list?fleetMemberId=$FLEET_A" -H "token: $OTHER_TOKEN" | json "','.join(sorted(o['fleetOwnerId'] for o in d['ownersList']))")
  case ",$OTHER_LIST," in *",$FLEET_A,"*) bad "$MODE: the access list shows another member's fleets ($OTHER_LIST)";; *) ok "$MODE: the access list ignores the member id the request names";; esac
  check "$MODE: the access list answers without one" "$(status "$DRIVER/driver/fleet/access/list" -H "token: $OTHER_TOKEN")" 200
  SPOOF_MEMBER="mbMemberPersonId=$FLEET_A"
  check "$MODE: trip transactions refuse another owner's fleet even with a made-up member" "$(status -X POST "$DRIVER/driver/fleet/trip/transactions/v2?fleetOwnerId=$FLEET_A&$SPOOF_MEMBER&limit=5&offset=0" -H "token: $OTHER_TOKEN" -H 'Content-Type: application/json' -d '{}')" "$([ "$MODE" = OFF ] && echo 404 || echo 403)"
  check "$MODE: trip transactions of one driver refuse it too" "$(status "$DRIVER/driver/fleet/trip/transactions/00000000-0000-4000-8000-000000000000?fleetOwnerId=$FLEET_A&mbMemberPersonId=$FLEET_A&limit=5&offset=0" -H "token: $OTHER_TOKEN")" "$([ "$MODE" = OFF ] && echo 404 || echo 403)"
  check "$MODE: so do the waypoints of a trip" "$(status "$DRIVER/driver/dashboard/fleet/trip/00000000-0000-4000-8000-000000000000/$FLEET_A/waypoints?memberPersonId=$FLEET_A" -H "token: $OTHER_TOKEN")" 403
  [ "$MODE" = OFF ] && add_member_row "$OTHER_ID" # the old lookup of the owner's own fleet needs it
  check "$MODE: the owner's own trip transactions answer" "$(status -X POST "$DRIVER/driver/fleet/trip/transactions/v2?fleetOwnerId=$OTHER_ID&limit=5&offset=0" -H "token: $OTHER_TOKEN" -H 'Content-Type: application/json' -d '{}')" 200
  if [ "$MODE" = OFF ]; then
    # Reported, not asserted: as on main, the old lookup asks only whether the named fleet has a member association,
    # not whether the caller may act for it.
    add_member_row "$FLEET_A"
    echo "  note OFF, same as main: another owner's trip list, once that fleet has a member association, answers $(status -X POST "$DRIVER/driver/fleet/trip/transactions/v2?fleetOwnerId=$FLEET_A&limit=5&offset=0" -H "token: $OTHER_TOKEN" -H 'Content-Type: application/json' -d '{}')"
    drop_member_row "$FLEET_A"; drop_member_row "$OTHER_ID"
  fi
done
set_mode OFF

echo "== 14. what a request reaches by an id of its own: another fleet's owner, certificate, driver (ENFORCE)"
set_mode ENFORCE
RC_A=$(psql -d atlas_dev -tAc "select id from atlas_driver_offer_bpp.vehicle_registration_certificate where fleet_owner_id='$FLEET_A' limit 1")
refused() { # refused <what> <status> : the other owner is refused, so it is 403
  check "$1" "$2" 403
}
refused "another owner's fleet information is refused" "$(status "$DRIVER/driver/$FLEET_A/fleetOwnerInfo" -H "token: $OTHER_TOKEN")"
check "an owner's own fleet information is not" "$([ "$(status "$DRIVER/driver/$OTHER_ID/fleetOwnerInfo" -H "token: $OTHER_TOKEN")" != 403 ] && echo yes || echo no)" yes
LISTED=$(curl -s -m 30 "$DRIVER/driver/fleetOwner/list?limit=50&offset=0" -H "token: $OTHER_TOKEN" | json "','.join(sorted(o['fleetOwnerId'] for o in d.get('ownersList', [])))")
case ",$LISTED," in *",$FLEET_A,"*|*",$FLEET_B,"*) bad "the fleet owner list shows another owner's fleets ($LISTED)";; *) ok "the fleet owner list shows nothing of another owner's";; esac
refused "another owner's vehicle information is refused" "$(status "$DRIVER/driver/vehicle/info?rcId=$RC_A" -H "token: $OTHER_TOKEN")"
refused "so is its onboarding status" "$(status "$DRIVER/onboarding/register/vehicleStatus?rcId=$RC_A" -H "token: $OTHER_TOKEN")"
refused "and its documents" "$(status "$DRIVER/onboarding/vehicle/documents?rcId=$RC_A" -H "token: $OTHER_TOKEN")"
refused "analytics for another owner's fleet are refused" "$(status -X POST "$DRIVER/driver/fleet/dashboard/analytics/cache" -H "token: $OTHER_TOKEN" -H 'Content-Type: application/json' -d "{\"fleetOwnerId\":\"$FLEET_A\",\"activeDriverCount\":1,\"activeVehicleCount\":1,\"currentOnlineDriver\":1}")"
refused "moving a driver that is not in the owner's fleets is refused" "$(status -X POST "$DRIVER/driver/fleet/driver/00000000-0000-4000-8000-000000000000/changeFleetOwner" -H "token: $OTHER_TOKEN" -H 'Content-Type: application/json' -d "{\"newFleetOwnerId\":\"$OTHER_ID\"}")"
refused "so is moving one into another owner's fleet" "$(status -X POST "$DRIVER/driver/fleet/driver/00000000-0000-4000-8000-000000000000/changeFleetOwner" -H "token: $OTHER_TOKEN" -H 'Content-Type: application/json' -d "{\"newFleetOwnerId\":\"$FLEET_A\"}")"
refused "the badges of another owner's fleet are refused" "$(status "$DRIVER/driver/fleet/getAllBadge?mbFleetOwnerId=$FLEET_A&mbMemberPersonId=$FLEET_A" -H "token: $OTHER_TOKEN")"
check "an owner's own badges are not" "$([ "$(status "$DRIVER/driver/fleet/getAllBadge" -H "token: $OTHER_TOKEN")" != 403 ] && echo yes || echo no)" yes
# The payout-account route names a certificate by its number. The test certificates are copies whose number hash
# is made up, so a lookup by number cannot find them; it uses the same check as the lookups by id above.
set_mode OFF

echo "== 15. a driver's rides, TDS rates and the internal helpers (ENFORCE)"
set_mode ENFORCE
DRV=$(python3 -c 'import uuid; print(uuid.uuid4())')
psql -d atlas_dev -tA >/dev/null <<SQL
INSERT INTO atlas_driver_offer_bpp.person SELECT r.* FROM (SELECT * FROM atlas_driver_offer_bpp.person WHERE role='DRIVER' LIMIT 1) p
  CROSS JOIN LATERAL jsonb_populate_record(null::atlas_driver_offer_bpp.person,
    to_jsonb(p) || jsonb_build_object('id','$DRV','mobile_number_hash','\\x' || md5(random()::text),'created_at', to_char(now(), 'YYYY-MM-DD"T"HH24:MI:SS"Z"'))) r;
INSERT INTO atlas_driver_offer_bpp.fleet_driver_association (id, driver_id, fleet_owner_id, is_active, associated_on, associated_till)
VALUES (gen_random_uuid()::text, '$DRV', '$OTHER_ID', true, now(), now() + interval '1 year');
SQL
error_of() { curl -s -m 30 "$1" -H "token: $2" | json "d.get('errorCode', 'none')"; }
RIDES="$DRIVER/driver/fleet/listRides/$DRV?limit=5&offset=0"
check "an owner lists the rides of a driver in their fleet (not refused)" "$([ "$(error_of "$RIDES" "$OTHER_TOKEN")" != ACCESS_DENIED ] && echo yes || echo no)" yes
check "another owner is refused those rides" "$(error_of "$RIDES" "$OWNER_TOKEN")" ACCESS_DENIED
check "and the rides of a driver in no fleet" "$(status "$DRIVER/driver/fleet/listRides/00000000-0000-4000-8000-000000000000?limit=5&offset=0" -H "token: $OTHER_TOKEN")" 403
# The driver-registration routes check the driver against the caller's fleets in ENFORCE, whatever the merchant's
# fleet-member-hierarchy flag says (the flag decided whether the old association check ran at all).
post_error() { curl -s -m 30 -X POST "$1" -H "token: $2" -H 'Content-Type: application/json' -d "$3" | json "d.get('errorCode', 'none')"; }
REMINDER="$DRIVER/driver/$DRV/trigger/reminder"
REMINDER_BODY='{"documentType":"DriverLicense"}'
check "an owner triggers a reminder for a driver in their fleet (not refused)" "$([ "$(post_error "$REMINDER" "$OTHER_TOKEN" "$REMINDER_BODY")" != ACCESS_DENIED ] && echo yes || echo no)" yes
check "another owner is refused a reminder for that driver" "$(post_error "$REMINDER" "$OWNER_TOKEN" "$REMINDER_BODY")" ACCESS_DENIED
check "and one for a driver in no fleet" "$(post_error "$DRIVER/driver/00000000-0000-4000-8000-000000000000/trigger/reminder" "$OTHER_TOKEN" "$REMINDER_BODY")" ACCESS_DENIED
# The document type in the path is a JSON value, so it is sent in quotes (%22).
check "another owner may not unlink that driver's document" "$(post_error "$DRIVER/driver/$DRV/unlink/document/%22DriverLicense%22" "$OWNER_TOKEN" "{}")" ACCESS_DENIED
check "nor one for a driver in no fleet" "$(post_error "$DRIVER/driver/00000000-0000-4000-8000-000000000000/unlink/document/%22DriverLicense%22" "$OTHER_TOKEN" "{}")" ACCESS_DENIED
check "a fleet owner cannot change a TDS rate" "$(status -X POST "$DRIVER/driver/tdsRate/update" -H "token: $OTHER_TOKEN" -H 'Content-Type: application/json' -d "{\"ownerType\":\"FLEET_OWNER\",\"ownerId\":\"$FLEET_A\",\"tdsRate\":5.0}")" 403
check "the internal helper is not served to anyone (no token)" "$(status "$DRIVER/driver/dashboard/internal/helper/getFleetOwnerId?memberPersonId=$FLEET_A")" 403
check "nor with a token" "$(status "$DRIVER/driver/dashboard/internal/helper/getFleetOwnerIds?memberPersonId=$FLEET_A" -H "token: $OTHER_TOKEN")" 403
check "the same helper still answers on the token-guarded tree" "$(status "$LEGACY/driver/dashboard/internal/helper/getFleetOwnerId?memberPersonId=$FLEET_A" -H "token: $LEGACY_TOKEN")" 200
set_mode OFF

echo "== 16. an operator reaches only the fleets linked to them; a rental owner's TDS rate"
OP_PHONE2=$(mobile); OP_EMAIL2="review-op2-$OP_PHONE2@dashboard.com"
status -X POST "$DRIVER/operator/dashboard/register" -H "token: $ADMIN_TOKEN" -H 'Content-Type: application/json' \
  -d "{\"firstName\":\"Review\",\"lastName\":\"Operator\",\"roleId\":\"$OP_ROLE\",\"email\":\"$OP_EMAIL2\",\"mobileNumber\":\"$OP_PHONE2\",\"mobileCountryCode\":\"+91\",\"password\":\"Review@12345\",\"fleetAssignment\":null}" >/dev/null
sleep 12
OP2_TOKEN=$(curl -s -m 30 -X POST "$DASH/user/login" -H 'Content-Type: application/json' -d "{\"email\":\"$OP_EMAIL2\",\"password\":\"Review@12345\"}" | json "d['authToken']")
OP2_ID=$(person_of "$OP2_TOKEN")
psql -d atlas_dev -tA >/dev/null <<SQL
INSERT INTO atlas_driver_offer_bpp.fleet_operator_association (id, fleet_owner_id, operator_id, is_active, associated_on, associated_till)
VALUES (gen_random_uuid()::text, '$FLEET_A', '$OP2_ID', true, now(), now() + interval '1 year');
SQL
for MODE in ENFORCE OFF; do
  set_mode $MODE
  [ "$MODE" = OFF ] && add_member_row "$FLEET_A"
  check "$MODE: an operator lists the vehicles of a fleet linked to them" "$(status "$DRIVER/driver/fleet/vehicleAssociation?Limit=5&Offset=0&fleetOwnerId=$FLEET_A" -H "token: $OP2_TOKEN")" 200
  check "$MODE: but not those of a fleet that is not linked to them" "$([ "$(status "$DRIVER/driver/fleet/vehicleAssociation?Limit=5&Offset=0&fleetOwnerId=$OTHER_ID" -H "token: $OP2_TOKEN")" = 200 ] && echo allowed || echo refused)" refused
  check "$MODE: nor the drivers of one" "$([ "$(status "$DRIVER/driver/fleet/driverAssociation?Limit=5&Offset=0&fleetOwnerId=$OTHER_ID" -H "token: $OP2_TOKEN")" = 200 ] && echo allowed || echo refused)" refused
  MEMBER_BODY() { echo "{\"fleetMemberId\":\"$OP2_ID\",\"fleetOwnerId\":\"$1\",\"enabled\":true,\"isFleetOwner\":false}"; }
  check "$MODE: an operator links a member to a fleet linked to them" "$(status -X POST "$DRIVER/operator/fleet/member/association/create" -H "token: $OP2_TOKEN" -H 'Content-Type: application/json' -d "$(MEMBER_BODY "$FLEET_A")")" 200
  check "$MODE: but not to a fleet that is not linked to them" "$(status -X POST "$DRIVER/operator/fleet/member/association/create" -H "token: $OP2_TOKEN" -H 'Content-Type: application/json' -d "$(MEMBER_BODY "$OTHER_ID")")" 403
done
drop_member_row "$FLEET_A"
# A rental fleet owner (role and assignment both rental) is held to their own fleet when a TDS rate is changed.
RENTAL_ROLE=$(psql -d atlas_dev -tAc "select id from atlas_dashboard.role where name='RENTAL_FLEET_OWNER'")
psql -d atlas_dev -tA >/dev/null <<SQL
UPDATE atlas_dashboard.person SET role_id='$RENTAL_ROLE', dashboard_access_type='RENTAL_FLEET_OWNER' WHERE id='$OTHER_ID';
UPDATE atlas_dashboard.person_role_assignment SET role_id='$RENTAL_ROLE' WHERE person_id='$OTHER_ID' AND scope_id='$OTHER_ID';
SQL
redis_flush_all() { curl -s -m 10 -X POST "$MOCK/mock/redis/del" -H 'Content-Type: application/json' -d '{"pattern":"*","target":"cluster"}' >/dev/null; }
redis_flush_all
set_mode ENFORCE
TDS() { status -X POST "$DRIVER/driver/tdsRate/update" -H "token: $OTHER_TOKEN" -H 'Content-Type: application/json' -d "{\"ownerType\":\"FLEET_OWNER\",\"ownerId\":\"$1\",\"tdsRate\":5.0}"; }
check "a rental owner cannot change another owner's TDS rate" "$(TDS "$FLEET_A")" 403
check "and their own reaches the rate validation, not a refusal" "$([ "$(TDS "$OTHER_ID")" != 403 ] && echo yes || echo no)" yes
set_mode OFF

echo "== 17. cancellation-penalty waiver: the ride has to belong to a fleet the owner holds (ENFORCE)"
# The route is gated by a capability the fleet role does not hold, so it is granted for this section, which is
# what makes the ride check reachable. A ride needs its booking, fare parameters and location mappings to load.
MID=$(psql -d atlas_dev -tAc "select id from atlas_driver_offer_bpp.merchant where short_id='$MERCHANT'")
W_ROLE=$FLEET_ROLE
psql -d atlas_dev -tAc "insert into atlas_dashboard.role_capability (role_id, capability_id) values ('$W_ROLE','finance.adjustment.write') on conflict do nothing" >/dev/null
W_A_TOKEN=$(login); W_B_TOKEN=$(login)
W_A_ID=$(person_of "$W_A_TOKEN"); W_B_ID=$(person_of "$W_B_TOKEN")
mkride() { # mkride <fleet owner id, or empty for a ride of no fleet>: prints the ride id
  local rid bid lid fo="NULL"
  rid=$(python3 -c 'import uuid;print(uuid.uuid4())'); bid=$(python3 -c 'import uuid;print(uuid.uuid4())'); lid=$(python3 -c 'import uuid;print(uuid.uuid4())')
  [ -n "$1" ] && fo="'$1'"
  psql -d atlas_dev -tA >/dev/null <<SQL
INSERT INTO atlas_driver_offer_bpp.location (id, lat, lon, merchant_id, merchant_operating_city_id) VALUES ('$lid', 12.9, 77.6, '$MID', '$BLR');
INSERT INTO atlas_driver_offer_bpp.location_mapping (id, tag, location_id, entity_id, "order", version, merchant_id, merchant_operating_city_id)
  VALUES (gen_random_uuid()::text, 'RIDE', '$lid', '$rid', 0, 'LATEST', '$MID', '$BLR'), (gen_random_uuid()::text, 'BOOKING', '$lid', '$bid', 0, 'LATEST', '$MID', '$BLR');
INSERT INTO atlas_driver_offer_bpp.fare_parameters (id, base_fare, fare_parameters_type) VALUES ('$bid', 100, 'Progressive');
INSERT INTO atlas_driver_offer_bpp.fare_parameters_progressive_details (fare_parameters_id, dead_km_fare) VALUES ('$bid', 0);
INSERT INTO atlas_driver_offer_bpp.booking (id, bap_id, bap_uri, estimated_fare, fare_parameters_id, primary_exophone, provider_id, quote_id, start_time, status, transaction_id, vehicle_variant, merchant_operating_city_id)
  VALUES ('$bid','b','http://b',100,'$bid','0','$MID','$bid', now(),'COMPLETED','$bid','SEDAN','$BLR');
INSERT INTO atlas_driver_offer_bpp.ride (id, booking_id, driver_id, otp, short_id, status, tracking_url, fleet_owner_id, merchant_id, merchant_operating_city_id)
  VALUES ('$rid', '$bid', '$W_A_ID', '1234', substr('$rid',1,30), 'COMPLETED', 'x', $fo, '$MID', '$BLR');
SQL
  echo "$rid"
}
R_NONE=$(mkride ""); R_A=$(mkride "$W_A_ID"); R_B=$(mkride "$W_B_ID")
set_mode ENFORCE
waive() { curl -s -m 30 -X POST "$DRIVER/ride/$1/waiverRideCancellationPenalty" -H "token: $2" -H 'Content-Type: application/json' -d '{"reason":"review"}' | json "d.get('errorCode','none')"; }
check "an owner waiving a ride of their own fleet gets past the ownership check (the ride is not cancelled)" "$(waive "$R_A" "$W_A_TOKEN")" INVALID_REQUEST
check "another owner is refused that ride" "$(waive "$R_A" "$W_B_TOKEN")" ACCESS_DENIED
check "an owner is refused a ride of another fleet" "$(waive "$R_B" "$W_A_TOKEN")" ACCESS_DENIED
check "and a ride that belongs to no fleet" "$(waive "$R_NONE" "$W_A_TOKEN")" ACCESS_DENIED
set_mode OFF
psql -d atlas_dev -tA >/dev/null <<SQL
CREATE TEMP TABLE b_ids AS SELECT booking_id AS id FROM atlas_driver_offer_bpp.ride WHERE id IN ('$R_NONE','$R_A','$R_B');
DELETE FROM atlas_driver_offer_bpp.location_mapping WHERE entity_id IN ('$R_NONE','$R_A','$R_B') OR entity_id IN (SELECT id FROM b_ids);
DELETE FROM atlas_driver_offer_bpp.ride WHERE id IN ('$R_NONE','$R_A','$R_B');
DELETE FROM atlas_driver_offer_bpp.booking WHERE id IN (SELECT id FROM b_ids);
DELETE FROM atlas_driver_offer_bpp.fare_parameters_progressive_details WHERE fare_parameters_id IN (SELECT id FROM b_ids);
DELETE FROM atlas_driver_offer_bpp.fare_parameters WHERE id IN (SELECT id FROM b_ids);
DELETE FROM atlas_dashboard.role_capability WHERE role_id='$W_ROLE' AND capability_id='finance.adjustment.write';
SQL

echo "== 18. payout account by vehicle number: the vehicle has to belong to a fleet the owner holds (ENFORCE)"
# The test certificates are copies whose number hash is made up, so one is given the hash of its real number
# (the salt is the dev one) and put in the first owner's fleet; it is put back afterwards.
SALT=$(python3 -c "import re,sys; print(re.search(r'encHashSalt\s*=\s*\"([^\"]+)\"', open('$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)/dhall-configs/dev/secrets/dynamic-offer-driver-app.dhall').read()).group(1))")
SEED_RC=$(psql -d atlas_dev -tAc "select id from atlas_driver_offer_bpp.vehicle_registration_certificate where fleet_owner_id is null order by id limit 1")
ORIG_HASH=$(psql -d atlas_dev -tAc "select encode(certificate_number_hash,'hex') from atlas_driver_offer_bpp.vehicle_registration_certificate where id='$SEED_RC'")
psql -d atlas_dev -tAc "update atlas_driver_offer_bpp.vehicle_registration_certificate set fleet_owner_id='$W_A_ID' where id='$SEED_RC'" >/dev/null
set_mode ENFORCE
RC_NUMBER=$(curl -s -m 30 "$DRIVER/driver/fleet/vehicleAssociation?Limit=5&Offset=0" -H "token: $W_A_TOKEN" | json "d['listItem'][0].get('registrationNo') or d['listItem'][0].get('vehicleNo')")
NUM_HASH=$(python3 -c "import hashlib; print(hashlib.sha256(('$SALT' + '$RC_NUMBER').encode()).hexdigest())")
psql -d atlas_dev -tAc "update atlas_driver_offer_bpp.vehicle_registration_certificate set certificate_number_hash=decode('$NUM_HASH','hex') where id='$SEED_RC'" >/dev/null
set_mode ENFORCE
payout() { curl -s -m 30 -X POST "$DRIVER/driver/addRidePayoutAccountNumber" -H "token: $1" -H 'Content-Type: application/json' \
  -d "{\"driverMobileNumber\":\"9000000001\",\"driverMobileNumberCountryCode\":\"+91\",\"vehicleRegistrationNumber\":\"$RC_NUMBER\"}" | json "d.get('errorCode', d.get('result', 'none'))"; }
check "an owner adds a payout account for a vehicle of their fleet" "$(payout "$W_A_TOKEN")" Success
check "another owner is refused that vehicle's number" "$(payout "$W_B_TOKEN")" ACCESS_DENIED
set_mode OFF
wait_sql "select count(*) from atlas_driver_offer_bpp.driver_ride_payout_bank_account where rc_id='$SEED_RC'" 1 30 >/dev/null # the app writes through Redis; the row lands a moment later
PAYOUT_DRIVER=$(psql -d atlas_dev -tAc "select driver_id from atlas_driver_offer_bpp.driver_ride_payout_bank_account where rc_id='$SEED_RC'")
psql -d atlas_dev -tA >/dev/null <<SQL
DELETE FROM atlas_driver_offer_bpp.driver_ride_payout_bank_account WHERE rc_id='$SEED_RC';
UPDATE atlas_driver_offer_bpp.vehicle_registration_certificate SET fleet_owner_id=NULL, certificate_number_hash=decode('$ORIG_HASH','hex') WHERE id='$SEED_RC';
DELETE FROM atlas_driver_offer_bpp.driver_information WHERE driver_id='$PAYOUT_DRIVER' AND driver_id IN (SELECT id FROM atlas_driver_offer_bpp.person WHERE created_at > '$SINCE');
DELETE FROM atlas_driver_offer_bpp.driver_stats WHERE driver_id='$PAYOUT_DRIVER' AND driver_id IN (SELECT id FROM atlas_driver_offer_bpp.person WHERE created_at > '$SINCE');
SQL

echo "== 19. notifications for an operator with no driver-app person, and a driver in the second of two held fleets"
M_OWNER_TOKEN=$(login); M_A=$(person_of "$M_OWNER_TOKEN")
M_B=$(curl -s -m 20 -X POST "$DRIVER/fleet/v2/create" -H "token: $M_OWNER_TOKEN" -H 'Content-Type: application/json' \
  -d "{\"fleetName\":\"Review fleet M\",\"fleetType\":\"NORMAL_FLEET\",\"contactMobileCountryCode\":\"+91\",\"contactMobileNumber\":\"$(mobile)\",\"contactEmail\":\"review-m@example.com\"}" | json "d['fleetId']")
M_OTHER_TOKEN=$(login)
sleep 12
M_DRV=$(python3 -c 'import uuid; print(uuid.uuid4())')
psql -d atlas_dev -tA >/dev/null <<SQL
INSERT INTO atlas_driver_offer_bpp.person SELECT r.* FROM (SELECT * FROM atlas_driver_offer_bpp.person WHERE role='DRIVER' ORDER BY created_at LIMIT 1) p
  CROSS JOIN LATERAL jsonb_populate_record(null::atlas_driver_offer_bpp.person,
    to_jsonb(p) || jsonb_build_object('id','$M_DRV','mobile_number_hash','\\x' || md5(random()::text),'created_at', to_char(now(), 'YYYY-MM-DD"T"HH24:MI:SS"Z"'))) r;
INSERT INTO atlas_driver_offer_bpp.fleet_driver_association (id, driver_id, fleet_owner_id, is_active, associated_on, associated_till)
VALUES (gen_random_uuid()::text, '$M_DRV', '$M_B', true, now(), now() + interval '1 year');
SQL
set_mode ENFORCE
status -X POST "$SEL?enable=true&fleetOwnerId=$M_A" -H "token: $M_OWNER_TOKEN" >/dev/null
status -X POST "$SEL?enable=true&fleetOwnerId=$M_B" -H "token: $M_OWNER_TOKEN" >/dev/null
DETAILS="$DRIVER/driver/fleet/driver/details?driverId=$M_DRV"
check "an owner of two fleets reads a driver of the second (not refused as outside the fleet)" "$([ "$(error_of "$DETAILS" "$M_OWNER_TOKEN")" != DRIVER_NOT_ACTIVE_IN_FLEET ] && echo yes || echo no)" yes
check "another owner is refused that driver" "$(error_of "$DETAILS" "$M_OTHER_TOKEN")" DRIVER_NOT_ACTIVE_IN_FLEET
NOTE="$DRIVER/notification/notification/list"
for MODE in ENFORCE OFF; do
  set_mode $MODE
  check "$MODE: an admin, who has no driver-app person, lists notifications" "$(status "$NOTE" -H "token: $ADMIN_TOKEN")" 200
done
set_mode OFF
psql -d atlas_dev -tA >/dev/null <<SQL
DELETE FROM atlas_driver_offer_bpp.fleet_driver_association WHERE driver_id='$M_DRV';
DELETE FROM atlas_driver_offer_bpp.person WHERE id='$M_DRV';
SQL

echo
echo "passed: $PASS, failed: $FAIL"
[ "$FAIL" = 0 ]
