#!/usr/bin/env bash
#
# Algerian test accounts that sign in WITHOUT an SMS (2026-09-13), until Algeria
# has an SMS provider. Algeria stays closed to everyone else (OPEN_COUNTRIES).
#
#   passengers  +213 0555 00 00 01 / 02 / 03   the private test code (SMS_BYPASS_CODE)
#   drivers     +213 0666 00 00 01  Voiture    a personal code, generated
#               +213 0666 00 00 02  Herbin     a personal code, generated
#
# -- No code in this file, since 2026-09-27 -----------------------------------
# This repository is PUBLIC. It used to carry the passengers' code (111111)
# and both drivers' personal codes, which let anybody sign in as them. Now the
# passengers' code comes from /opt/ny/secrets/test-accounts.env, and each run
# gives both drivers a fresh random code, printed once to the terminal and
# replacing the one before (enrol-driver.sh always does). Nothing in this file
# is ever a code.
#
# Runs ON the VPS, AFTER the +213 backend is deployed -- it checks that first
# and stops otherwise. Idempotent.
#
# Everything here is a TEST account and must go before Algeria opens: remove
# the numbers from SMS_BYPASS in /opt/ny/secrets/test-accounts.env and revoke the two driver
# codes with enrol-driver.sh --revoke.
#
set -uo pipefail
cd "$(dirname "$0")"
DZ=algeria0-0000-0000-0000-00000algeria
LAT=36.7538; LON=3.0588
BYPASS="+2130555000001,+2130555000002,+2130555000003,+2130666000001,+2130666000002"
# number|first|last|variant|make|model|colour|plate
DRIVERS='
0666000001|Test|Voiture|SEDAN|Renault|Symbol|Blanc|00001 116 16
0666000002|Test|Herbin|HATCHBACK|Toyota|Hilux|Blanc|00002 116 16
'
SECRETS=/opt/ny/secrets/test-accounts.env
say() { printf '\n== %s\n' "$*"; }
ok()  { printf '   ok   %s\n' "$*"; }
bad() { printf '   BAD  %s\n' "$*"; }
pg()  { docker exec ny-postgres psql -U postgres -d atlas_dev -At -c "$1"; }

say "does the backend accept +213 yet?"
code=$(curl -s -o /tmp/dz.json -w '%{http_code}' -X POST http://127.0.0.1:8013/v2/auth \
  -H 'content-type: application/json' \
  -d '{"mobileCountryCode":"+213","mobileNumber":"0555000001","merchantId":"YATRI"}')
if ! grep -q authId /tmp/dz.json; then
  bad "not yet ($code $(head -c 120 /tmp/dz.json)) -- deploy the +213 build first; nothing changed"
  exit 1
fi
ok "yes"

say "1. the bypass list (sign-in without SMS, and past the closed-country gate)"
# The list lives in the secrets file, not in docker-compose.yml (which is in
# this public repository). Added once; the file is created root-only if new.
sudo -n python3 - "$BYPASS" "$SECRETS" <<'PY'
import os, sys
add, path = sys.argv[1].split(","), sys.argv[2]
lines = open(path).read().splitlines() if os.path.exists(path) else []
cur = next((l for l in lines if l.startswith("SMS_BYPASS=")), "SMS_BYPASS=")
have = [n for n in cur.split("=", 1)[1].split(",") if n]
new = have + [n for n in add if n not in have]
if new != have:
    lines = [l for l in lines if not l.startswith("SMS_BYPASS=")] + ["SMS_BYPASS=" + ",".join(new)]
    old = os.umask(0o077)
    open(path, "w").write("\n".join(lines) + "\n")
    os.umask(old)
    print("   ok   added")
else:
    print("   ok   already listed")
if not any(l.startswith("SMS_BYPASS_CODE=") for l in lines):
    print("   BAD  no SMS_BYPASS_CODE in " + path + " -- no number is exempt until it is set")
PY
[ $? -eq 0 ] || exit 1
docker compose up -d --no-deps --force-recreate auth-guard 2>&1 | tail -n 2

say "2. driver codes"
echo "$DRIVERS" | while IFS='|' read -r num first last variant make model colour plate; do
  [ -z "$num" ] && continue
  # Enrolling prints a NEW random code and replaces any earlier one: running
  # this script rotates the test drivers' codes. Shown once; stored hashed.
  env COUNTRY_CODE=+213 NSN_LENGTH=9 TRUNK_ZERO=1 MOBILE_FIRST=567 FIXED_SECOND= \
    ./enrol-driver.sh "$num" "$first $last (TEST)" | sed 's/^/   /'
done

say "3. driver accounts: created by signing in, then approved as the agency would"
echo "$DRIVERS" | while IFS='|' read -r num first last variant make model colour plate; do
  [ -z "$num" ] && continue
  did=$(pg "SELECT id FROM atlas_driver_offer_bpp.person WHERE unencrypted_mobile_number='$num' AND merchant_id='$DZ'")
  if [ -z "$did" ]; then
    curl -s -o /dev/null -X POST http://127.0.0.1:8017/ui/auth -H 'content-type: application/json' \
      -d "{\"mobileCountryCode\":\"+213\",\"mobileNumber\":\"$num\",\"merchantId\":\"$DZ\"}"
    did=$(pg "SELECT id FROM atlas_driver_offer_bpp.person WHERE unencrypted_mobile_number='$num' AND merchant_id='$DZ'")
  fi
  [ -n "$did" ] || { bad "$num: no person row"; continue; }
  pg "UPDATE atlas_driver_offer_bpp.person SET first_name='$first', last_name='$last' WHERE id='$did'" >/dev/null
  pg "UPDATE atlas_driver_offer_bpp.driver_information
         SET enabled=true, verified=true, blocked=false, active=false, on_ride=false
       WHERE driver_id='$did'" >/dev/null
  pg "INSERT INTO atlas_driver_offer_bpp.driver_stats (driver_id) VALUES ('$did') ON CONFLICT (driver_id) DO NOTHING" >/dev/null
  if [ "$(pg "SELECT count(*) FROM atlas_driver_offer_bpp.vehicle WHERE driver_id='$did'")" = "0" ]; then
    pg "INSERT INTO atlas_driver_offer_bpp.vehicle
          (driver_id, capacity, make, model, variant, color, registration_no,
           merchant_id, vehicle_class, created_at, updated_at)
        VALUES ('$did', 4, '$make', '$model', '$variant', '$colour', '$plate',
                '$DZ', '3WT', now(), now())" >/dev/null
  fi
  # Test credit, through the ledger like any other movement, so the app lets
  # them go online (it refuses a driver who cannot afford a 100 DA day).
  if [ "$(pg "SELECT count(*) FROM movin.wallet_entry WHERE driver_id='$did' AND note='Crédit de test'")" = "0" ]; then
    pg "INSERT INTO movin.wallet (driver_id) VALUES ('$did') ON CONFLICT (driver_id) DO NOTHING" >/dev/null
    pg "INSERT INTO movin.wallet_entry (driver_id, kind, amount, note) VALUES ('$did', 'adjustment', 1000, 'Crédit de test')" >/dev/null
    pg "UPDATE movin.wallet SET balance = balance + 1000, updated_at = now() WHERE driver_id='$did'" >/dev/null
  fi
  ok "+213 $num  $first $last  $variant  $plate  balance $(pg "SELECT balance FROM movin.wallet WHERE driver_id='$did'") DA"
done

say "4. proof through the PUBLIC API, the way the app signs in"
API=https://api.movinapp.net
sleep 3
a=$(curl -s -X POST "$API/v2/auth" -H 'content-type: application/json' \
  -d '{"mobileCountryCode":"+213","mobileNumber":"0555000001","merchantId":"YATRI"}')
aid=$(echo "$a" | python3 -c 'import sys,json;print(json.load(sys.stdin).get("authId",""))' 2>/dev/null)
if [ -n "$aid" ]; then
  v=$(curl -s -X POST "$API/v2/auth/$aid/verify" -H 'content-type: application/json' \
    -d "{\"otp\":\"${SMS_BYPASS_CODE:?source /opt/ny/secrets/test-accounts.env first}\",\"deviceToken\":\"dz-test-accounts\"}")
  echo "$v" | grep -q '"token"' && ok "passenger +213 0555000001 signs in with the private test code" \
                               || bad "passenger verify: $(echo "$v" | head -c 140)"
else
  bad "passenger auth: $(echo "$a" | head -c 140)"
fi
a=$(curl -s -X POST "$API/ui/auth" -H 'content-type: application/json' \
  -d "{\"mobileCountryCode\":\"+213\",\"mobileNumber\":\"0666000001\",\"merchantId\":\"$DZ\"}")
echo "$a" | grep -q authId && ok "driver +213 0666000001 reaches the code screen" \
                            || bad "driver auth: $(echo "$a" | head -c 140)"
c=$(curl -s -o /dev/null -w '%{http_code}' -X POST "$API/v2/auth" -H 'content-type: application/json' \
  -d '{"mobileCountryCode":"+213","mobileNumber":"0555999999","merchantId":"YATRI"}')
[ "$c" = "403" ] && ok "any other +213 number is still refused (403 COUNTRY_NOT_OPEN)" \
                 || bad "another +213 number answered $c"
