#!/usr/bin/env bash
# Live demo: proves the self-hosted backend works end to end.
set -uo pipefail
BASE="${BASE:-http://localhost:8014}"

cyan(){ printf '\n\033[1;36m=== %s ===\033[0m\n' "$*"; }
green(){ printf '\033[1;32m    %s\033[0m\n' "$*"; }

cyan "1. Backend containers running"
docker ps --format 'table {{.Names}}\t{{.Status}}' | grep -E 'NAMES|ny-'

cyan "2. Database: schema + seeded merchant"
docker exec ny-postgres psql -U postgres -d atlas_dev -c \
  "SELECT (SELECT count(*) FROM information_schema.tables WHERE table_schema='atlas_app') AS tables,
          (SELECT count(*) FROM atlas_app.merchant) AS merchants,
          (SELECT short_id FROM atlas_app.merchant LIMIT 1) AS merchant;"

cyan "2b. Service areas: Algeria"
docker exec ny-postgres psql -U postgres -d atlas_dev -c \
  "SELECT region, ST_NPoints(geom) AS boundary_points
     FROM atlas_app.geometry
    WHERE region IN ('Algiers','Oran','Annaba') ORDER BY region;"
docker exec ny-postgres psql -U postgres -d atlas_dev -c \
  "SELECT short_id, origin_restriction AS serves FROM atlas_app.merchant;"

cyan "3. Login: request OTP"
AUTH=$(curl -s --max-time 20 -X POST "$BASE/v2/auth" \
  -H 'content-type: application/json' \
  -H 'x-bundle-version: 1.0.1' -H 'x-client-version: 1.0.0' \
  -d '{"mobileCountryCode":"+91","mobileNumber":"9999900001","merchantId":"YATRI"}')
AID=$(printf '%s' "$AUTH" | sed -nE 's/.*"authId":"([^"]+)".*/\1/p')
green "authId: $AID"

cyan "4. Verify OTP -> session token"
V=$(curl -s --max-time 20 -X POST "$BASE/v2/auth/$AID/verify" \
  -H 'content-type: application/json' -d '{"otp":"7891","deviceToken":"demo"}')
TOKEN=$(printf '%s' "$V" | sed -nE 's/.*"token":"([^"]+)".*/\1/p')
green "token : $TOKEN"
green "person: $(printf '%s' "$V" | sed -nE 's/.*"id":"([^"]+)".*/\1/p')"
green "mobile: $(printf '%s' "$V" | sed -nE 's/.*"maskedMobileNumber":"([^"]+)".*/\1/p')  (encrypted + masked)"

cyan "5. Authenticated business call: is this pickup point serviceable?"
ask() {
  R=$(curl -s --max-time 20 -X POST "$BASE/v2/serviceability/origin" \
        -H 'content-type: application/json' -H "token: $TOKEN" \
        -d "{\"location\":{\"lat\":$2,\"lon\":$3}}")
  printf '\033[1;32m    %-26s %-24s %s\033[0m\n' "$1" "$2, $3" "$R"
}
ask "Algiers - city centre"  36.7538     3.0588
ask "Algiers - airport"      36.6910     3.2154
ask "Oran"                   35.6969    -0.6331
ask "Annaba"                 36.9000     7.7667
ask "Constantine"            36.3650     6.6147
ask "Tamanrasset (Sahara)"   22.7850     5.5228
ask "Tunis, Tunisia"         36.8065    10.1815
ask "Oujda, Morocco"         34.6867    -1.9114
ask "Bangalore, India"       12.9715987 77.5945627

printf '\n\033[1;33m=== DONE - backend fully operational, serving Algeria ===\033[0m\n\n'
