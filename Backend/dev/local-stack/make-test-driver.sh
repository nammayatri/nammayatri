#!/usr/bin/env bash
# Create a driver who can sign in, go online, and has NO subscription.
#
#     ./make-test-driver.sh 0555009999 "Boss Test" 246810
#
# ── Why one script and not four steps ──────────────────────────────────────
# A usable driver needs four things and three of them are invisible until they
# are missing:
#
#   1. a personal code in the auth guard   -- without it, POST /ui/auth is
#      refused before the backend ever sees him
#   2. a person row                        -- created by signing in, not by SQL,
#      because POST /ui/auth is what calls createDriverWithDetails
#   3. enabled + verified, not blocked     -- or dispatch skips him silently
#   4. a linked vehicle                    -- or he can go online, see a green
#      switch, and never receive anything, because dispatch matches on variant
#
# Miss the fourth and everything looks right from his side. That is the single
# most confusing state in this stack.
#
# ── And deliberately NO subscription row ───────────────────────────────────
# So he lands in `never`: the app says *Aucun abonnement*, the home screen
# carries the banner, and dispatch only reaches him when no paying driver is
# around. That is the state to hand somebody who wants to test paying.
set -euo pipefail

cd "$(dirname "$0")"
NUM="${1:?usage: ./make-test-driver.sh <number> <name> [code]}"
NAME="${2:?usage: ./make-test-driver.sh <number> <name> [code]}"
CODE="${3:-}"
# Through the EDGE, not 8017.
#
# The auth guard -- which holds the personal codes and substitutes the fixed
# 7891 the backend accepts -- sits in nginx's path on 443 only. Port 8017 is the
# proxy in front of the driver app itself, so going there reaches the backend
# with the guard skipped: a six-digit personal code arrives raw and comes back
#
#   "expectation": "(length(otp) == 4 and otp matches regex /^[0-9]*$/)"
#
# which reads as a wrong code and is a wrong URL. (This is also why
# probe-booking-flow.py uses 8017 with the raw 7891 and works.)
#
# --resolve, because this box cannot reach its own public address.
HOST="api.movinapp.net"
GUARD="https://$HOST"
CURL=(curl -s --max-time 25 --resolve "$HOST:443:127.0.0.1")

say() { printf '\n\033[1m== %s\033[0m\n' "$*"; }
ok()  { printf '   \033[1;32mok  \033[0m%s\n' "$*"; }
die() { printf '   \033[1;31mBAD \033[0m%s\n' "$*"; exit 1; }

psql() { docker exec ny-postgres psql -U postgres -d atlas_dev -tAc "$1"; }

say "1. the personal code"
if [ -n "$CODE" ]; then
  ./enrol-driver.sh --set "$NUM" "$CODE" >/dev/null && ok "code set to $CODE"
else
  CODE="$(./enrol-driver.sh "$NUM" "$NAME" | grep -oE '[0-9]{6}' | head -1)"
  [ -n "$CODE" ] || die "enrol-driver.sh did not print a code"
  ok "code generated: $CODE"
fi

say "2. the person row, created by signing in"
MER="$(psql "SELECT id FROM atlas_driver_offer_bpp.merchant LIMIT 1")"
AUTH="$("${CURL[@]}" -X POST "$GUARD/ui/auth" -H 'Content-Type: application/json' \
        -d "{\"mobileNumber\":\"$NUM\",\"mobileCountryCode\":\"+213\",\"merchantId\":\"$MER\"}")"
AID="$(echo "$AUTH" | sed -n 's/.*"authId":"\([^"]*\)".*/\1/p')"
[ -n "$AID" ] || die "auth refused: $AUTH"
VER="$("${CURL[@]}" -X POST "$GUARD/ui/auth/$AID/verify" -H 'Content-Type: application/json' \
       -d "{\"otp\":\"$CODE\",\"deviceToken\":\"make-test-driver\"}")"
echo "$VER" | grep -q '"token"' || die "verify refused: $VER"
DID="$(psql "SELECT id FROM atlas_driver_offer_bpp.person WHERE unencrypted_mobile_number = '$NUM' AND role = 'DRIVER'")"
[ -n "$DID" ] || die "verify succeeded but no person row appeared"
ok "driver id $DID"

say "3. approved by the office"
psql "UPDATE atlas_driver_offer_bpp.person SET first_name = '$NAME' WHERE id = '$DID'" >/dev/null
psql "UPDATE atlas_driver_offer_bpp.driver_information SET enabled = true, verified = true, blocked = false WHERE driver_id = '$DID'" >/dev/null
psql "INSERT INTO atlas_driver_offer_bpp.driver_stats (driver_id) VALUES ('$DID') ON CONFLICT (driver_id) DO NOTHING" >/dev/null
ok "enabled, verified, not blocked"

say "4. a car, or he receives nothing"
# vehicle_class '3WT' is copied from the rows dispatch is known to accept. It
# reads wrong for a sedan and is an upstream artifact; dispatch matches on
# `variant`, so it is left alone deliberately -- see simulate-driver.py.
# The plate decodes to 2022 on the offer screen: [genre][YY] is the middle group.
if [ "$(psql "SELECT count(*) FROM atlas_driver_offer_bpp.vehicle WHERE driver_id = '$DID'")" = "0" ]; then
  psql "INSERT INTO atlas_driver_offer_bpp.vehicle
          (driver_id, capacity, make, model, variant, color, registration_no,
           merchant_id, vehicle_class, created_at, updated_at)
        VALUES ('$DID', 4, 'Renault', 'Symbol', 'SEDAN', 'White', '09090 122 16',
                '$MER', '3WT', now(), now())" >/dev/null
  ok "Renault Symbol · White · 09090 122 16"
else
  ok "already has a vehicle"
fi

say "5. no subscription, on purpose"
psql "DELETE FROM movin.subscription WHERE driver_id = '$DID'" >/dev/null
STATE="$(psql "SELECT state FROM movin.driver_subscription_state WHERE driver_id = '$DID'")"
[ "$STATE" = "never" ] || die "expected state 'never', got '$STATE'"
ok "state is 'never' — the app will say Aucun abonnement"

# The dispatch list is republished on a timer; nudge it so he is restricted now
# rather than within five minutes.
docker compose restart maps-shim >/dev/null 2>&1 || true
sleep 3
RESTRICTED="$(docker exec ny-redis redis-cli GET dynamic-offer-driver-app:movin:restricted)"
case "$RESTRICTED" in
  *"$DID"*) ok "dispatch already knows he is unpaid" ;;
  *)        printf '   \033[1;33m?   \033[0mnot in the restriction list yet: %s\n' "$RESTRICTED" ;;
esac

cat <<CRED

   ─────────────────────────────────────────────
     numéro   $NUM
     code     $CODE
     nom      $NAME
     voiture  Renault Symbol · blanche · 09090 122 16
     abonnement : aucun
   ─────────────────────────────────────────────

   The code is printed once. enrol-driver.sh keeps a salted hash, not the code.

CRED
