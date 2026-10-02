#!/usr/bin/env bash
#
# Steps 6 and 7 of the two-country plan (2026-09-13), after the files are in
# place: restart the auth guard and the maps shim, then prove both still serve
# Mauritania and that Algeria is refused as "not open yet".
#
# Runs ON the VPS, from /opt/ny/local-stack. Reads nothing secret and prints
# nothing secret.
#
set -uo pipefail
cd "$(dirname "$0")"
say() { printf '\n== %s\n' "$*"; }
ok()  { printf '   ok   %s\n' "$*"; }
bad() { printf '   BAD  %s\n' "$*"; FAILED=1; }
FAILED=0
API=https://api.movinapp.net

say "restart the guard and the shim"
docker restart ny-auth-guard ny-maps-shim >/dev/null
for i in $(seq 1 30); do
  curl -sf http://127.0.0.1:8030/healthz >/dev/null && break
  sleep 2
done
curl -sf http://127.0.0.1:8030/healthz >/dev/null && ok "shim healthy" || bad "shim not healthy"

say "sign-in: +213 refused as not open, +222 still reaches the backend"
code=$(curl -s -o /tmp/g213.json -w '%{http_code}' -X POST "$API/v2/auth" \
  -H 'content-type: application/json' \
  -d '{"mobileCountryCode":"+213","mobileNumber":"0550123456","merchantId":"YATRI"}')
if [ "$code" = "403" ] && grep -q COUNTRY_NOT_OPEN /tmp/g213.json; then
  ok "+213 -> 403 COUNTRY_NOT_OPEN"
else
  bad "+213 -> $code $(head -c 160 /tmp/g213.json)"
fi
# The SMS_BYPASS test rider: no SMS is sent and no credit spent.
code=$(curl -s -o /tmp/g222.json -w '%{http_code}' -X POST "$API/v2/auth" \
  -H 'content-type: application/json' \
  -d '{"mobileCountryCode":"+222","mobileNumber":"22778899","merchantId":"YATRI"}')
if [ "$code" = "200" ] && grep -q authId /tmp/g222.json; then
  ok "+222 test rider -> 200 with an authId"
else
  bad "+222 -> $code $(head -c 160 /tmp/g222.json)"
fi

say "wallet: both countries' rows loaded"
docker exec ny-maps-shim node -e \
  "const w=require('/app/wallet.js');for(const [k,v] of Object.entries(w.COUNTRIES))console.log('   ',k,v.price,v.currency,v.gateway)" \
  || bad "wallet.js did not load"

say "dispatch restriction recomputed with per-country prices"
sleep 3
docker logs --since 2m ny-maps-shim 2>&1 | grep -E '\[restricted\]|\[wallet\]' | tail -n 5 > /tmp/shim-restricted.log
cat /tmp/shim-restricted.log
# A failed query keeps the last list, which is safe for dispatch and silent to
# everyone -- so here it is a FAILURE, not a line to read past. The first
# deploy printed it and still reported success.
if grep -q 'query failed' /tmp/shim-restricted.log; then
  bad "the restriction query failed"
elif grep -q 'restricted (startup)' /tmp/shim-restricted.log; then
  ok "restriction list republished"
else
  bad "no restriction list published since the restart"
fi

say "result"
[ "$FAILED" = "0" ] && ok "steps 6 and 7 are live" || bad "something above failed"
exit "$FAILED"
