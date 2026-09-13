#!/usr/bin/env bash
#
# Step 5 of the two-country plan (2026-09-13): swap in the backend images that
# accept both +222 and +213, then stop for the ride-flow proof.
#
#   bash deploy-backend.sh            # verify the image, tag a rollback, swap
#   bash deploy-backend.sh rollback   # put the previous image back
#
# ── Three things this refuses to skip ──────────────────────────────────────
# 1. The patch must be IN both rebuilt binaries before anything is swapped.
#    `grep -a -c` inside the image, never `strings` (not installed here, and it
#    once produced a confident wrong finding), with "+222" as the control.
# 2. A rollback tag is made BEFORE the swap, so going back is one command.
# 3. The compose file hardcodes `image: ny-rider:patched` on all five services
#    -- the NY_IMAGE variable its header describes is referenced nowhere. So the
#    swap is a re-tag of that name, not a variable.
#
set -uo pipefail
cd "$(dirname "$0")"
IMG=ghcr.io/nammayatri-algeria/ny-backend:latest
SERVICES="rider-app driver-app beckn-gateway mock-registry mock-google"
say() { printf '\n== %s  (%s)\n' "$*" "$(date -u +%T)"; }
ok()  { printf '   ok   %s\n' "$*"; }
bad() { printf '   BAD  %s\n' "$*"; }

wait_up() {
  for port in 8013 8016; do
    for i in $(seq 1 60); do
      code=$(curl -s -o /dev/null -w '%{http_code}' "http://127.0.0.1:$port/" || true)
      [ "$code" != "000" ] && break
      sleep 2
    done
    [ "$code" != "000" ] && ok ":$port answering" || bad ":$port not answering after 120 s"
  done
}

if [ "${1:-}" = "rollback" ]; then
  last=$(docker images --format '{{.Tag}}' ny-rider | grep '^rollback-' | sort | tail -n 1)
  [ -n "$last" ] || { bad "no rollback tag"; exit 1; }
  say "rolling back to ny-rider:$last"
  docker tag "ny-rider:$last" ny-rider:patched
  docker compose up -d --no-build $SERVICES 2>&1 | tail -n 6
  wait_up
  exit 0
fi

say "pull $IMG"
docker pull -q "$IMG" >/dev/null || { bad "pull failed"; exit 1; }
ok "$(docker image inspect "$IMG" --format '{{.Id}}' | cut -c1-19)"

say "is the two-country patch inside both rebuilt binaries?"
for exe in rider-app-exe dynamic-offer-driver-app-exe; do
  n213=$(docker run --rm --entrypoint sh "$IMG" -c "grep -a -c '+213' /opt/app/$exe" 2>/dev/null || echo 0)
  n222=$(docker run --rm --entrypoint sh "$IMG" -c "grep -a -c '+222' /opt/app/$exe" 2>/dev/null || echo 0)
  built=$(docker run --rm --entrypoint sh "$IMG" -c "ls -l --time-style=+%F\ %R /opt/app/$exe" | awk '{print $6, $7}')
  if [ "${n213:-0}" -gt 0 ] && [ "${n222:-0}" -gt 0 ]; then
    ok "$exe  +213 x$n213  +222 x$n222  built $built"
  else
    bad "$exe  +213 x$n213  +222 x$n222 — NOT swapping"
    exit 1
  fi
done

say "rollback tag, then swap"
RB="rollback-$(date -u +%Y%m%d-%H%M)"
docker tag ny-rider:patched "ny-rider:$RB" && ok "previous image kept as ny-rider:$RB"
docker tag "$IMG" ny-rider:patched
docker compose up -d --no-build $SERVICES 2>&1 | tail -n 6
wait_up

say "sign-in reaches the backend in both countries (loopback, behind the guard)"
for body in '{"mobileCountryCode":"+222","mobileNumber":"22778899","merchantId":"YATRI"}' \
            '{"mobileCountryCode":"+213","mobileNumber":"0555000199","merchantId":"YATRI"}'; do
  code=$(curl -s -o /tmp/auth.json -w '%{http_code}' -X POST http://127.0.0.1:8013/v2/auth \
    -H 'content-type: application/json' -d "$body")
  grep -q authId /tmp/auth.json && ok "$code  $(echo "$body" | cut -c1-48)…" \
                               || bad "$code  $(head -c 140 /tmp/auth.json)"
done
say "done — now prove a full ride in each country"
