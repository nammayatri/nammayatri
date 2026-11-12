#!/usr/bin/env bash
# The key rotation, on the live server: the ~10-minute window. Run as root,
# from /opt/ny/local-stack, by the owner (it reads the secrets folder):
#
#   sudo bash key-rotation/window.sh            the switch
#   sudo bash key-rotation/window.sh rollback   back to today's keys and salt
#   sudo bash key-rotation/window.sh configs    after a switch made before
#                                               2026-10-13's fix: the services'
#                                               encrypted credentials too
#
# Prepared beforehand (2026-10-13, owner's OK): /opt/ny/secrets/hash-salt.env
# and passetto-master.env, and ny-passetto-keys-db holding 1 master key and
# 4 keys. The same steps were proved end to end on the ride regression's stack
# (ci-proof.sh: the same rider and driver before and after) and rehearsed on a
# copy of this server's data (rehearse.sh: 184 values in 4.5 s).
#
# ── The switch ──────────────────────────────────────────────────────────────
#   1. checks, with NOTHING stopped: secrets, key store, today's salt, and
#      rotate.js check (every stored value read, every hash reproduced); the
#      test passenger signs in and who she is is noted;
#   2. a safety dump of atlas_app and atlas_driver_offer_bpp (local);
#   3. a passetto on the new keys, beside today's, on 127.0.0.1:8022;
#   4. everything that reads or writes numbers stopped; rotate.js apply (one
#      transaction; every value read back) and the photographs renamed;
#   5. the switch: compose.keys.yml added to .env's COMPOSE_FILE, the new
#      salt into number-change.env, passetto + rider + driver + maps-shim
#      recreated (--no-deps: nothing else), the people cached in Redis
#      dropped, everything started again;
#   6. proofs: rotate.js check through the LIVE passetto (now the new keys)
#      with the new salt, and the test passenger signs in as the same person.
# If step 4's conversion fails, its transaction is rolled back and the stack
# is started again exactly as it was. If a proof in step 6 fails, it says so
# and gives the rollback command; it does not guess.
#
# ── Rollback ────────────────────────────────────────────────────────────────
# The same conversion the other way (rotate.js apply from the new keys and
# salt to today's), today's passetto started again on its own database (never
# touched), the overlay taken out of COMPOSE_FILE, the old salt put back.
set -euo pipefail
cd "$(dirname "$0")/.."
STACK=$(pwd)
SECRETS=/opt/ny/secrets
ENVF="$STACK/.env"
NCE="$SECRETS/number-change.env"
OVERLAY=key-rotation/compose.keys.yml
IMAGE=juspayin/passetto-hs:0b18530
NODE=ny-maps-shim:local
NET=local-stack_default
DB=postgres://postgres:root@localhost:5434/atlas_dev
APPS=(ny-auth-guard movin-admin-api ny-maps-shim ny-rider ny-driver)
UNITS=(movin-fleet movin-bot)
WORK=$(mktemp -d /root/key-rotation.XXXXXX)
chmod 700 "$WORK"
say() { printf '\n== %s\n' "$*"; }
ok() { printf '   ok  %s\n' "$*"; }
die() { printf '\nSTOPPED: %s\n' "$*" >&2; exit 1; }
[ "$(id -u)" = 0 ] || die "run it with sudo"
cleanup() { docker rm -f ny-passetto-temp >/dev/null 2>&1 || true; }
trap cleanup EXIT

salt_new() { sed -n 's/^MOVIN_HASH_SALT=//p' "$SECRETS/hash-salt.env"; }
salt_old() {
  python3 - "$STACK/2023/Backend/dhall-configs/dev/secrets" <<'PY'
import os, re, sys
d = sys.argv[1]
salts = {re.search(r'encHashSalt\s*=\s*"([^"]*)"', open(os.path.join(d, f)).read()).group(1)
         for f in ('rider-app.dhall', 'dynamic-offer-driver-app.dhall')}
if len(salts) != 1: sys.exit('the two apps hash with different salts')
print(salts.pop(), end='')
PY
}
# rotate.js in the shim's image, on the host network; $1 old url, $2 new url,
# $3 old salt file, $4 new salt file, then rotate.js's own arguments.
rotate() {
  local old=$1 new=$2 os=$3 ns=$4; shift 4
  docker run --rm --network host -v "$STACK/key-rotation":/code:ro -v "$WORK":/work \
    -v local-stack_avatars:/avatars \
    -e DATABASE_URL="$DB" -e OLD_PASSETTO_URL="$old" -e NEW_PASSETTO_URL="$new" \
    -e OLD_SALT_FILE="/work/$os" -e NEW_SALT_FILE="/work/$ns" \
    "$NODE" node /code/rotate.js "$@"
}
temp_passetto() { # $1 database container, $2 mode (server|demo)
  docker rm -f ny-passetto-temp >/dev/null 2>&1 || true
  local pw=()
  [ "$2" = server ] && pw=(-e "MASTER_PASSWORD=$(sed -n 's/^MASTER_PASSWORD=//p' "$SECRETS/passetto-master.env")")
  docker run -d --name ny-passetto-temp --network "$NET" -p 127.0.0.1:8022:8012 \
    -e PASSETTO_PG_BACKEND_CONN_STRING="postgresql://passetto:passetto@$1:5432/passetto" \
    "${pw[@]}" "$IMAGE" "$2" >/dev/null
  for _ in $(seq 1 60); do
    curl -s -o /dev/null --max-time 3 -X POST -H 'Content-Type: application/json' -d '{}' \
      http://127.0.0.1:8022/decrypt && return 0
    sleep 2
  done
  die "the temporary passetto did not answer"
}
# Who the test passenger is: the entity her new token names (rider API, direct).
passenger_is() {
  local authid token
  authid=$(curl -s --max-time 20 -X POST http://localhost:8013/v2/auth -H 'content-type: application/json' \
    -d '{"mobileCountryCode":"+222","mobileNumber":"22778899","merchantId":"YATRI"}' \
    | sed -nE 's/.*"authId":"([^"]+)".*/\1/p')
  [ -n "$authid" ] || return 1
  token=$(curl -s --max-time 20 -X POST "http://localhost:8013/v2/auth/$authid/verify" \
    -H 'content-type: application/json' -d '{"otp":"7891","deviceToken":"key-rotation"}' \
    | sed -nE 's/.*"token":"([^"]+)".*/\1/p')
  [ -n "$token" ] || return 1
  docker exec ny-postgres psql -U postgres -d atlas_dev -At \
    -c "SELECT entity_id FROM atlas_app.registration_token WHERE token = '$token'"
}
wait_up() { # $1 url
  for _ in $(seq 1 90); do
    [ "$(curl -s -o /dev/null -w '%{http_code}' --max-time 5 "$1" || true)" != 000 ] && return 0
    sleep 2
  done
  return 1
}
stop_all() {
  systemctl stop "${UNITS[@]}" 2>/dev/null || true
  docker stop "${APPS[@]}" >/dev/null
  ok "stopped: ${APPS[*]} ${UNITS[*]}"
}
start_all() { # services the switch did not recreate, then the units
  docker start ny-auth-guard movin-admin-api >/dev/null
  systemctl start "${UNITS[@]}" 2>/dev/null || true
}
drop_cached_configs() {
  docker exec ny-redis sh -c "redis-cli --scan --pattern '*CachedQueries:MerchantServiceConfig*' | xargs -r redis-cli del" >/dev/null || true
}
drop_cached_people() {
  docker exec ny-redis sh -c "redis-cli --scan --pattern '*Person*' | grep -v DriverLocation | xargs -r redis-cli del" >/dev/null || true
}
compose() { (cd "$STACK" && docker compose "$@"); }

case "${1:-}" in configs|rollback)
  docker inspect ny-passetto-db >/dev/null 2>&1 \
    || die "retired 2026-10-15: the old key store is gone (cleanup.sh); restore a backup from before the switch instead" ;;
esac
if [ "${1:-}" = configs ]; then
  # The first switch (2026-10-13) converted every number but not the services'
  # credentials, which merchant_service_config keeps encrypted inside JSON: a
  # ride search then failed. Today's passetto still holds the old keys on its
  # own database (never touched): it decrypts them, the live one (the new
  # keys) encrypts them, and the cached copies are dropped.
  say "configs: the services' encrypted credentials, from the old keys to the new"
  grep -q "$OVERLAY" "$ENVF" || die "the switch has not been made: run window.sh, which does this too"
  temp_passetto ny-passetto-db demo
  ok "the old keys' passetto on 127.0.0.1:8022"
  rotate http://localhost:8022 http://localhost:8021 old-salt old-salt configs || die "configs refused (nothing written)"
  drop_cached_configs
  ok "cached configs dropped: the apps read the new ones"
  exit 0
fi

if [ "${1:-}" = rollback ]; then
  say "ROLLBACK: back to today's keys and salt"
  grep -q "$OVERLAY" "$ENVF" || die "the overlay is not in COMPOSE_FILE: nothing to roll back"
  salt_new > "$WORK/new-salt"; salt_old > "$WORK/old-salt"; chmod 600 "$WORK"/*salt
  temp_passetto ny-passetto-db demo
  ok "today's passetto on 127.0.0.1:8022 (its database was never touched)"
  stop_all
  rotate http://localhost:8021 http://localhost:8022 new-salt old-salt apply --avatars /avatars \
    || { start_all; die "the conversion back failed (nothing written if before COMMIT); the stack is on the NEW keys"; }
  rotate http://localhost:8021 http://localhost:8022 new-salt old-salt configs \
    || { start_all; die "the configs' conversion back failed; numbers are on today's keys, configs on the new"; }
  sed -i "s#:$OVERLAY##" "$ENVF"
  sed -i "s#^NUMBER_HASH_SALT=.*#NUMBER_HASH_SALT=$(cat "$WORK/old-salt")#" "$NCE"
  docker rm -f ny-passetto-temp >/dev/null
  compose up -d --no-deps passetto rider-app driver-app maps-shim
  drop_cached_people
  drop_cached_configs
  start_all
  wait_up http://localhost:8013/v2/auth/channels || true
  ok "back on today's keys and salt; the new key database is kept, unused"
  exit 0
fi

say "1. checks -- nothing stopped yet"
[ -s "$SECRETS/hash-salt.env" ] && [ -s "$SECRETS/passetto-master.env" ] || die "a secrets file is missing"
grep -q "$OVERLAY" "$ENVF" && die "the overlay is already in COMPOSE_FILE: the switch was done"
[ "$(grep -c '^NUMBER_HASH_SALT=' "$NCE")" = 1 ] || die "number-change.env has no single NUMBER_HASH_SALT line"
keys=$(docker exec ny-passetto-keys-db psql -U passetto -d passetto -At -c \
  'SELECT (SELECT count(*) FROM "Passetto"."Master")||'"'"'|'"'"'||(SELECT count(*) FROM "Passetto"."Keys")')
[ "$keys" = "1|4" ] || die "the new key store holds $keys, not 1|4"
ok "secrets present, new key store 1|4"
salt_old > "$WORK/old-salt"; salt_new > "$WORK/new-salt"; chmod 600 "$WORK"/*salt
cmp -s "$WORK/old-salt" "$WORK/new-salt" && die "the new salt is the old one"
[ "$(sed -n 's/^NUMBER_HASH_SALT=//p' "$NCE")" = "$(cat "$WORK/old-salt")" ] \
  || die "number-change.env's salt is not the apps' current salt"
ok "today's salt read from the apps' configuration; the shim agrees"
rotate http://localhost:8021 http://localhost:8021 old-salt old-salt check || die "rotate.js check refused"
before=$(passenger_is) || die "the test passenger could not sign in before the switch"
ok "the test passenger (+222 22778899) is $before"

say "2. a safety dump of the two schemas"
docker exec ny-postgres pg_dump -U postgres -d atlas_dev -n atlas_app -n atlas_driver_offer_bpp -Fc \
  > "$WORK/before.dump" || die "the safety dump failed"
ok "$(du -h "$WORK/before.dump" | cut -f1) in $WORK/before.dump"

say "3. a passetto on the new keys, beside today's"
temp_passetto ny-passetto-keys-db server
ok "127.0.0.1:8022"

say "4. the window opens: stop, convert"
start=$(date +%s)
stop_all
if ! rotate http://localhost:8021 http://localhost:8022 old-salt new-salt apply --avatars /avatars; then
  docker start ny-maps-shim ny-rider ny-driver >/dev/null
  start_all
  die "the conversion refused; its transaction was rolled back and the stack is started as it was"
fi
rotate http://localhost:8021 http://localhost:8022 old-salt old-salt configs \
  || die "the services' credentials did not convert; numbers are converted -- run: sudo bash key-rotation/window.sh rollback"

say "5. the switch"
sed -i "s#^COMPOSE_FILE=\(.*\)\$#COMPOSE_FILE=\1:$OVERLAY#" "$ENVF"
grep -q "$OVERLAY" "$ENVF" || die "could not add the overlay to COMPOSE_FILE"
cp -p "$NCE" "$WORK/number-change.env.before"
sed -i "s#^NUMBER_HASH_SALT=.*#NUMBER_HASH_SALT=$(cat "$WORK/new-salt")#" "$NCE"
docker rm -f ny-passetto-temp >/dev/null
compose up -d --no-deps passetto rider-app driver-app maps-shim
drop_cached_people
drop_cached_configs
start_all
wait_up http://localhost:8013/v2/auth/channels || die "rider-app does not answer after the switch -- consider: sudo bash key-rotation/window.sh rollback"
wait_up http://localhost:8016/ || true
ok "the window closed after $(( $(date +%s) - start )) s"

say "6. proofs"
rotate http://localhost:8021 http://localhost:8021 new-salt new-salt check \
  || die "the LIVE passetto with the new salt cannot read everything -- run: sudo bash key-rotation/window.sh rollback"
after=$(passenger_is) || die "the test passenger cannot sign in -- run: sudo bash key-rotation/window.sh rollback"
[ "$after" = "$before" ] || die "the test passenger signed in as a different person ($after) -- run: sudo bash key-rotation/window.sh rollback"
ok "the test passenger is still $after"
say "done: new keys and salt are live. Safety dump kept in $WORK."
