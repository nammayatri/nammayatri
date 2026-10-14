#!/usr/bin/env bash
# The key rotation, end to end, on the throwaway stack the ride regression
# builds (.github/workflows/algeria-ride-regression.yml) -- never on the server.
#
#   bash key-rotation/ci-proof.sh        (from stack/, after setup.sh)
#
# A passenger and a driver sign in; the person each one IS is noted (the
# registration token's entity). Then the whole switch, as the window will do
# it: a new key database and passetto keys, rotate.js apply with the backends
# stopped, compose.keys.yml switched on (passetto on the new keys, the salt
# from MOVIN_HASH_SALT). Then the same two numbers sign in again, and each
# must be the SAME person: a backend that hashed the number differently, or
# could not decrypt it, would make a new account instead -- the failure that
# matters, and the one nothing else here would show.
set -euo pipefail
cd "$(dirname "$0")/.."
PG="docker exec ny-postgres psql -U postgres -d atlas_dev -At"
KEYS="-f docker-compose.yml -f key-rotation/compose.keys.yml"
SECRETS=/opt/ny/secrets
RIDER=(+213 0550123456)
DRIVER=(+213 0551234567)
log() { printf '\n==> %s\n' "$*"; }
die() { printf 'FAILED: %s\n' "$*" >&2; exit 1; }
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"; docker rm -f ny-passetto-new >/dev/null 2>&1 || true' EXIT

# Who the number signs in as: the entity its new token names.
rider_is() {
  local authid token
  authid=$(curl -s --max-time 20 -X POST http://localhost:8014/v2/auth -H 'content-type: application/json' \
    -d "{\"mobileCountryCode\":\"${RIDER[0]}\",\"mobileNumber\":\"${RIDER[1]}\",\"merchantId\":\"YATRI\"}" \
    | sed -nE 's/.*"authId":"([^"]+)".*/\1/p')
  [ -n "$authid" ] || die "rider: sign-in not accepted"
  token=$(curl -s --max-time 20 -X POST "http://localhost:8014/v2/auth/$authid/verify" -H 'content-type: application/json' \
    -d '{"otp":"7891","deviceToken":"key-rotation"}' | sed -nE 's/.*"token":"([^"]+)".*/\1/p')
  [ -n "$token" ] || die "rider: code refused"
  $PG -c "SELECT entity_id FROM atlas_app.registration_token WHERE token = '$token'"
}
driver_is() {
  local mid authid token
  mid=$($PG -c "SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'NAMMA_YATRI_PARTNER'")
  authid=$(curl -s --max-time 20 -X POST http://localhost:8017/ui/auth -H 'content-type: application/json' \
    -d "{\"mobileNumber\":\"${DRIVER[1]}\",\"mobileCountryCode\":\"${DRIVER[0]}\",\"merchantId\":\"$mid\"}" \
    | sed -nE 's/.*"authId":"([^"]+)".*/\1/p')
  [ -n "$authid" ] || die "driver: sign-in not accepted"
  token=$(curl -s --max-time 20 -X POST "http://localhost:8017/ui/auth/$authid/verify" -H 'content-type: application/json' \
    -d '{"otp":"7891","deviceToken":"key-rotation"}' | sed -nE 's/.*"token":"([^"]+)".*/\1/p')
  [ -n "$token" ] || die "driver: code refused"
  $PG -c "SELECT entity_id FROM atlas_driver_offer_bpp.registration_token WHERE token = '$token'"
}
up() { # $1 port $2 path
  for _ in $(seq 1 90); do
    [ "$(curl -s -o /dev/null -w '%{http_code}' --max-time 5 "http://localhost:$1$2" || true)" != 000 ] && return 0
    sleep 2
  done
  die "nothing answers on :$1"
}

log "before: who each number is"
rider_before=$(rider_is); driver_before=$(driver_is)
[ -n "$rider_before" ] && [ -n "$driver_before" ] || die "no person behind a token"
echo "    rider  ${RIDER[*]}: $rider_before"
echo "    driver ${DRIVER[*]}: $driver_before"

log "secrets: a new salt and master password, the current salt (files only)"
sudo mkdir -p "$SECRETS" && sudo chown "$(id -u)" "$SECRETS"
printf 'MOVIN_HASH_SALT=%s\n' "$(openssl rand -hex 32)" > "$SECRETS/hash-salt.env"
printf 'MASTER_PASSWORD=%s\n' "$(openssl rand -hex 24)" > "$SECRETS/passetto-master.env"
sed -n 's/^MOVIN_HASH_SALT=//p' "$SECRETS/hash-salt.env" > "$WORK/new-salt"
python3 - 2023/Backend/dhall-configs/dev/secrets/rider-app.dhall "$WORK/old-salt" <<'PY'
import re, sys
m = re.search(r'encHashSalt\s*=\s*"([^"]*)"', open(sys.argv[1]).read())
if not m: sys.exit('no encHashSalt')
open(sys.argv[2], 'w').write(m.group(1))
PY
master=$(sed -n 's/^MASTER_PASSWORD=//p' "$SECRETS/passetto-master.env")

log "the new key database, its schema (no demo keys) and new keys"
docker compose $KEYS up -d --no-deps passetto-keys-db
for _ in $(seq 1 60); do docker exec ny-passetto-keys-db pg_isready -q -h 127.0.0.1 && break; sleep 1; done
docker run --rm --entrypoint cat juspayin/passetto-hs:0b18530 /opt/app/create_schema.sql | grep -v '^INSERT' \
  | docker exec -i ny-passetto-keys-db psql -U passetto -d passetto -q -v ON_ERROR_STOP=1
net=$(docker inspect ny-passetto-keys-db -f '{{range $k, $v := .NetworkSettings.Networks}}{{$k}}{{end}}')
conn="postgresql://passetto:passetto@ny-passetto-keys-db:5432/passetto"
docker run --rm --network "$net" -e PASSETTO_PG_BACKEND_CONN_STRING="$conn" \
  juspayin/passetto-hs:0b18530 init "$master" 4 >/dev/null
[ "$(docker exec ny-passetto-keys-db psql -U passetto -d passetto -At -c 'SELECT count(*) FROM "Passetto"."Keys"')" = 4 ] \
  || die "the new key database does not hold four keys"
docker run -d --name ny-passetto-new --network "$net" -p 127.0.0.1:8022:8012 \
  -e PASSETTO_PG_BACKEND_CONN_STRING="$conn" -e MASTER_PASSWORD="$master" \
  juspayin/passetto-hs:0b18530 server >/dev/null
up 8022 /

pg_started=$(docker inspect ny-postgres -f '{{.State.StartedAt}}')
log "the window: backends stopped, every value converted"
docker stop ny-rider ny-driver >/dev/null
docker run --rm --network host -v "$PWD/key-rotation":/code:ro -v "$WORK":/work \
  -e DATABASE_URL=postgres://postgres:root@localhost:5434/atlas_dev \
  -e OLD_PASSETTO_URL=http://localhost:8021 -e NEW_PASSETTO_URL=http://localhost:8022 \
  -e OLD_SALT_FILE=/work/old-salt -e NEW_SALT_FILE=/work/new-salt \
  ny-maps-shim:local node /code/rotate.js apply
# The services' credentials, encrypted inside merchant_service_config's JSON:
# missed by the first live switch (2026-10-13), when a ride search failed.
docker run --rm --network host -v "$PWD/key-rotation":/code:ro \
  -e DATABASE_URL=postgres://postgres:root@localhost:5434/atlas_dev \
  -e OLD_PASSETTO_URL=http://localhost:8021 -e NEW_PASSETTO_URL=http://localhost:8022 \
  ny-maps-shim:local node /code/rotate.js configs
docker rm -f ny-passetto-new >/dev/null

log "switched: passetto on the new keys, the backends on the new salt"
# --no-deps: only these three. Without it compose also recreated ny-postgres
# (first proof run, 2026-10-13) -- harmless here, a database restart on the day.
docker compose $KEYS up -d --no-deps passetto rider-app driver-app
recreated=$(docker inspect ny-postgres -f '{{.State.StartedAt}}')
[ "$recreated" = "$pg_started" ] || die "the switch restarted the database"
docker exec ny-redis sh -c "redis-cli --scan --pattern '*Person*' | grep -v DriverLocation | xargs -r redis-cli del" >/dev/null || true
docker exec ny-redis sh -c "redis-cli --scan --pattern '*CachedQueries:MerchantServiceConfig*' | xargs -r redis-cli del" >/dev/null || true
up 8014 /v2/auth/channels
up 8017 /openapi

log "after: the same numbers must be the same people"
rider_after=$(rider_is); driver_after=$(driver_is)
echo "    rider  ${RIDER[*]}: $rider_after"
echo "    driver ${DRIVER[*]}: $driver_after"
[ "$rider_after" = "$rider_before" ] || die "the rider signed in as a different person"
[ "$driver_after" = "$driver_before" ] || die "the driver signed in as a different person"
# And a priced ride in each country: a search reads the services'
# credentials, which sign-in never does -- the check the first live switch
# lacked.
bash setup.sh drivers >/dev/null
bash setup.sh price-both
n=$($PG -c "SELECT count(*) FROM atlas_app.person WHERE mobile_number_hash IS NOT NULL")
echo "    $n passenger(s) with a number, every one now hashed with the new salt"

log "key rotation proved: same people, new keys, new salt"
