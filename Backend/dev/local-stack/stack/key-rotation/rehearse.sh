#!/usr/bin/env bash
# The key rotation, rehearsed on a throwaway copy. Run ON the server, from a
# folder holding this script and rotate.js:
#
#   bash rehearse.sh
#
# Copies the tables that hold encrypted numbers, the passetto database and the
# passengers' photographs into containers on their own internal network (no
# ports, nothing shared with the live stack), starts a second passetto on NEW
# keys, runs `rotate.js check` and `rotate.js apply` there, measures them, and
# removes everything it made -- containers, network, files -- however it ends.
#
# The live stack is only READ: pg_dump of seven tables and of passetto's
# database, a copy of the photographs, the current salt into a file nobody
# else can read. The salts and the new master password are never printed.
set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LIVE=/opt/ny/local-stack
R=movin-keys-rehearsal
PG_IMAGE=postgis/postgis:15-3.4
PASSETTO_DB_IMAGE=postgres:12.3
PASSETTO_IMAGE=juspayin/passetto-hs:0b18530
NODE_IMAGE=ny-maps-shim:local
TABLES=(atlas_app.person atlas_driver_offer_bpp.person atlas_driver_offer_bpp.ride_details
  atlas_driver_offer_bpp.rider_details atlas_driver_offer_bpp.driver_license
  atlas_driver_offer_bpp.idfy_verification atlas_driver_offer_bpp.vehicle_registration_certificate)

say() { printf '\n== %s\n' "$*"; }
die() { printf 'STOPPED: %s\n' "$*" >&2; exit 1; }

WORK=$(mktemp -d /tmp/movin-keys.XXXXXX)
chmod 700 "$WORK"
teardown() {
  docker rm -f "$R-pg" "$R-old-db" "$R-old" "$R-new-db" "$R-new" >/dev/null 2>&1
  docker network rm "$R" >/dev/null 2>&1
  rm -rf "$WORK"
  echo "== removed: containers, network, $WORK"
}
trap teardown EXIT

say "a throwaway copy: network $R, internal, no ports"
docker network create --internal "$R" >/dev/null || die "could not create network $R"
docker run -d --name "$R-pg" --network "$R" --memory 512m --restart no \
  -e POSTGRES_PASSWORD=rehearsal "$PG_IMAGE" >/dev/null || die "postgres did not start"
for db in old-db new-db; do
  docker run -d --name "$R-$db" --network "$R" --memory 256m --restart no \
    -e POSTGRES_DB=passetto -e POSTGRES_USER=passetto -e POSTGRES_PASSWORD=passetto \
    "$PASSETTO_DB_IMAGE" >/dev/null || die "$db did not start"
done
# Over TCP: during its first start the image runs a temporary server on the
# socket only, then restarts -- a socket check can catch the temporary one.
for c in "$R-pg" "$R-old-db" "$R-new-db"; do
  for _ in $(seq 1 60); do docker exec "$c" pg_isready -q -h 127.0.0.1 2>/dev/null && break; sleep 1; done
  docker exec "$c" pg_isready -q -h 127.0.0.1 || { docker logs --tail 15 "$c" 2>&1 | sed 's/^/   | /'; die "$c not ready"; }
done
sleep 3

say "the seven tables that hold numbers, copied from the live database"
docker exec "$R-pg" psql -U postgres -qc 'CREATE SCHEMA atlas_app; CREATE SCHEMA atlas_driver_offer_bpp;' \
  || die "schemas"
args=(); for t in "${TABLES[@]}"; do args+=(-t "$t"); done
# Tables and rows only (no post-data section): their foreign keys name tables
# the rehearsal does not need, and rotate.js finds rows by id without an index.
docker exec ny-postgres pg_dump -U postgres -d atlas_dev --no-owner --no-privileges \
  --section=pre-data --section=data "${args[@]}" \
  | docker exec -i "$R-pg" psql -U postgres -q -v ON_ERROR_STOP=1 >/dev/null || die "copying the tables"
for t in "${TABLES[@]}"; do
  live=$(docker exec ny-postgres psql -U postgres -d atlas_dev -Atc "SELECT count(*) FROM $t")
  copy=$(docker exec "$R-pg" psql -U postgres -Atc "SELECT count(*) FROM $t")
  [ "$live" = "$copy" ] || die "$t: $live rows live, $copy copied"
  echo "   $t: $copy rows"
done

say "passetto on today's keys (a copy of its database)"
docker exec ny-passetto-db pg_dump -U passetto -d passetto --no-owner --no-privileges \
  | docker exec -i "$R-old-db" psql -U passetto -d passetto -q -v ON_ERROR_STOP=1 >/dev/null \
  || die "copying passetto's database"
docker run -d --name "$R-old" --network "$R" --memory 256m --restart no \
  -e PASSETTO_PG_BACKEND_CONN_STRING="postgresql://passetto:passetto@$R-old-db:5432/passetto" \
  "$PASSETTO_IMAGE" demo >/dev/null || die "passetto (today's keys) did not start"

say "passetto on NEW keys: an empty schema, init, then server"
# The image's create_schema.sql also INSERTs upstream's demo master key and
# three demo keys: only its CREATE statements are kept, or `init` meets a
# master key already there ("Invalid password") -- and a server started on it
# would be the demo keys again.
docker run --rm --entrypoint cat "$PASSETTO_IMAGE" /opt/app/create_schema.sql | grep -v '^INSERT' \
  | docker exec -i "$R-new-db" psql -U passetto -d passetto -q -v ON_ERROR_STOP=1 >/dev/null \
  || die "passetto's empty schema"
openssl rand -hex 24 > "$WORK/master"
openssl rand -hex 32 > "$WORK/new-salt"
chmod 600 "$WORK/master" "$WORK/new-salt"
docker run --rm --network "$R" \
  -e PASSETTO_PG_BACKEND_CONN_STRING="postgresql://passetto:passetto@$R-new-db:5432/passetto" \
  "$PASSETTO_IMAGE" init "$(cat "$WORK/master")" 4 > "$WORK/init.log" 2>&1 \
  || { grep -v "$(cat "$WORK/master")" "$WORK/init.log" | tail -15 | sed 's/^/   | /'; die "passetto init"; }
keys=$(docker exec "$R-new-db" psql -U passetto -d passetto -Atc \
  "SELECT count(*) FROM information_schema.tables WHERE table_schema NOT IN ('pg_catalog','information_schema')")
nkeys=$(docker exec "$R-new-db" psql -U passetto -d passetto -Atc 'SELECT count(*) FROM "Passetto"."Keys"')
nmaster=$(docker exec "$R-new-db" psql -U passetto -d passetto -Atc 'SELECT count(*) FROM "Passetto"."Master"')
echo "   new key store: $keys table(s), $nmaster master key, $nkeys keys"
[ "$nmaster" = 1 ] && [ "$nkeys" = 4 ] || die "the new key store is not one master key and four keys"
docker run -d --name "$R-new" --network "$R" --memory 256m --restart no \
  -e PASSETTO_PG_BACKEND_CONN_STRING="postgresql://passetto:passetto@$R-new-db:5432/passetto" \
  -e MASTER_PASSWORD="$(cat "$WORK/master")" \
  "$PASSETTO_IMAGE" server >/dev/null || die "passetto (new keys) did not start"

up() {
  docker run --rm --network "$R" alpine sh -c \
    "wget -S -O /dev/null --header 'Content-Type: application/json' --post-data '{}' http://$1:8012/decrypt 2>&1 | grep -q 'HTTP/'"
}
for p in "$R-old" "$R-new"; do
  for _ in $(seq 1 60); do up "$p" && break; sleep 2; done
  up "$p" || die "$p does not answer"
done
echo "   both answer"

say "today's salt, from the rider app's configuration (into a file, never printed)"
python3 - "$LIVE/2023/Backend/dhall-configs/dev/secrets" "$WORK/old-salt" <<'PY' || die "reading today's salt"
import re, sys, os
d, out = sys.argv[1], sys.argv[2]
salts = []
for f in ('rider-app.dhall', 'dynamic-offer-driver-app.dhall'):
    m = re.search(r'encHashSalt\s*=\s*"([^"]*)"', open(os.path.join(d, f)).read())
    if not m: sys.exit(f'no encHashSalt in {f}')
    salts.append(m.group(1))
if salts[0] != salts[1]: sys.exit('the rider and driver apps hash with DIFFERENT salts')
open(out, 'w').write(salts[0])
os.chmod(out, 0o600)
print('   the rider and driver apps use the same salt')
PY

say "the passengers' photographs, copied"
mkdir -p "$WORK/avatars"
docker run --rm -v local-stack_avatars:/from:ro -v "$WORK/avatars":/to alpine sh -c 'cp -a /from/. /to/' \
  || die "copying the photographs"
echo "   $(ls "$WORK/avatars" | wc -l) file(s)"

cp "$HERE/rotate.js" "$WORK/"
run() {
  docker run --rm --network "$R" -v "$WORK":/work -w /work \
    -e DATABASE_URL="postgres://postgres:rehearsal@$R-pg:5432/postgres" \
    -e OLD_PASSETTO_URL="http://$R-old:8012" -e NEW_PASSETTO_URL="http://$R-new:8012" \
    -e OLD_SALT_FILE=/work/old-salt -e NEW_SALT_FILE=/work/new-salt \
    "$NODE_IMAGE" node /work/rotate.js "$@"
}

say "rotate.js check"
start=$(date +%s%N)
run check || die "check refused"
echo "   $(( ($(date +%s%N) - start) / 1000000 )) ms"

say "rotate.js apply"
start=$(date +%s%N)
run apply --avatars /work/avatars || die "apply refused"
echo "   $(( ($(date +%s%N) - start) / 1000000 )) ms"

say "after: today's keys can no longer open a converted number"
sample=$(docker exec "$R-pg" psql -U postgres -Atc \
  "SELECT mobile_number_encrypted FROM atlas_app.person WHERE mobile_number_encrypted IS NOT NULL LIMIT 1")
code=$(docker run --rm --network "$R" alpine sh -c \
  "wget -S -O /dev/null --header 'Content-Type: application/json' --post-data '{\"value\":\"$sample\"}' http://$R-old:8012/decrypt 2>&1 | grep -o 'HTTP/[0-9.]* [0-9]*' | tail -1")
echo "   today's passetto on a converted value: $code"
case "$code" in *" 200") die "today's keys still open a converted number" ;; esac

say "rehearsal passed"
