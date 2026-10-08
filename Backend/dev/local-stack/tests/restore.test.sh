#!/usr/bin/env bash
# stack/backup.sh and stack/restore.sh, end to end, on invented data.
#
#     bash tests/restore.test.sh          (needs docker; skipped without it)
#
# A small "live" stack is built from nothing -- PostGIS with the four data
# schemas and the extension schemas the guard expects, passetto with the same
# seed keys the server's has, a documents volume, a codes file, Arabic place
# names -- and phone numbers are ENCRYPTED BY PASSETTO, as the apps do. Then:
#
#   1. the real backup.sh takes a backup of it;
#   2. `restore.sh rehearse` puts it into a throwaway copy and proves it;
#   3. a rehearsal of a broken archive fails, and leaves no container behind;
#   4. `restore.sh live` refuses while something outside the data depends on it;
#   5. a live restore of a broken archive changes nothing (one transaction);
#   6. `restore.sh live` brings back rows, a phone number and an Arabic name
#      that were changed after the backup.
# Phase 7 follow-up, 2026-10-08. No real data anywhere: everything is made here.
set -uo pipefail
HERE="$(cd "$(dirname "$0")/.." && pwd)"
ROOT="$(git -C "$HERE" rev-parse --show-toplevel)"
STACK="$HERE/stack"
if ! docker info >/dev/null 2>&1; then
  # In CI a skip would be a silent green; there, no docker is a failure.
  [ "${GITHUB_ACTIONS:-}" = true ] && { echo "FAIL: no docker on the CI runner"; exit 1; }
  echo "docker is not available here: restore test skipped"
  exit 0
fi

P="rt-src"
T="$(mktemp -d)"
fails=0
pass() { printf '   ok    %s\n' "$*"; }
fail() { printf '   FAIL  %s\n' "$*"; fails=$((fails + 1)); }
cleanup() {
  docker rm -f "$P-pg" "$P-passetto-db" "$P-passetto" >/dev/null 2>&1
  docker rm -f movin-rehearsal-pg movin-rehearsal-passetto-db movin-rehearsal-passetto >/dev/null 2>&1
  docker volume rm "$P-docs" movin-rehearsal-docs >/dev/null 2>&1
  docker network rm "$P" movin-rehearsal >/dev/null 2>&1
  rm -rf "$T"
}
trap cleanup EXIT
cleanup; T="$(mktemp -d)"

sql() { docker exec -i "$P-pg" psql -U postgres -d atlas_dev -At -v ON_ERROR_STOP=1 "$@"; }
pt() { docker run --rm --network "$P" alpine wget -qO- --header 'Content-Type: application/json' \
         --post-data "$2" "http://$P-passetto:8012/$1" 2>/dev/null; }

echo "== a small live stack, invented"
docker network create "$P" >/dev/null
docker run -d --name "$P-pg" --network "$P" -e POSTGRES_PASSWORD=x -e POSTGRES_DB=atlas_dev \
  postgis/postgis:15-3.4 >/dev/null
docker run -d --name "$P-passetto-db" --network "$P" \
  -e POSTGRES_DB=passetto -e POSTGRES_USER=passetto -e POSTGRES_PASSWORD=passetto \
  -v "$ROOT/Backend/dev/sql-seed/passetto-seed.sql:/docker-entrypoint-initdb.d/create_schema.sql:ro" \
  postgres:12.3 >/dev/null
for i in $(seq 1 120); do
  docker exec "$P-pg" psql -U postgres -d atlas_dev -Atc "SELECT 1 FROM pg_extension WHERE extname='postgis_tiger_geocoder'" 2>/dev/null | grep -q 1 \
    && docker exec "$P-passetto-db" psql -U passetto -d passetto -Atc 'SELECT count(*) FROM "Passetto"."Keys"' 2>/dev/null | grep -q 3 && break
  sleep 2
done
# A fixed port, as on the server (8021): a random one changes when restore.sh
# restarts passetto, and the check would talk to nobody.
PPORT=18712
docker run -d --name "$P-passetto" --network "$P" -p 127.0.0.1:$PPORT:8012 \
  -e PASSETTO_PG_BACKEND_CONN_STRING="postgresql://passetto:passetto@$P-passetto-db:5432/passetto" \
  juspayin/passetto-hs:0b18530 demo >/dev/null
PURL="http://127.0.0.1:$PPORT"
for i in $(seq 1 60); do pt encrypt '{"value":"S\"0\""}' | grep -q value && break; sleep 2; done
enc() { pt encrypt "{\"value\":\"S\\\"$1\\\"\"}" | python3 -c 'import json,sys; print(json.load(sys.stdin)["value"])'; }
E1=$(enc 22778899); E2=$(enc 0555000199); E3=$(enc 22100001)
[ -n "$E1" ] && [ -n "$E3" ] && pass "passetto up; numbers encrypted the way the apps do" || { fail "passetto did not encrypt"; exit 1; }

sql -q <<EOF
CREATE EXTENSION IF NOT EXISTS pg_trgm; CREATE EXTENSION IF NOT EXISTS unaccent;
CREATE SCHEMA atlas_app; CREATE SCHEMA atlas_driver_offer_bpp; CREATE SCHEMA atlas_registry;
CREATE SCHEMA movin; CREATE SCHEMA geo;
CREATE TABLE atlas_app.person (id text PRIMARY KEY, mobile_number_encrypted text, name text);
CREATE TABLE atlas_app.booking (id text PRIMARY KEY, rider_id text REFERENCES atlas_app.person(id));
CREATE TABLE atlas_app.ride (id text PRIMARY KEY, booking_id text REFERENCES atlas_app.booking(id));
CREATE TABLE atlas_app.geometry (id serial PRIMARY KEY, region text, geom public.geometry(MultiPolygon, 4326));
CREATE TABLE atlas_driver_offer_bpp.person (id text PRIMARY KEY, mobile_number_encrypted text, merchant_id text);
CREATE TABLE atlas_registry.subscriber (subscriber_id text PRIMARY KEY);
CREATE TABLE movin.wallet (driver_id text PRIMARY KEY, balance numeric);
CREATE VIEW movin.wallet_check AS SELECT w.driver_id, p.merchant_id FROM movin.wallet w JOIN atlas_driver_offer_bpp.person p ON p.id = w.driver_id;
CREATE SEQUENCE movin.invoice_seq;
CREATE TABLE geo.place (id serial PRIMARY KEY, place_id text UNIQUE, display_name text, name_ar text);
INSERT INTO atlas_app.person VALUES ('r1', '$E1', 'Rider, "quoted"'), ('r2', '$E2', E'two\nlines');
INSERT INTO atlas_app.booking VALUES ('b1', 'r1'), ('b2', 'r2');
INSERT INTO atlas_app.ride VALUES ('x1', 'b1');
INSERT INTO atlas_app.geometry (region, geom) VALUES ('Mauritania', ST_Multi(ST_MakeEnvelope(-17, 14, -5, 27, 4326)));
INSERT INTO atlas_driver_offer_bpp.person VALUES ('d1', '$E3', 'favorit0'), ('d2', NULL, 'algeria0');
INSERT INTO atlas_registry.subscriber VALUES ('YATRI');
INSERT INTO movin.wallet VALUES ('d1', 120);
SELECT setval('movin.invoice_seq', 37);
INSERT INTO geo.place (place_id, display_name, name_ar) VALUES
  ('n1', 'Nouakchott', 'نواكشوط'), ('n2', 'Rosso', 'روصو'), ('w3', 'Rue X', NULL);
EOF
docker volume create "$P-docs" >/dev/null
docker run --rm -v "$P-docs":/v alpine sh -c \
  'printf "\x89PNG\r\n\x1a\nxx" > /v/a.png && mkdir -p /v/d1 && printf "%%PDF-1.4" > /v/d1/b.pdf'
printf '{"d1": {"salt": "s", "hash": "h"}}\n' > "$T/codes.json"
openssl rand -base64 24 > "$T/pass"; chmod 600 "$T/pass"
pass "data, extension schemas, documents, codes"

export DB_CONTAINER="$P-pg" PASSETTO_CONTAINER="$P-passetto-db" DOCS_VOLUME="$P-docs" \
       PASS_FILE="$T/pass" BACKUP_DIR="$T/backups" RCLONE_REMOTE="" DRIVER_CODES="$T/codes.json"

echo "== 1. backup.sh takes a backup"
"$STACK/backup.sh" > "$T/b.log" 2>&1
A=$(ls "$T"/backups/movin-*.tar.gz.gpg 2>/dev/null | head -1)
[ -n "$A" ] && pass "archive made" || { fail "no archive"; tail -15 "$T/b.log"; exit 1; }
grep -q "Arabic place names: 2 name" "$T/b.log" && pass "it carries the 2 Arabic names, by place_id" || fail "Arabic names not in the backup"

echo "== 2. rehearse"
"$STACK/restore.sh" rehearse "$A" > "$T/r.log" 2>&1; rc=$?
[ $rc -eq 0 ] && grep -q "rehearsal passed" "$T/r.log" && pass "rehearsal passed" || { fail "rehearsal failed"; tail -25 "$T/r.log"; }
grep -q "7 tables, every one with exactly the rows in the dump" "$T/r.log" && pass "every table's rows match the dump" || fail "table check"
grep -q "sampled numbers that decrypt: 3" "$T/r.log" && pass "3 of 3 phone numbers decrypt with the restored keys" || fail "decrypt check"
grep -q "2 names in the backup; 2 written" "$T/r.log" && pass "Arabic names matched by place_id" || fail "Arabic names"
grep -q "a document opens" "$T/r.log" && pass "documents back and opening" || fail "documents"
[ -z "$(docker ps -aq -f name=movin-rehearsal)" ] && pass "throwaway copy removed" || fail "rehearsal containers left behind"

# A broken archive: the same backup with one bad statement in the data.
mkdir "$T/bad"; gpg --batch --quiet --decrypt --passphrase-file "$T/pass" "$A" | tar -xz -C "$T/bad"
printf 'INSERT INTO atlas_app.ride VALUES (%s);\n' "'broken'" >> "$T/bad/atlas.sql"   # wrong column count
tar -czf "$T/bad.tar.gz" -C "$T/bad" . && gpg --batch --yes --quiet --symmetric --passphrase-file "$T/pass" -o "$T/bad.gpg" "$T/bad.tar.gz"

echo "== 3. a broken archive does not rehearse"
"$STACK/restore.sh" rehearse "$T/bad.gpg" > "$T/r2.log" 2>&1 \
  && fail "a broken archive rehearsed" || pass "refused: $(grep -o 'the data did not load[^;]*' "$T/r2.log" | head -1)"
[ -z "$(docker ps -aq -f name=movin-rehearsal)" ] && pass "and left nothing behind" || fail "containers left"

export LIVE_PG="$P-pg" LIVE_PASSETTO_DB="$P-passetto-db" LIVE_PASSETTO="$P-passetto" \
       LIVE_PASSETTO_URL="$PURL" LIVE_REDIS="" LIVE_DOCS_VOLUME="$P-docs" LIVE_CODES="$T/live-codes.json" \
       LIVE_APPS="" LIVE_UNITS="" RESTORE_CONFIRM="restore live"

# What changes after the backup, and must come back.
sql -q -c "DELETE FROM atlas_app.ride; UPDATE atlas_app.person SET mobile_number_encrypted = 'garbage' WHERE id = 'r1';
           INSERT INTO atlas_app.person VALUES ('r3', NULL, 'after the backup');
           UPDATE geo.place SET name_ar = 'wrong' WHERE place_id = 'n2';"
before=$(sql -c "SELECT count(*) FROM atlas_app.person")

echo "== 4. live refuses while something outside the data depends on it"
sql -q -c "CREATE VIEW geo.riders AS SELECT id FROM atlas_app.person"
"$STACK/restore.sh" live "$A" > "$T/l0.log" 2>&1 && fail "it went ahead" \
  || { grep -q "would be dropped with the data: geo.riders" "$T/l0.log" && pass "refused, naming geo.riders" || { fail "refused for another reason"; tail -5 "$T/l0.log"; }; }
sql -q -c "DROP VIEW geo.riders"

echo "== 5. a live restore of a broken archive changes nothing"
SKIP_SAFETY_BACKUP=1 "$STACK/restore.sh" live "$T/bad.gpg" > "$T/l1.log" 2>&1 && fail "it went ahead" || pass "refused"
[ "$(sql -c "SELECT count(*) FROM atlas_app.person")" = "$before" ] \
  && [ "$(sql -c "SELECT count(*) FROM atlas_app.ride")" = 0 ] && pass "the live data is exactly as it was" || fail "the failed restore changed the data"

echo "== 6. live"
"$STACK/restore.sh" live "$A" > "$T/l2.log" 2>&1; rc=$?
[ $rc -eq 0 ] && grep -q "restored and checked" "$T/l2.log" && pass "restored and checked" || { fail "live restore failed"; tail -25 "$T/l2.log"; }
grep -q "first, a backup of what is about to be replaced" "$T/l2.log" && [ "$(ls "$T"/backups/movin-*.gpg | wc -l)" -ge 2 ] \
  && pass "a safety backup was taken first" || fail "no safety backup"
[ "$(sql -c "SELECT count(*) FROM atlas_app.person")" = 2 ] && [ "$(sql -c "SELECT count(*) FROM atlas_app.ride")" = 1 ] \
  && pass "rows as at the backup (the later rider gone, the deleted ride back)" || fail "rows not restored"
v=$(sql -c "SELECT mobile_number_encrypted FROM atlas_app.person WHERE id = 'r1'")
curl -s -H 'Content-Type: application/json' -d "{\"value\":\"$v\"}" "$PURL/decrypt" | grep -q '22778899' \
  && pass "the damaged phone number decrypts again" || fail "phone number"
[ "$(sql -c "SELECT name_ar FROM geo.place WHERE place_id = 'n2'")" = "روصو" ] && pass "the Arabic name is back" || fail "Arabic name"
[ "$(sql -c "SELECT ST_GeometryType(geom) FROM atlas_app.geometry")" = "ST_MultiPolygon" ] && pass "PostGIS geometry intact" || fail "geometry"
[ "$(sql -c "SELECT last_value FROM movin.invoice_seq")" = 37 ] && pass "sequences intact (invoice_seq 37)" || fail "sequence"
[ "$(sql -c "SELECT count(*) FROM movin.wallet_check")" = 1 ] && pass "views across data schemas intact" || fail "view"
cmp -s "$T/codes.json" "$T/live-codes.json" && pass "the codes file is back" || fail "codes"

echo
[ $fails -eq 0 ] && echo "restore: all checks passed" || { echo "restore: $fails FAILED"; exit 1; }
