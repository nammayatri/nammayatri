#!/usr/bin/env bash
# Put a backup made by ./backup.sh back -- for real, or into a throwaway copy to
# prove that it can be. Phase 7 follow-up, 2026-10-08.
#
#   ./restore.sh rehearse F   restore F into throwaway containers beside the live
#                             stack (own network, no ports), prove every part of
#                             it, and remove them. Nothing live is touched.
#   ./restore.sh live F       restore F INTO THE LIVE STACK. Takes a fresh backup
#                             first, stops what reads the data, replaces it, starts
#                             everything again. Asks you to type a confirmation.
#
# F is a local movin-*.tar.gz.gpg, or `offsite:latest` / `offsite:<name>` to fetch
# it from $RCLONE_REMOTE first -- the copy that will actually be used on the day
# the server is gone.
#
# ── Why this exists ─────────────────────────────────────────────────────────
# `./backup.sh restore` proves a backup into a scratch database and stops there.
# Until 2026-10-08 nothing put one back: no script, never rehearsed (the
# rollback runbook said so). A restore is run on the worst day, by whoever is
# there; it has to be one command that cannot make things worse.
#
# ── What it restores, and in what order ────────────────────────────────────
#   1. the data schemas (atlas_*, movin) -- dropped and reloaded in ONE
#      transaction: if any statement fails, the database is exactly as before;
#   2. passetto, the keys that open the phone numbers -- the same, and its
#      service restarted, because it holds the keys in memory;
#   3. the Arabic place names, by OSM place_id (the index itself is rebuilt by
#      ./geocoder-prepare.sh, and its row ids change when it is);
#   4. the driver documents, added back into their volume (nothing deleted);
#   5. the drivers' sign-in codes (the file the guard reads).
# Then it CHECKS: every table's rows against the dump, a sample of phone
# numbers actually decrypted by passetto, the documents opened, the codes
# parsed. A restore that is not checked is a guess.
#
# ── What it will not do ─────────────────────────────────────────────────────
#   * `live` refuses if anything outside the data schemas depends on them: a
#     DROP ... CASCADE would remove it silently.
#   * `live` refuses to start without a fresh backup of what it is replacing.
#   * `rehearse` never names a live container, network, port or volume.
set -uo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"
HERE="$(pwd)"

PASS_FILE="${PASS_FILE:-/root/.movin-backup-pass}"
RCLONE_REMOTE="${RCLONE_REMOTE:-movin-drive:movin-backups}"
DATA_SCHEMAS="atlas_app atlas_driver_offer_bpp atlas_registry movin"

# The live stack (env-overridable, so the same code can be tested on a copy).
LIVE_PG="${LIVE_PG:-ny-postgres}"
LIVE_DB="${LIVE_DB:-atlas_dev}"
LIVE_PASSETTO_DB="${LIVE_PASSETTO_DB:-ny-passetto-db}"
LIVE_PASSETTO="${LIVE_PASSETTO:-ny-passetto}"
LIVE_PASSETTO_URL="${LIVE_PASSETTO_URL:-http://127.0.0.1:8021}"
LIVE_REDIS="${LIVE_REDIS:-ny-redis}"
LIVE_DOCS_VOLUME="${LIVE_DOCS_VOLUME-local-stack_movin-driver-docs}"
LIVE_CODES="${LIVE_CODES:-$HERE/auth-guard/driver-codes.json}"
# Everything that reads or writes the data, stopped for the swap. The order is
# the order they are started again in: the backends first, then what calls them.
LIVE_APPS="${LIVE_APPS-ny-mock-registry ny-beckn-gateway ny-rider ny-driver ny-maps-shim ny-auth-guard movin-admin-api}"
LIVE_UNITS="${LIVE_UNITS-movin-fleet movin-bot}"

# The throwaway copy. Same images as the live stack, so the proof is about the
# data and not about a different Postgres.
R="movin-rehearsal"
PG_IMAGE="${PG_IMAGE:-postgis/postgis:15-3.4}"
PASSETTO_DB_IMAGE="${PASSETTO_DB_IMAGE:-postgres:12.3}"
PASSETTO_IMAGE="${PASSETTO_IMAGE:-juspayin/passetto-hs:0b18530}"

say()  { printf '\n\033[1m== %s\033[0m\n' "$*"; }
ok()   { printf '   \033[1;32mok  \033[0m%s\n' "$*"; }
bad()  { printf '   \033[1;31mBAD \033[0m%s\n' "$*"; }
info() { printf '       %s\n' "$*"; }
die()  { bad "$*"; exit 1; }
FAIL=0
check() { if [ "$1" = "$2" ]; then ok "$3: $2"; else bad "$3: expected $1, got $2"; FAIL=1; fi; }

# ── fetch, decrypt, unpack ──────────────────────────────────────────────────
WORK=$(mktemp -d)
cleanup_work() { rm -rf "$WORK"; }
trap cleanup_work EXIT

open_backup() {
  local src="$1" file
  [ -f "$PASS_FILE" ] || die "no passphrase at $PASS_FILE"
  case "$src" in
    offsite:*)
      local name="${src#offsite:}"
      if [ "$name" = latest ]; then
        name=$(rclone lsf "$RCLONE_REMOTE" --include 'movin-*.tar.gz.gpg' 2>/dev/null | sort | tail -1)
        [ -n "$name" ] || die "no backup found in $RCLONE_REMOTE"
      fi
      say "fetching $name from $RCLONE_REMOTE"
      rclone copyto "$RCLONE_REMOTE/$name" "$WORK/$name" 2>/dev/null || die "could not fetch $name"
      file="$WORK/$name"
      ok "fetched ($(du -h "$file" | cut -f1))" ;;
    *) [ -f "$src" ] || die "no such file: $src"; file="$src" ;;
  esac
  say "decrypting $(basename "$file")"
  gpg --batch --yes --quiet --decrypt --passphrase-file "$PASS_FILE" \
      --output "$WORK/bundle.tar.gz" "$file" || die "could not decrypt -- wrong passphrase?"
  tar -xzf "$WORK/bundle.tar.gz" -C "$WORK" || die "archive is corrupt"
  [ -f "$WORK/atlas.sql" ] && [ -f "$WORK/passetto.sql" ] || die "archive lacks atlas.sql or passetto.sql"
  ok "decrypted and unpacked"
  sed 's/^/       /' "$WORK/MANIFEST.txt" | sed -n '1,9p'
}

# Rows each table should have: the length of its COPY block in the dump.
expected_rows() {
  awk '/^COPY /{t=$2; n=0; on=1; next} on && $0=="\\."{print t, n; on=0; next} on{n++}' "$1" | sort
}

# ── the restore itself, into whichever containers it is given ───────────────
# $1 postgres container  $2 database  $3 passetto-db container
restore_data() {
  local pg="$1" db="$2" pdb="$3"

  say "the data schemas ($DATA_SCHEMAS) -- one transaction"
  { echo "SET client_min_messages = warning;"
    for s in $DATA_SCHEMAS; do echo "DROP SCHEMA IF EXISTS $s CASCADE;"; done
    cat "$WORK/atlas.sql"
  } | docker exec -i "$pg" psql -U postgres -d "$db" -q -v ON_ERROR_STOP=1 --single-transaction \
      > "$WORK/atlas.log" 2>&1 \
    || { tail -5 "$WORK/atlas.log" | sed 's/^/       /'; die "the data did not load; the transaction was rolled back, nothing changed"; }
  ok "loaded, with no error"

  say "passetto, the keys to the phone numbers -- one transaction"
  { echo 'SET client_min_messages = warning;'
    echo 'DROP SCHEMA IF EXISTS "Passetto" CASCADE;'
    cat "$WORK/passetto.sql"
  } | docker exec -i "$pdb" psql -U passetto -d passetto -q -v ON_ERROR_STOP=1 --single-transaction \
      > "$WORK/passetto.log" 2>&1 \
    || { tail -5 "$WORK/passetto.log" | sed 's/^/       /'; die "passetto did not load; rolled back"; }
  ok "loaded, with no error"
}

# $1 postgres container  $2 database
restore_arabic() {
  local pg="$1" db="$2"
  [ -f "$WORK/geo-arabic.csv" ] || { info "no Arabic names in this backup (taken before 2026-10-08)"; return; }
  say "Arabic place names, by place_id"
  local has
  has=$(docker exec "$pg" psql -U postgres -d "$db" -At -c "SELECT to_regclass('geo.place') IS NOT NULL")
  if [ "$has" != t ]; then
    bad "no geo.place here -- rebuild the index first (./geocoder-prepare.sh), then run this again"
    FAIL=1; return
  fi
  local n
  n=$(docker exec -i "$pg" psql -U postgres -d "$db" -At -v ON_ERROR_STOP=1 -c "
      CREATE TEMP TABLE ar (place_id text, name_ar text);
      COPY ar FROM STDIN WITH (FORMAT csv);
      WITH u AS (UPDATE geo.place p SET name_ar = ar.name_ar FROM ar
                  WHERE p.place_id = ar.place_id AND p.name_ar IS DISTINCT FROM ar.name_ar
                 RETURNING 1)
      SELECT count(*) FROM u;" < "$WORK/geo-arabic.csv" 2>&1 | tail -1)
  local want missing
  want=$(wc -l < "$WORK/geo-arabic.csv" | tr -d ' ')
  missing=$(docker exec -i "$pg" psql -U postgres -d "$db" -At -c "
      CREATE TEMP TABLE ar (place_id text, name_ar text);
      COPY ar FROM STDIN WITH (FORMAT csv);
      SELECT count(*) FROM ar LEFT JOIN geo.place p USING (place_id) WHERE p.place_id IS NULL;" \
      < "$WORK/geo-arabic.csv" 2>&1 | tail -1)
  ok "$want names in the backup; $n written; $missing whose place is not in this index"
}

# $1 docs volume  $2 codes path
restore_files() {
  local vol="$1" codes="$2"
  if [ -f "$WORK/documents.tar.gz" ]; then
    say "driver documents -> $vol (added back; nothing deleted)"
    docker volume inspect "$vol" >/dev/null 2>&1 || docker volume create "$vol" >/dev/null
    docker run --rm -v "$vol":/v -v "$WORK":/in:ro alpine tar -xzf /in/documents.tar.gz -C /v \
      || die "could not unpack the documents"
    ok "unpacked"
  fi
  if [ -f "$WORK/driver-codes.json" ]; then
    say "drivers' sign-in codes -> $codes"
    [ -f "$codes" ] && cp -p "$codes" "$codes.before-restore"
    install -m 600 "$WORK/driver-codes.json" "$codes"
    ok "written$([ -f "$codes.before-restore" ] && echo "; the previous file kept as $(basename "$codes").before-restore")"
  fi
}

# ── the checks ──────────────────────────────────────────────────────────────
# $1 pg  $2 db  $3 how to decrypt one value: a function name  $4 docs volume  $5 codes path
verify() {
  local pg="$1" db="$2" decrypt="$3" vol="$4" codes="$5"

  say "every table against the dump"
  expected_rows "$WORK/atlas.sql" > "$WORK/want"
  local tables=0 wrong=0 t n got
  while read -r t n; do
    got=$(docker exec "$pg" psql -U postgres -d "$db" -At -c "SELECT count(*) FROM $t" 2>/dev/null)
    tables=$((tables + 1))
    [ "$got" = "$n" ] || { bad "$t: dump has $n rows, restored $got"; wrong=$((wrong + 1)); }
  done < "$WORK/want"
  if [ "$wrong" = 0 ]; then ok "$tables tables, every one with exactly the rows in the dump"; else FAIL=1; fi
  for pair in "riders:atlas_app.person" "rides:atlas_app.ride" "drivers:atlas_driver_offer_bpp.person"; do
    local label="${pair%%:*}" tbl="${pair##*:}"
    check "$(grep -E "^  $label " "$WORK/MANIFEST.txt" | awk '{print $2}')" \
          "$(docker exec "$pg" psql -U postgres -d "$db" -At -c "SELECT count(*) FROM $tbl")" "$label (manifest)"
  done

  say "phone numbers, decrypted by passetto (counted, never shown)"
  docker exec "$pg" psql -U postgres -d "$db" -At -c "
      (SELECT mobile_number_encrypted FROM atlas_app.person
        WHERE mobile_number_encrypted IS NOT NULL ORDER BY random() LIMIT 10)
      UNION ALL
      (SELECT mobile_number_encrypted FROM atlas_driver_offer_bpp.person
        WHERE mobile_number_encrypted IS NOT NULL ORDER BY random() LIMIT 10)" > "$WORK/enc"
  local total=0 opened=0 v
  while IFS= read -r v; do
    [ -n "$v" ] || continue
    total=$((total + 1))
    "$decrypt" "$v" | grep -q '"value":"S' && opened=$((opened + 1))
  done < "$WORK/enc"
  rm -f "$WORK/enc"
  if [ "$total" = 0 ]; then info "no encrypted phone numbers in this backup"
  else check "$total" "$opened" "sampled numbers that decrypt"; fi

  if [ -f "$WORK/documents.tar.gz" ]; then
    say "driver documents"
    local want got_n
    want=$(grep -E '^documents ' "$WORK/MANIFEST.txt" | awk '{print $2}')
    got_n=$(docker run --rm -v "$vol":/v:ro alpine sh -c 'find /v -type f | wc -l' | tr -d '[:space:]')
    if [ "$got_n" -ge "${want:-0}" ] 2>/dev/null; then ok "documents: $got_n in the volume (backup had $want)"
    else bad "documents: backup had $want, volume has $got_n"; FAIL=1; fi
    local magic
    magic=$(docker run --rm -v "$vol":/v:ro alpine sh -c 'f=$(find /v -type f | head -1); [ -n "$f" ] && head -c 4 "$f" | od -An -tx1' | tr -d ' \n')
    case "$magic" in
      ffd8ff*|89504e47|25504446|52494646) ok "a document opens as an image or a PDF" ;;
      "") info "no document files" ;;
      *) bad "a document is not an image or a PDF (magic $magic)"; FAIL=1 ;;
    esac
  fi

  if [ -f "$WORK/driver-codes.json" ]; then
    say "drivers' sign-in codes"
    python3 -c "import json,sys; json.load(open(sys.argv[1]))" "$codes" 2>/dev/null \
      && ok "$codes parses ($(grep -c '"salt"' "$codes") enrolled)" \
      || { bad "$codes does not parse"; FAIL=1; }
  fi
}

# ── rehearse ────────────────────────────────────────────────────────────────
rehearse_decrypt() {
  docker run --rm --network "$R" alpine wget -qO- --header 'Content-Type: application/json' \
    --post-data "{\"value\":\"$1\"}" "http://$R-passetto:8012/decrypt" 2>/dev/null
}

# Any HTTP answer at all means passetto is listening (an empty value is a 4xx).
passetto_up() {
  docker run --rm --network "$R" alpine sh -c \
    "wget -S -O /dev/null --header 'Content-Type: application/json' --post-data '{}' http://$R-passetto:8012/decrypt 2>&1 | grep -q 'HTTP/'"
}

teardown() {
  docker rm -f "$R-pg" "$R-passetto-db" "$R-passetto" >/dev/null 2>&1
  docker volume rm "$R-docs" >/dev/null 2>&1
  docker network rm "$R" >/dev/null 2>&1
}

rehearse() {
  local started=$SECONDS
  trap 'teardown; cleanup_work' EXIT
  teardown   # a rehearsal interrupted earlier
  open_backup "${1:?usage: ./restore.sh rehearse <file | offsite:latest>}"

  say "a throwaway copy: network $R, no ports, nothing shared with the live stack"
  docker network create --internal "$R" >/dev/null || die "could not create network $R"
  docker run -d --name "$R-pg" --network "$R" --memory 1g --restart no \
    -e POSTGRES_PASSWORD=rehearsal -e POSTGRES_DB=atlas_dev "$PG_IMAGE" >/dev/null || die "postgres did not start"
  docker run -d --name "$R-passetto-db" --network "$R" --memory 256m --restart no \
    -e POSTGRES_DB=passetto -e POSTGRES_USER=passetto -e POSTGRES_PASSWORD=passetto \
    "$PASSETTO_DB_IMAGE" >/dev/null || die "passetto-db did not start"
  local i
  for i in $(seq 1 90); do
    docker exec "$R-pg" pg_isready -U postgres -d atlas_dev -q 2>/dev/null \
      && docker exec "$R-passetto-db" pg_isready -U passetto -q 2>/dev/null \
      && docker exec "$R-pg" psql -U postgres -d atlas_dev -Atc "SELECT 1 FROM pg_extension WHERE extname='postgis'" 2>/dev/null | grep -q 1 \
      && break
    sleep 2
  done
  ok "postgres and passetto-db ready (empty: no seed, no keys)"
  # The extensions the live database has beside PostGIS, which the dump does
  # not carry (they live in `public`, which is rebuilt, not backed up).
  docker exec "$R-pg" psql -U postgres -d atlas_dev -q -c \
    "CREATE EXTENSION IF NOT EXISTS pg_trgm; CREATE EXTENSION IF NOT EXISTS unaccent; CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";" >/dev/null

  restore_data "$R-pg" atlas_dev "$R-passetto-db"

  # passetto starts only now, on the restored keys: an empty database would
  # make it generate keys of its own, and every number would fail to open.
  docker run -d --name "$R-passetto" --network "$R" --memory 256m --restart no \
    -e PASSETTO_PG_BACKEND_CONN_STRING="postgresql://passetto:passetto@$R-passetto-db:5432/passetto" \
    "$PASSETTO_IMAGE" demo >/dev/null || die "passetto did not start"
  for i in $(seq 1 60); do passetto_up && break; sleep 2; done
  passetto_up || die "passetto did not answer"
  ok "passetto started on the restored keys"

  if [ -f "$WORK/geo-arabic.csv" ]; then
    # The index is rebuilt from OSM, not restored. Stand in for it with every
    # place the names belong to, so the step that matches them is exercised.
    docker exec -i "$R-pg" psql -U postgres -d atlas_dev -q -c "
      CREATE SCHEMA geo; CREATE TABLE geo.place (place_id text PRIMARY KEY, name_ar text);
      CREATE TEMP TABLE ar (place_id text, name_ar text); COPY ar FROM STDIN WITH (FORMAT csv);
      INSERT INTO geo.place (place_id) SELECT place_id FROM ar;" < "$WORK/geo-arabic.csv" >/dev/null
  fi
  restore_arabic "$R-pg" atlas_dev
  restore_files "$R-docs" "$WORK/codes.json"

  verify "$R-pg" atlas_dev rehearse_decrypt "$R-docs" "$WORK/codes.json"

  say "removing the throwaway copy"
  teardown
  ok "removed"
  [ "$FAIL" = 0 ] || die "the rehearsal found a problem -- see BAD above"
  ok "rehearsal passed in $((SECONDS - started)) s: this backup can be put back"
}

# ── live ────────────────────────────────────────────────────────────────────
live_decrypt() {
  curl -s --max-time 10 -H 'Content-Type: application/json' \
    -d "{\"value\":\"$1\"}" "$LIVE_PASSETTO_URL/decrypt"
}

live() {
  local started=$SECONDS
  open_backup "${1:?usage: ./restore.sh live <file | offsite:latest>}"

  say "is anything outside the data schemas built on them?"
  local deps
  deps=$(docker exec "$LIVE_PG" psql -U postgres -d "$LIVE_DB" -At -c "
    SELECT DISTINCT dn.nspname || '.' || dc.relname
      FROM pg_depend d
      JOIN pg_rewrite rw ON rw.oid = d.objid
      JOIN pg_class dc ON dc.oid = rw.ev_class JOIN pg_namespace dn ON dn.oid = dc.relnamespace
      JOIN pg_class sc ON sc.oid = d.refobjid  JOIN pg_namespace sn ON sn.oid = sc.relnamespace
     WHERE sn.nspname = ANY (string_to_array('$DATA_SCHEMAS', ' '))
       AND dn.nspname <> ALL (string_to_array('$DATA_SCHEMAS', ' '))")
  [ -z "$deps" ] || die "these would be dropped with the data: $deps"
  ok "nothing"

  say "THIS REPLACES THE LIVE DATA"
  info "database:  $LIVE_PG / $LIVE_DB, schemas $DATA_SCHEMAS"
  info "keys:      $LIVE_PASSETTO_DB"
  info "stopped for the swap: $LIVE_APPS ${LIVE_UNITS:+(units: $LIVE_UNITS)}"
  info "Everything written since this backup was taken is lost (a backup of it is taken first)."
  local answer=""
  if [ "${RESTORE_CONFIRM:-}" = "restore live" ]; then answer="restore live"
  else printf '       type  restore live  to go on: '; read -r answer < /dev/tty; fi
  [ "$answer" = "restore live" ] || die "not confirmed; nothing changed"

  say "first, a backup of what is about to be replaced"
  if [ "${SKIP_SAFETY_BACKUP:-}" = 1 ]; then info "skipped (SKIP_SAFETY_BACKUP=1)"
  else ./backup.sh || die "the safety backup failed; nothing changed"; fi

  say "stopping what reads the data"
  local c u stopped_units=""
  for c in $LIVE_APPS; do docker stop "$c" >/dev/null 2>&1 && ok "$c stopped" || info "$c: not running"; done
  for u in $LIVE_UNITS; do
    if systemctl is-active --quiet "$u" 2>/dev/null; then systemctl stop "$u" && stopped_units="$stopped_units $u" && ok "$u stopped"; fi
  done
  docker stop "$LIVE_PASSETTO" >/dev/null 2>&1 && ok "$LIVE_PASSETTO stopped"

  start_again() {
    say "starting everything again"
    docker start "$LIVE_PASSETTO" >/dev/null 2>&1 && ok "$LIVE_PASSETTO started"
    for c in $LIVE_APPS; do docker start "$c" >/dev/null 2>&1 && ok "$c started" || info "$c: could not start"; done
    for u in $stopped_units; do systemctl start "$u" && ok "$u started"; done
  }
  if ! (restore_data "$LIVE_PG" "$LIVE_DB" "$LIVE_PASSETTO_DB"); then
    start_again
    die "restore failed: the part named above was rolled back (each part is its own transaction)"
  fi
  restore_arabic "$LIVE_PG" "$LIVE_DB"
  restore_files "$LIVE_DOCS_VOLUME" "$LIVE_CODES"

  if [ -n "$LIVE_REDIS" ]; then
    say "emptying the caches (they describe the data that was replaced)"
    docker exec "$LIVE_REDIS" redis-cli FLUSHALL >/dev/null && ok "$LIVE_REDIS flushed"
  fi
  start_again
  # passetto reads its keys at start; checking before it answers would report
  # every number unreadable. Any HTTP answer means it is up.
  local i
  for i in $(seq 1 60); do
    [ "$(curl -s -o /dev/null -w '%{http_code}' --max-time 5 -H 'Content-Type: application/json' \
         -d '{}' "$LIVE_PASSETTO_URL/decrypt")" != 000 ] && break
    sleep 2
  done
  verify "$LIVE_PG" "$LIVE_DB" live_decrypt "$LIVE_DOCS_VOLUME" "$LIVE_CODES"
  [ "$FAIL" = 0 ] || die "restored, but a check failed -- see BAD above"
  ok "restored and checked in $((SECONDS - started)) s"
  info "The place index is not in a backup: if this is a new server, rebuild it"
  info "(./geocoder-prepare.sh, then geocoder/append-country.sql), then run this again"
  info "for the Arabic names."
}

case "${1:-}" in
  rehearse) rehearse "${2:-}" ;;
  live)     live "${2:-}" ;;
  *) die "usage: ./restore.sh rehearse <file | offsite:latest>  |  ./restore.sh live <file | offsite:latest>" ;;
esac
