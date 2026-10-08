#!/usr/bin/env bash
# Back up the parts of this stack that cannot be rebuilt.
#
#   ./backup.sh            take a backup now
#   ./backup.sh restore F  restore backup F into a scratch database and check it
#                          (to put one BACK, or rehearse doing so: ./restore.sh)
#   ./backup.sh list       what we have, and how old the newest one is
#   ./backup.sh install    install the nightly systemd timer
#
# ── Why this is not just `pg_dump atlas_dev` ────────────────────────────────
#
# 1. **The keys live in a different container.** atlas_app stores rider phone
#    numbers encrypted, and the keys that open them are in `ny-passetto-db`, a
#    separate Postgres. A backup of ny-postgres alone restores perfectly and
#    leaves every phone number permanently unreadable. Both are dumped here, and
#    a restore is only meaningful with both.
#
# 2. **Most of the database is rebuildable, and skipping it is the difference
#    between a 7 MB backup and a 183 MB one.** Measured:
#
#        geo      155 MB   the place index -- ./geocoder-prepare.sh regenerates
#                          it from the same .osm.pbf we already hold, in ~5 min
#        public     7 MB   PostGIS spatial_ref_sys, ships with the extension
#        tiger      2 MB   PostGIS reference data, likewise
#        ─────────────────
#        atlas_app              3.9 MB  ← riders, bookings, rides
#        atlas_driver_offer_bpp 3.6 MB  ← drivers, fares, the BPP side
#        atlas_registry          40 kB
#
#    So the backup carries an explicit include list. An include list has one
#    failure mode -- a schema added later is silently left out -- so the script
#    refuses to run if the set of schemas in the database is not the set it was
#    written for. Better a loud stop than a year of backups missing a table.
#
# 3. **It is encrypted before it leaves the box.** The dump contains phone
#    numbers and the keys to decrypt them, in the same archive. Uploading that
#    to anyone's cloud account unencrypted would be worse than having no backup,
#    because it would feel safe.
set -uo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

DB_CONTAINER="${DB_CONTAINER:-ny-postgres}"
DB_NAME="${DB_NAME:-atlas_dev}"
DB_USER="${DB_USER:-postgres}"

PASSETTO_CONTAINER="${PASSETTO_CONTAINER:-ny-passetto-db}"
PASSETTO_DB="${PASSETTO_DB:-passetto}"
PASSETTO_USER="${PASSETTO_USER:-passetto}"

BACKUP_DIR="${BACKUP_DIR:-/var/backups/movin}"

# The volume admin-api writes driver papers to. Set DOCS_VOLUME="" to skip it
# deliberately; leaving it unset and absent is a stop, not a skip, because a
# backup that quietly holds half the picture is the failure this file exists
# against.
DOCS_VOLUME="${DOCS_VOLUME-local-stack_movin-driver-docs}"
PASS_FILE="${PASS_FILE:-/root/.movin-backup-pass}"

# Enough to go back a month, which is the point: a problem noticed on Friday may
# have started the previous weekend, and "last night" would already have
# overwritten the last good copy with the broken one.
KEEP_DAILY="${KEEP_DAILY:-7}"
KEEP_WEEKLY="${KEEP_WEEKLY:-4}"

# Set to an rclone remote (e.g. "movin-drive:movin-backups") to copy off the
# server. Left empty the backup still runs and says plainly that it stayed here
# -- a local-only backup is worth something, and pretending it went offsite is
# worth less than nothing.
RCLONE_REMOTE="${RCLONE_REMOTE:-}"

# The schemas this script was written for. Anything else is a stop, not a guess.
#
# `movin` is ours: the drivers' wallets and top-up receipts, their papers, the
# console's accounts and the account deletion requests. None of it is
# rebuildable and none of it exists anywhere else, so it is data.
#
# It was added on 2026-08-30, four days late, and the delay is the whole reason
# this guard exists. `movin` was created on 26 August; from the 27th the backup
# refused to run and said exactly why, every night, in the journal -- and
# nobody read the journal. So the guard worked and the habit did not. Two
# things follow, and they are worth more than this line:
#
#   * a schema added to this database is not finished until it is named here;
#   * a backup nobody is told about failing is a backup that has stopped.
#     `systemctl status movin-backup.service` after any schema change, or wire
#     the unit's failure to something that speaks.
DATA_SCHEMAS="atlas_app atlas_driver_offer_bpp atlas_registry movin"
# `tiger_data` was found by this guard on its first run, which is the argument
# for having it: it is PostGIS's TIGER geocoder data, nothing to do with us, and
# it would have been just as invisible if it had been a schema that mattered.
REBUILDABLE_SCHEMAS="geo public tiger tiger_data topology"

say()  { printf '\n\033[1m== %s\033[0m\n' "$*"; }
ok()   { printf '   \033[1;32mok  \033[0m%s\n' "$*"; }
bad()  { printf '   \033[1;31mBAD \033[0m%s\n' "$*"; }
info() { printf '       %s\n' "$*"; }
die()  { bad "$*"; exit 1; }

psql_q() { docker exec "$DB_CONTAINER" psql -U "$DB_USER" -d "$DB_NAME" -At -c "$1"; }

# ── the guard that keeps an include list honest ─────────────────────────────
check_schemas() {
  local found expected
  found=$(psql_q "
    SELECT string_agg(nspname, ' ' ORDER BY nspname)
      FROM pg_namespace
     WHERE nspname NOT LIKE 'pg\\_%' AND nspname <> 'information_schema';")
  expected=$(printf '%s\n' $DATA_SCHEMAS $REBUILDABLE_SCHEMAS | sort | tr '\n' ' ')
  expected="${expected% }"

  if [ "$found" != "$expected" ]; then
    bad "the database has schemas this script does not know about"
    info "expected: $expected"
    info "found:    $found"
    info ""
    info "Decide whether the new one holds data or is rebuildable, add it to"
    info "DATA_SCHEMAS or REBUILDABLE_SCHEMAS above, and run again. Until then"
    info "a backup would quietly leave it out."
    exit 1
  fi
}

require_passphrase() {
  [ -f "$PASS_FILE" ] || die "no passphrase at $PASS_FILE — see 'setting it up' at the bottom of this file"
  [ -s "$PASS_FILE" ] || die "$PASS_FILE is empty"
  local perms
  perms=$(stat -c '%a' "$PASS_FILE")
  [ "$perms" = "600" ] || die "$PASS_FILE is mode $perms; must be 600"
}

# ── take a backup ───────────────────────────────────────────────────────────
take() {
  require_passphrase
  say "checking the database is the shape this script expects"
  check_schemas
  ok "schemas as expected"

  mkdir -p "$BACKUP_DIR"
  local stamp work out
  stamp=$(date -u +%Y%m%dT%H%M%SZ)
  work=$(mktemp -d)
  trap 'rm -rf "$work"' RETURN
  out="$BACKUP_DIR/movin-$stamp.tar.gz.gpg"

  say "dumping the data that cannot be rebuilt"
  local args=()
  for s in $DATA_SCHEMAS; do args+=(--schema="$s"); done
  docker exec "$DB_CONTAINER" pg_dump -U "$DB_USER" -d "$DB_NAME" \
      --no-owner --no-privileges "${args[@]}" > "$work/atlas.sql" \
    || die "pg_dump of $DB_NAME failed"
  ok "atlas: $(du -h "$work/atlas.sql" | cut -f1)"

  say "dumping the encryption keys"
  # Without this the restore above is rows of unreadable ciphertext.
  docker exec "$PASSETTO_CONTAINER" pg_dump -U "$PASSETTO_USER" -d "$PASSETTO_DB" \
      --no-owner --no-privileges > "$work/passetto.sql" \
    || die "pg_dump of $PASSETTO_DB failed"
  ok "passetto: $(du -h "$work/passetto.sql" | cut -f1)"

  # The Arabic place names. The index they live in (`geo`) is rebuilt rather
  # than backed up, and a rebuild drops `name_ar` (docs/maps.md): refilling it
  # takes arabic-names.sql plus the hand-reviewed and composed Mauritanian
  # names, and those (arabic-reviewed*.sql) are keyed on geo.place.id, which a
  # rebuild renumbers. So the names travel here -- 58 329 on 2026-10-08 --
  # keyed by the OSM place_id, which a rebuild keeps. Found while writing the
  # restore, 2026-10-08.
  local ar_note="none -- no geo.place in this database"
  if [ "$(psql_q "SELECT to_regclass('geo.place') IS NOT NULL")" = t ]; then
    docker exec "$DB_CONTAINER" psql -U "$DB_USER" -d "$DB_NAME" -At -c \
        "COPY (SELECT place_id, name_ar FROM geo.place WHERE name_ar IS NOT NULL ORDER BY place_id) TO STDOUT WITH (FORMAT csv)" \
        > "$work/geo-arabic.csv" || die "could not export the Arabic place names"
    ar_note="$(wc -l < "$work/geo-arabic.csv" | tr -d ' ') name(s), by place_id"
    ok "Arabic place names: $ar_note"
  fi

  # The only piece of live state on this box that is not in a database: the
  # per-driver sign-in codes the auth guard checks. Restoring the databases
  # without this brings every driver back and leaves none of them able to sign
  # in, which would look like the backup worked.
  #
  # Found rather than assumed. Until 2026-10-06 this script was *copied* to
  # /root/backup.sh for the systemd unit, where a relative path resolved to
  # /root/auth-guard/ and would silently have found nothing; the unit now runs
  # /opt/ny/local-stack/backup.sh, but a copy run from anywhere stays safe. A backup that
  # quietly leaves something out is the failure this whole file is written
  # against, so the lookup is explicit and the miss is logged.
  local codes_note="absent — no driver is enrolled"
  local codes=""
  for candidate in \
      "${DRIVER_CODES:-}" \
      "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/auth-guard/driver-codes.json" \
      "/opt/ny/local-stack/auth-guard/driver-codes.json"; do
    if [ -n "$candidate" ] && [ -f "$candidate" ]; then codes="$candidate"; break; fi
  done
  if [ -n "$codes" ]; then
    cp "$codes" "$work/driver-codes.json"
    codes_note="$(grep -c '"salt"' "$work/driver-codes.json" || echo 0) enrolled driver(s)"
    ok "driver codes: $codes_note"
  else
    info "no driver-codes.json to include"
  fi

  # Driver papers: identity documents, the only user-created files on this box
  # outside a database, and the evidence for why a driver was approved. The
  # rows in movin.driver_document come with the `movin` schema; without this
  # they would restore as rows pointing at files that no longer exist.
  #
  # Read through a container and never from /var/lib/docker/volumes/… on the
  # host: that path is docker's own business, it has changed across versions,
  # and reaching into it behind docker's back is how a backup breaks on an
  # upgrade morning.
  local docs_note="not included"
  if [ -n "$DOCS_VOLUME" ]; then
    docker volume inspect "$DOCS_VOLUME" >/dev/null 2>&1 \
      || die "volume $DOCS_VOLUME is missing — driver documents would be left out. Set DOCS_VOLUME='' to skip on purpose."
    docker run --rm -v "$DOCS_VOLUME":/v:ro -v "$work":/out alpine \
        tar -czf /out/documents.tar.gz -C /v . \
      || die "could not read $DOCS_VOLUME"
    local nfiles
    nfiles=$(docker run --rm -v "$DOCS_VOLUME":/v:ro alpine sh -c 'find /v -type f | wc -l' | tr -d '[:space:]')
    docs_note="$nfiles file(s), $(du -h "$work/documents.tar.gz" | cut -f1) compressed"
    ok "documents: $docs_note"
  else
    info "DOCS_VOLUME is empty — driver documents deliberately excluded"
  fi

  # A restore months from now is done by someone who was not here today.
  cat > "$work/MANIFEST.txt" <<EOF
Movin DZ backup
taken            $stamp (UTC)
host             $(hostname)
atlas database   $DB_NAME, schemas: $DATA_SCHEMAS
passetto         $PASSETTO_DB
driver codes     $codes_note
arabic names     $ar_note
documents        $docs_note
                 driver papers from the $DOCS_VOLUME volume; restore with
                 docker run --rm -v $DOCS_VOLUME:/v -v \$PWD:/in alpine \\
                   tar -xzf /in/documents.tar.gz -C /v
                 restore by hand to local-stack/auth-guard/driver-codes.json;
                 without it no driver can sign in, whatever the database says
postgres         $(docker exec "$DB_CONTAINER" psql -U "$DB_USER" -At -c 'SHOW server_version;' 2>/dev/null)

NOT included, because it is rebuilt rather than restored:
  geo      the place-search index — run ./geocoder-prepare.sh (~5 min); its
           Arabic names ARE included (geo-arabic.csv) and ./restore.sh puts them back
  public   PostGIS spatial_ref_sys, ships with the extension
  tiger    PostGIS reference data

rows at the time of the backup
  riders    $(psql_q 'SELECT count(*) FROM atlas_app.person;')
  bookings  $(psql_q 'SELECT count(*) FROM atlas_app.booking;')
  rides     $(psql_q 'SELECT count(*) FROM atlas_app.ride;')
  drivers   $(psql_q 'SELECT count(*) FROM atlas_driver_offer_bpp.person;')

to check it     ./backup.sh restore <this file>     (scratch database)
to rehearse it  ./restore.sh rehearse <this file>   (throwaway copy, every part)
to put it back  ./restore.sh live <this file>
EOF

  say "packing and encrypting"
  # An `[ -f … ] && members+=(…)` here would abort the whole backup under
  # `set -e` on the day nobody is enrolled: the test is the last command of the
  # list, so its failure is the line's exit status.
  local members=(atlas.sql passetto.sql MANIFEST.txt)
  if [ -f "$work/geo-arabic.csv" ]; then members+=(geo-arabic.csv); fi
  if [ -f "$work/driver-codes.json" ]; then members+=(driver-codes.json); fi
  if [ -f "$work/documents.tar.gz" ]; then members+=(documents.tar.gz); fi
  tar -czf "$work/bundle.tar.gz" -C "$work" "${members[@]}" \
    || die "tar failed"
  gpg --batch --yes --symmetric --cipher-algo AES256 \
      --passphrase-file "$PASS_FILE" \
      --output "$out" "$work/bundle.tar.gz" \
    || die "gpg failed"
  chmod 600 "$out"
  ok "$out ($(du -h "$out" | cut -f1))"

  say "offsite copy"
  if [ -z "$RCLONE_REMOTE" ]; then
    bad "RCLONE_REMOTE is not set — this backup is still ON THIS SERVER ONLY"
    info "It survives a container rebuild. It does not survive losing the VPS,"
    info "which is the failure it exists for."
  elif ! command -v rclone >/dev/null; then
    bad "rclone is not installed — backup stayed on this server"
  elif rclone copy "$out" "$RCLONE_REMOTE" --no-traverse 2>&1 | sed 's/^/       /'; then
    ok "copied to $RCLONE_REMOTE"
  else
    bad "upload failed — backup stayed on this server"
  fi

  prune
  say "done"
  info "$(ls -1 "$BACKUP_DIR"/movin-*.tar.gz.gpg 2>/dev/null | wc -l) backup(s) held locally"
}

# ── retention ───────────────────────────────────────────────────────────────
# Keep every backup for KEEP_DAILY days, then one per week for KEEP_WEEKLY
# weeks. Deliberately conservative: deleting a backup is the one thing here
# that cannot be undone.
prune() {
  say "pruning"
  local now cutoff_daily cutoff_weekly kept=0 removed=0
  now=$(date -u +%s)
  cutoff_daily=$((now - KEEP_DAILY * 86400))
  cutoff_weekly=$((now - (KEEP_DAILY + KEEP_WEEKLY * 7) * 86400))

  declare -A week_seen=()
  # Newest first, so the first backup seen in any week is the one kept.
  while IFS= read -r f; do
    [ -n "$f" ] || continue
    local ts age epoch
    ts=$(basename "$f" | sed -E 's/^movin-(.*)\.tar\.gz\.gpg$/\1/')
    epoch=$(date -u -d "${ts:0:8} ${ts:9:2}:${ts:11:2}:${ts:13:2}" +%s 2>/dev/null) || continue

    if [ "$epoch" -ge "$cutoff_daily" ]; then
      kept=$((kept + 1)); continue
    fi
    if [ "$epoch" -lt "$cutoff_weekly" ]; then
      rm -f "$f"; removed=$((removed + 1)); continue
    fi
    local week
    week=$(date -u -d "@$epoch" +%G-W%V)
    if [ -z "${week_seen[$week]:-}" ]; then
      week_seen[$week]=1; kept=$((kept + 1))
    else
      rm -f "$f"; removed=$((removed + 1))
    fi
  done < <(ls -1t "$BACKUP_DIR"/movin-*.tar.gz.gpg 2>/dev/null)

  ok "kept $kept, removed $removed"
}

# ── restore, into a scratch database ────────────────────────────────────────
# Never into the live one. A restore is run at the worst possible moment by
# someone who is already having a bad day; it must not be able to make things
# worse by overwriting a database that was merely damaged.
restore() {
  local file="${1:-}"
  [ -n "$file" ] || die "usage: ./backup.sh restore <file>"
  [ -f "$file" ] || die "no such file: $file"
  require_passphrase

  local work scratch
  work=$(mktemp -d)
  trap 'rm -rf "$work"' RETURN
  scratch="movin_restore_test"

  say "decrypting"
  gpg --batch --yes --quiet --decrypt --passphrase-file "$PASS_FILE" \
      --output "$work/bundle.tar.gz" "$file" || die "could not decrypt — wrong passphrase?"
  tar -xzf "$work/bundle.tar.gz" -C "$work" || die "archive is corrupt"
  ok "decrypted and unpacked"
  sed 's/^/       /' "$work/MANIFEST.txt"

  say "restoring into $scratch (the live database is not touched)"
  docker exec "$DB_CONTAINER" psql -U "$DB_USER" -d postgres -c \
    "DROP DATABASE IF EXISTS $scratch;" >/dev/null
  docker exec "$DB_CONTAINER" psql -U "$DB_USER" -d postgres -c \
    "CREATE DATABASE $scratch;" >/dev/null
  docker exec -i "$DB_CONTAINER" psql -U "$DB_USER" -d "$scratch" -q \
    < "$work/atlas.sql" > "$work/restore.log" 2>&1
  ok "restored"

  say "checking it against the manifest"
  local want got fail=0
  for pair in "riders:atlas_app.person" "bookings:atlas_app.booking" \
              "rides:atlas_app.ride" "drivers:atlas_driver_offer_bpp.person"; do
    local label="${pair%%:*}" tbl="${pair##*:}"
    want=$(grep -E "^  $label " "$work/MANIFEST.txt" | awk '{print $2}')
    got=$(docker exec "$DB_CONTAINER" psql -U "$DB_USER" -d "$scratch" -At -c \
          "SELECT count(*) FROM $tbl;" 2>/dev/null)
    if [ "$want" = "$got" ]; then
      ok "$label $got"
    else
      bad "$label: manifest says $want, restored $got"; fail=1
    fi
  done

  # The documents, unpacked into scratch and actually opened. A tar that lists
  # cleanly can still hold truncated files, and "the archive exists" is not the
  # claim anybody needs.
  if [ -f "$work/documents.tar.gz" ]; then
    say "checking the driver documents"
    mkdir -p "$work/documents"
    tar -xzf "$work/documents.tar.gz" -C "$work/documents" \
      || die "documents archive is corrupt"
    local dgot dwant dfirst dmagic
    dgot=$(find "$work/documents" -type f | wc -l | tr -d '[:space:]')
    dwant=$(grep -E '^documents ' "$work/MANIFEST.txt" | awk '{print $2}')
    if [ "$dgot" = "$dwant" ]; then
      ok "documents $dgot"
    else
      bad "documents: manifest says $dwant, restored $dgot"; fail=1
    fi
    dfirst=$(find "$work/documents" -type f | head -1)
    if [ -n "$dfirst" ]; then
      dmagic=$(head -c 4 "$dfirst" | od -An -tx1 | tr -d ' \n')
      case "$dmagic" in
        ffd8ff*)   ok "first file opens: JPEG" ;;
        89504e47)  ok "first file opens: PNG" ;;
        25504446)  ok "first file opens: PDF" ;;
        52494646)  ok "first file opens: WEBP" ;;
        *)         bad "first file is not an image or a PDF (magic $dmagic)"; fail=1 ;;
      esac
    else
      info "no document files in this backup yet"
    fi
  fi

  docker exec "$DB_CONTAINER" psql -U "$DB_USER" -d postgres -c \
    "DROP DATABASE IF EXISTS $scratch;" >/dev/null
  say "scratch database dropped"

  [ "$fail" = 0 ] || die "restore did not match the manifest"
  ok "this backup restores correctly"
  info "A backup nobody has restored is a guess. This one is not."
}

list() {
  say "backups in $BACKUP_DIR"
  if ! ls "$BACKUP_DIR"/movin-*.tar.gz.gpg >/dev/null 2>&1; then
    bad "none"; return
  fi
  ls -1t "$BACKUP_DIR"/movin-*.tar.gz.gpg | while read -r f; do
    printf '       %-46s %6s  %s\n' "$(basename "$f")" \
      "$(du -h "$f" | cut -f1)" "$(date -u -r "$f" '+%Y-%m-%d %H:%M UTC')"
  done
  local newest age
  newest=$(ls -1t "$BACKUP_DIR"/movin-*.tar.gz.gpg | head -1)
  age=$(( ( $(date -u +%s) - $(date -u -r "$newest" +%s) ) / 3600 ))
  echo
  if [ "$age" -gt 26 ]; then
    bad "newest backup is ${age}h old — the timer is not running"
  else
    ok "newest backup is ${age}h old"
  fi
}

# ── the nightly timer ───────────────────────────────────────────────────────
# systemd rather than cron: it survives reboots the same way, and `systemctl
# status` tells you why the last run failed instead of mailing root, which
# nobody reads.
install_timer() {
  # The units live in git, in systemd/ beside this file, and a release
  # (ops/deploy.sh) installs them -- this is only for a box that has never
  # had a release. Until 2026-10-06 the unit was written from here, pointed at
  # wherever this copy happened to be, and on the server that was a hand copy
  # in /root that no release ever updated.
  local here
  here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
  [ -f "$here/systemd/movin-backup.service" ] || die "no $here/systemd/movin-backup.service"
  install -m 644 "$here/systemd/movin-backup.service" "$here/systemd/movin-backup.timer" \
    /etc/systemd/system/
  grep -q "^ExecStart=$here/backup.sh$" /etc/systemd/system/movin-backup.service \
    || say "!! the unit runs $(sed -n 's/^ExecStart=//p' /etc/systemd/system/movin-backup.service), not this copy"
  systemctl daemon-reload
  systemctl enable --now movin-backup.timer
  say "timer installed"
  systemctl list-timers movin-backup.timer --no-pager | sed 's/^/       /'
}

case "${1:-take}" in
  take|"")  take ;;
  restore)  restore "${2:-}" ;;
  list)     list ;;
  install)  install_timer ;;
  prune)    prune ;;
  *)        die "unknown command: $1  (take | restore <file> | list | install)" ;;
esac

# ── setting it up, once ─────────────────────────────────────────────────────
#
#   1. A passphrase. This is the only thing standing between the backup file and
#      every rider's phone number, so it goes in the company password manager
#      next to the release signing key -- not only on this server.
#
#        openssl rand -base64 32 > /root/.movin-backup-pass
#        chmod 600 /root/.movin-backup-pass
#
#      Lose it and the backups are unreadable. That is the intended behaviour.
#
#   2. Somewhere off this server. With rclone configured against the company's
#      Google Drive:
#
#        rclone config                       # once, interactively
#        RCLONE_REMOTE=movin-drive:movin-backups ./backup.sh install
#
#   3. Prove it works, rather than assuming:
#
#        ./backup.sh
#        ./backup.sh restore /var/backups/movin/movin-<stamp>.tar.gz.gpg
