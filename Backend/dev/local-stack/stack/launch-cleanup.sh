#!/usr/bin/env bash
# The day-1 clean-up: the twelve simulated cars and the two test passengers go,
# so that only real drivers and passengers remain. Written 2026-10-18 from what
# was done by hand on 2026-10-01 (local-stack/docs/countries.md, *The test fleet*).
#
#   sudo ./launch-cleanup.sh               who would go and what stays. Writes nothing.
#   sudo ./launch-cleanup.sh --apply       do it, after a backup, and after you type
#                                          the confirmation it asks for
#
#   Rehearsal, on a throwaway copy of last night's backup (nothing live is touched):
#     REHEARSE_HOOK='./launch-cleanup.sh --rehearsal "$R_PG" --apply' \
#       ./restore.sh rehearse offsite:latest
#
# ── What "remove" means ─────────────────────────────────────────────────────
# Each account goes through the console's own db/deletion/anonymise.sql, the
# file the Suppressions screen runs: name, number (and its hash), sessions, cars,
# papers and positions erased in one transaction per account; the rides, the
# bookings, the ratings and the wallet LEDGER stay, because they are the
# drivers' payment history and revenue is the sum of the ledger
# (website repo, docs/deletion/README.md). So after it, the console still counts
# the test rides and the test day-charges the way it counted them before
# (« test accounts are counted, not excluded », owner, 2026-09-15): that is a
# decision for the owner, not something this script does.
#
# ── Who it will touch ───────────────────────────────────────────────────────
# Only the accounts launch-cleanup-targets.sql names, by country code and
# number. It refuses if that selection is not what was expected (more than
# twelve cars, more than two passengers) and does nothing if it finds none.
#
# ── What it does, in this order (live) ──────────────────────────────────────
#   1. checks: root, the database answers, anonymise.sql is where it should be;
#   2. the list, and what stays for each (ledger entries, paid top-ups);
#   3. REFUSES to go on if a target has uploaded papers (the files are on a
#      volume SQL cannot reach: see `--papers-handled`);
#   4. a full backup (./backup.sh) and a dump of the three account schemas into
#      /root/pre-launch-clean/ (root-only): the way back if a number was wrong.
#      That dump holds REAL people's data: it is one of the temporary safety
#      copies the runbook has you remove once the launch has settled;
#   5. `fleet-service.sh uninstall` BEFORE the erase, so the daemon cannot sign
#      the cars back in;
#   6. anonymise.sql, once per account;
#   7. the Redis entries of those people (sessions and cached profiles);
#   8. the checks again: nobody on the list is left, the real account counts
#      are what they were, nothing on the list can be found by its number.
# `--rehearsal CONTAINER` does steps 1, 2, 3, 6 and 8 against that database and
# nothing else: no backup, no fleet, no Redis, no files.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$HERE"

PG="${PG:-ny-postgres}"
DB="${DB:-atlas_dev}"
REDIS="${REDIS:-ny-redis}"
ANON="${ANONYMISE_SQL:-/opt/ny/movin-website/db/deletion/anonymise.sql}"
CODES="${DRIVER_CODES:-$HERE/auth-guard/driver-codes.json}"
SAFETY_DIR="${SAFETY_DIR:-/root/pre-launch-clean}"
# Not in db/: a release applies every NEW db/*.sql to the live database, and this
# file is a read of people's numbers, not a migration.
TARGETS_SQL="$HERE/launch-cleanup-targets.sql"
ACTOR="${ACTOR:-owner: test accounts removed for the launch $(date -u +%F)}"
NOTE="${NOTE:-launch clean-up: simulated cars and test passengers}"
MAX_DRIVERS=12
MAX_RIDERS=2

APPLY=0 REHEARSAL=0 PAPERS_HANDLED=0
while [ $# -gt 0 ]; do
  case "$1" in
    --apply) APPLY=1 ;;
    --rehearsal) REHEARSAL=1; PG="${2:?--rehearsal needs the database container}"; shift ;;
    --papers-handled) PAPERS_HANDLED=1 ;;
    -h|--help) sed -n '2,45p' "$0"; exit 0 ;;
    *) echo "usage: ./launch-cleanup.sh [--apply] [--rehearsal CONTAINER] [--papers-handled]" >&2; exit 2 ;;
  esac
  shift
done

say() { printf '\n\033[1m== %s\033[0m\n' "$*"; }
ok() { printf '   \033[1;32mok  \033[0m%s\n' "$*"; }
info() { printf '       %s\n' "$*"; }
die() { printf '\n\033[1;31mSTOPPED: \033[0m%s\n' "$*" >&2; exit 1; }

# One door to the database, so a test or a rehearsal can point it elsewhere.
# `pg` takes the SQL as an argument; `pgf` takes it on stdin (`pgf ... -f - < file`),
# because the container cannot see this host's files -- and only that one is run
# with -i: `docker exec -i` reads the caller's stdin, and inside a `while read`
# loop it would swallow the rest of the loop.
pg() { docker exec "$PG" psql -U postgres -d "$DB" -v ON_ERROR_STOP=1 -X -q "$@"; }
pgf() { docker exec -i "$PG" psql -U postgres -d "$DB" -v ON_ERROR_STOP=1 -X -q "$@"; }
count() { pg -At -c "$1"; }

say "checks"
[ "$REHEARSAL" = 1 ] || [ "$(id -u)" = 0 ] || die "run it with sudo"
docker inspect -f '{{.State.Running}}' "$PG" 2>/dev/null | grep -qx true || die "the database container $PG is not running"
pg -At -c 'select 1' >/dev/null || die "the database does not answer"
ok "the database $PG / $DB answers"
[ -f "$ANON" ] || die "anonymise.sql is not at $ANON (the website release puts it there)"
[ -f "$TARGETS_SQL" ] || die "$TARGETS_SQL is missing"
ok "anonymise.sql is the console's own file ($(sha256sum "$ANON" | cut -c1-12))"

say "the accounts to remove"
TARGETS="$(pgf -At -F'|' -f - < "$TARGETS_SQL")"
if [ -z "$TARGETS" ]; then
  ok "none of the listed numbers exists any more: nothing to do"
  exit 0
fi
n_drv=$(grep -c '^driver|' <<<"$TARGETS" || true)
n_rid=$(grep -c '^rider|' <<<"$TARGETS" || true)
printf '   %-7s %-38s %-5s %-12s %s\n' side id cc number name
while IFS='|' read -r side id cc num first; do
  printf '   %-7s %-38s %-5s %-12s %s\n' "$side" "$id" "$cc" "$num" "$first"
done <<<"$TARGETS"
info "$n_drv driver(s) and $n_rid passenger(s)"
[ "$n_drv" -le "$MAX_DRIVERS" ] || die "$n_drv drivers match; at most $MAX_DRIVERS were expected. The list or the data is not what this script was written for."
[ "$n_rid" -le "$MAX_RIDERS" ] || die "$n_rid passengers match; at most $MAX_RIDERS were expected."

DRIVER_IDS="$(awk -F'|' '$1=="driver"{printf "%s'"'"'%s'"'"'", sep, $2; sep=","}' <<<"$TARGETS")"
ALL_IDS="$(awk -F'|' '{printf "%s'"'"'%s'"'"'", sep, $2; sep=","}' <<<"$TARGETS")"

say "what stays, and what would be left behind"
if [ -n "$DRIVER_IDS" ]; then
  led=$(count "select count(*) from movin.wallet_entry where driver_id in ($DRIVER_IDS)")
  paid=$(count "select count(*) from movin.wallet_topup where driver_id in ($DRIVER_IDS) and status in ('paid','completed')")
  info "wallet ledger entries of these drivers: $led (they stay: revenue is the sum of the ledger)"
  if [ "$paid" != 0 ]; then
    info "WARNING: $paid paid top-up(s) from these accounts, i.e. real money moved through one of them."
    info "         The ledger keeps it either way; check it is what you expect before going on."
  fi
  docs=$(count "select count(*) from movin.driver_document where driver_id in ($DRIVER_IDS)")
  info "uploaded papers: $docs"
  if [ "$docs" != 0 ] && [ "$PAPERS_HANDLED" != 1 ]; then
    pg -At -c "select '         ' || stored_path from movin.driver_document where driver_id in ($DRIVER_IDS)" || true
    die "these drivers have papers on the documents volume, which SQL cannot remove. Delete those files first (or decide to keep them), then re-run with --papers-handled."
  fi
fi
if [ "$REHEARSAL" != 1 ] && [ -f "$CODES" ]; then
  for num in $(awk -F'|' '$1=="driver"{print $4}' <<<"$TARGETS"); do
    grep -q "\"$num\"" "$CODES" && info "WARNING: $num has an enrolment code in $CODES; remove it by hand."
  done
fi
info "not touched by SQL, so check by hand: profile photographs on the maps-shim avatars volume (keyed by person id)."

BEFORE_REAL_DRIVERS=$(count "select count(*) from atlas_driver_offer_bpp.person where role = 'DRIVER'")
BEFORE_REAL_RIDERS=$(count "select count(*) from atlas_app.person")

if [ "$APPLY" != 1 ]; then
  say "dry run: nothing was written"
  info "to do it:  sudo ./launch-cleanup.sh --apply"
  exit 0
fi

say "the confirmation"
WANT="erase $((n_drv + n_rid)) accounts"
# A rehearsal runs on a throwaway copy, usually from a hook with no terminal.
if [ "$REHEARSAL" = 1 ] || [ "${CONFIRM:-}" = "$WANT" ]; then answer="$WANT"
else printf '       these %s accounts cannot be un-erased. Type  %s  to go on: ' "$((n_drv + n_rid))" "$WANT"; read -r answer < /dev/tty; fi
[ "$answer" = "$WANT" ] || die "not confirmed; nothing changed"

TOKENS=""
if [ "$REHEARSAL" != 1 ]; then
  say "a backup, and a dump of the account schemas"
  ./backup.sh || die "the backup failed"
  umask 077
  mkdir -p "$SAFETY_DIR"
  chmod 700 "$SAFETY_DIR"
  OUT="$SAFETY_DIR/accounts-$(date -u +%Y%m%d-%H%M%S).sql"
  docker exec "$PG" pg_dump -U postgres -d "$DB" -n atlas_app -n atlas_driver_offer_bpp -n movin > "$OUT" \
    || die "the dump failed"
  chmod 600 "$OUT"
  ok "$OUT ($(du -h "$OUT" | cut -f1)), root-only. REAL data: remove it when the launch has settled."

  say "the simulated fleet stops answering"
  ./fleet-service.sh uninstall || die "could not uninstall the fleet service"
  ok "movin-fleet is gone: nothing will sign the cars back in"

  # The sessions are cached in Redis under the token, not under the person, so
  # the tokens have to be read before the erase deletes them.
  TOKENS="$( { pg -At -c "select token from atlas_driver_offer_bpp.registration_token where entity_id::text in ($ALL_IDS)";
               pg -At -c "select token from atlas_app.registration_token where entity_id::text in ($ALL_IDS)"; } 2>/dev/null || true)"
fi

say "erasing"
while IFS='|' read -r side id cc num first; do
  out="$(pgf -At -F'|' -v side="$side" -v person_id="$id" -v actor="$ACTOR" -v admin_id= -v note="$NOTE" -f - < "$ANON")" \
    || die "anonymise.sql failed on $side $id: the other accounts are as they were (one transaction each)"
  changed="$(tail -n1 <<<"$out")"
  ok "$side $num $first: $changed row(s) changed"
done <<<"$TARGETS"

if [ "$REHEARSAL" != 1 ]; then
  say "Redis: sessions and cached profiles of these people"
  del=0
  keys="$( { while read -r id; do [ -n "$id" ] && docker exec "$REDIS" redis-cli --scan --pattern "*${id}*"; done < <(awk -F'|' '{print $2}' <<<"$TARGETS");
             while read -r tok; do [ -n "$tok" ] && docker exec "$REDIS" redis-cli --scan --pattern "*${tok}*"; done <<<"$TOKENS"; } | sort -u)"
  while read -r key; do
    [ -n "$key" ] || continue
    docker exec "$REDIS" redis-cli DEL "$key" >/dev/null && del=$((del + 1))
  done <<<"$keys"
  ok "$del Redis key(s) dropped"
fi

say "after"
left="$(pgf -At -F'|' -f - < "$TARGETS_SQL")"
[ -z "$left" ] || die "these are still found by their number after the erase: $left"
ok "none of the listed numbers can be found any more"
AFTER_DRV=$(count "select count(*) from atlas_driver_offer_bpp.person where role = 'DRIVER'")
AFTER_RID=$(count "select count(*) from atlas_app.person")
[ "$AFTER_DRV" = "$BEFORE_REAL_DRIVERS" ] && [ "$AFTER_RID" = "$BEFORE_REAL_RIDERS" ] \
  && ok "the person rows are all still there ($AFTER_DRV drivers, $AFTER_RID passengers): erased, not deleted" \
  || info "person rows: drivers $BEFORE_REAL_DRIVERS -> $AFTER_DRV, passengers $BEFORE_REAL_RIDERS -> $AFTER_RID"
with_number=$(count "select count(*) from atlas_driver_offer_bpp.person where role = 'DRIVER' and unencrypted_mobile_number is not null")
info "drivers that still have a phone number on record: $with_number (these should be the real ones)"
audit=$(count "select count(*) from movin.admin_audit where actor_email = '$(printf '%s' "$ACTOR" | sed "s/'/''/g")'")
ok "$audit audit row(s) written under: $ACTOR"
say "done"
if [ "$REHEARSAL" = 1 ]; then info "(a rehearsal: this was a throwaway copy)"
else
  info "next: ./fleet-service.sh status (gone), the console's Chauffeurs and Passagers lists, then a search from a real phone"
  info "      must find NO car: a search that returns an estimate now means a simulated driver is still online."
fi
