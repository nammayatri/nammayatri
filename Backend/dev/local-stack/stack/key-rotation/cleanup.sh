#!/usr/bin/env bash
# After the key rotation: remove what was kept only for its rollback. Run as
# root, from /opt/ny/local-stack:
#
#   sudo bash key-rotation/cleanup.sh            what would go, and the checks
#   sudo bash key-rotation/cleanup.sh --apply    do it
#
#   ny-passetto-db      the old key store (upstream's seed keys) and its
#                       anonymous volume. Since 2026-10-15 compose.keys.yml puts
#                       it in the `old-keys` profile, so compose no longer makes it.
#   /root/key-rotation.* the window's work folders: before.dump (both schemas
#                       as they were before the switch, numbers under the old
#                       keys), the two salts, number-change.env.before.
#
# After it, window.sh rollback and configs are gone for good: they read the
# old keys from ny-passetto-db. Backups taken before the switch still restore,
# each carries its own copy of the key database it was made with.
#
# Nothing is removed unless every check passes: the live passetto reads the
# new key database, .env names the overlay, compose would neither recreate
# passetto/rider/driver nor make the old store again, nothing is connected to
# the old store, and a stored number decrypts through the live passetto.
set -euo pipefail
cd "$(dirname "$0")/.."
APPLY=0; [ "${1:-}" = --apply ] && APPLY=1
OLD=ny-passetto-db
say() { printf '\n== %s\n' "$*"; }
ok() { printf '   ok  %s\n' "$*"; }
die() { printf '\nSTOPPED, nothing removed: %s\n' "$*" >&2; exit 1; }
[ "$(id -u)" = 0 ] || die "run it with sudo"

say "checks"
docker inspect ny-passetto --format '{{range .Config.Env}}{{println .}}{{end}}' | grep -q '@ny-passetto-keys-db:' \
  || die "the live passetto does not read ny-passetto-keys-db"
ok "the live passetto reads the new key database"
grep -q '^COMPOSE_FILE=.*key-rotation/compose.keys.yml' .env || die ".env's COMPOSE_FILE does not name the key overlay"
ok ".env names the key overlay"
docker compose config --services | grep -qx passetto-db && die "compose still makes passetto-db: release compose.keys.yml first"
ok "compose no longer makes passetto-db"
dry=$(docker compose --dry-run up -d --no-deps passetto rider-app driver-app 2>&1)
[ "$(grep -c ' Running *$' <<<"$dry")" = 3 ] || { echo "$dry" >&2; die "compose would recreate passetto, rider or driver"; }
ok "compose would leave passetto, rider and driver as they are"
if docker inspect "$OLD" >/dev/null 2>&1; then
  n=$(docker exec "$OLD" psql -U passetto -d passetto -tAc \
      "select count(*) from pg_stat_activity where datname='passetto' and pid<>pg_backend_pid()")
  [ "$n" = 0 ] || die "$n connection(s) to $OLD"
  ok "nothing is connected to $OLD"
fi
enc=$(docker exec ny-postgres psql -U postgres -d atlas_dev -tAc \
  "select mobile_number_encrypted from atlas_app.person
    where mobile_number_encrypted like '0.1.0|%' limit 1")
[ -n "$enc" ] || die "no stored number to try"
out=$(curl -s --max-time 10 -X POST -H 'Content-Type: application/json' \
  -d "{\"value\":\"$enc\"}" http://127.0.0.1:8021/decrypt)
grep -q '"value":"S\\"' <<<"$out" || die "a stored number does not decrypt through the live passetto"
ok "a stored number decrypts through the live passetto (not shown)"

say "what goes"
if docker inspect "$OLD" >/dev/null 2>&1; then
  echo "   container $OLD ($(docker inspect "$OLD" --format '{{.State.Status}}, created {{.Created}}' | cut -c1-40)), and its anonymous volume"
else
  echo "   container $OLD: already gone"
fi
shopt -s nullglob
dirs=(/root/key-rotation.*)
for d in "${dirs[@]}"; do echo "   $d ($(du -sh "$d" | cut -f1)): $(ls "$d" | tr '\n' ' ')"; done
[ ${#dirs[@]} = 0 ] && echo "   /root/key-rotation.*: none"

if [ "$APPLY" = 0 ]; then printf '\n   dry run: nothing removed. --apply to do it.\n'; exit 0; fi

say "removing"
if docker inspect "$OLD" >/dev/null 2>&1; then
  docker stop "$OLD" >/dev/null && docker rm -v "$OLD" >/dev/null
  ok "$OLD removed, with its volume"
fi
for d in "${dirs[@]}"; do rm -rf -- "$d"; ok "$d removed"; done

say "after"
out=$(curl -s --max-time 10 -X POST -H 'Content-Type: application/json' \
  -d "{\"value\":\"$enc\"}" http://127.0.0.1:8021/decrypt)
grep -q '"value":"S\\"' <<<"$out" || die "a stored number no longer decrypts -- look at ny-passetto now"
ok "a stored number still decrypts through the live passetto"
[ "$(curl -s -o /dev/null -w '%{http_code}' --max-time 10 http://127.0.0.1:8013/v2/auth/channels)" = 200 ] \
  && ok "the rider app answers" || echo "   !!  the rider app did not answer 200: look at it"
echo "   containers running: $(docker ps -q | wc -l)"
