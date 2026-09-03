#!/usr/bin/env bash
# Apply one .sql file to the stack's database.
#
#     ./apply-migration.sh driver-offer-vehicle.sql
#     ./apply-migration.sh                          # list what is here
#
# Both schemas live in one database (`atlas_dev`): `atlas_app` is the rider's
# and `atlas_driver_offer_bpp` is the provider's. The SQL says which.
#
# ── Why this exists ─────────────────────────────────────────────────────────
# The two vehicle migrations were applied by hand, twice, and the second time
# the verification step silently did nothing. The cause is worth keeping:
#
#   docker exec ny-postgres psql ... <<'SQL'   # reaches psql: NOTHING
#
# Without `-i`, `docker exec` does not attach stdin, so the heredoc goes
# nowhere and psql prints an empty result that reads exactly like "no such
# column". And `-i` is not the fix when the script itself arrives on stdin
# (`ssh ny "bash -s" < work.sh`), because then `docker exec -i` eats the rest of
# the script. `docker cp` then `-f`, which is what this does, sidesteps both.
#
# ── What it will not do ─────────────────────────────────────────────────────
# There is no rollback here on purpose. Every migration in this directory adds
# a nullable column, which the *running* binary neither knows nor minds — that
# is what keeps a deploy reversible by an image swap alone. Dropping the column
# afterwards is the one action that would make it irreversible, so it is not
# offered.
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONTAINER="${PG_CONTAINER:-ny-postgres}"
DB="${PG_DB:-atlas_dev}"
USER_="${PG_USER:-postgres}"

say() { printf '\n\033[1m== %s\033[0m\n' "$*"; }
ok()  { printf '   \033[1;32mok  \033[0m%s\n' "$*"; }
bad() { printf '   \033[1;31mBAD \033[0m%s\n' "$*"; }

if [ $# -eq 0 ]; then
  say "migrations in $HERE"
  # Everything that adds or changes schema. The probe-*.sql files are
  # read-only diagnostics and are deliberately not listed as migrations.
  for f in "$HERE"/*.sql; do
    base="$(basename "$f")"
    case "$base" in probe-*) continue ;; esac
    printf '   %s\n' "$base"
  done
  echo
  echo "usage: ./apply-migration.sh <file.sql>"
  exit 0
fi

SQL="$1"
[ -f "$SQL" ] || SQL="$HERE/$1"
if [ ! -f "$SQL" ]; then
  bad "no such file: $1"
  exit 1
fi

BASE="$(basename "$SQL")"

if ! docker inspect "$CONTAINER" >/dev/null 2>&1; then
  bad "container $CONTAINER is not there — is the stack up?"
  exit 1
fi

say "applying $BASE to $DB"
if ! docker cp "$SQL" "$CONTAINER":/tmp/"$BASE" >/dev/null; then
  bad "could not copy it into $CONTAINER"
  exit 1
fi

# -v ON_ERROR_STOP=1 as well as the \set inside the file: the flag wins if a
# file ever forgets it, and a half-applied migration is the thing worth
# refusing hardest.
if docker exec "$CONTAINER" psql -U "$USER_" -d "$DB" \
     -v ON_ERROR_STOP=1 -f /tmp/"$BASE"; then
  ok "$BASE applied"
else
  bad "$BASE failed — nothing after the failing statement ran"
  exit 1
fi

say "columns this database now has that the old binary did not"
docker exec "$CONTAINER" psql -U "$USER_" -d "$DB" -c \
"SELECT table_schema || '.' || table_name AS tbl, column_name, data_type, is_nullable
   FROM information_schema.columns
  WHERE (table_schema, table_name, column_name) IN
        (('atlas_app','driver_offer','vehicle_desc'),
         ('atlas_driver_offer_bpp','search_request','chosen_drivers'))
  ORDER BY 1;"

echo
echo "   Images are not swapped by this script. Do that next, then restart"
echo "   maps-shim if the shim changed too."
