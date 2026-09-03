#!/usr/bin/env bash
#
# Install the rating trigger and backfill the averages.
#
# Run once per server. After that it maintains itself — see ratings-average.sql
# for why this is a trigger and not a systemd timer like backup.sh.
#
# ── No cache to clear, and that is checked rather than assumed ──────────────
# apply-tariff.sh exists almost entirely because the driver service caches fare
# policies in Redis, and SQL alone changes nothing a rider can see. The obvious
# worry is that `person` is cached the same way and this script has the same
# trap.
#
# It is not. Scanned on the live server: there is a
# `driver-offer:CachedQueries:DriverInformation:*` and a `Merchant`, a
# `TransporterConfig` and a `FarePolicy` — and **no `CachedQueries:Person`**.
# The driver's rating is read from Postgres each time an offer is built, which
# is also why setting the column by hand during testing made stars appear
# immediately.
#
# So this script applies SQL and stops. If a `Person` cache ever shows up, this
# is the comment that was wrong and the DEL loop from apply-tariff.sh is the fix.
#
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SQL="${1:-$HERE/ratings-average.sql}"

say() { printf '\n\033[1m== %s\033[0m\n' "$*"; }
ok()  { printf '   \033[1;32mok  \033[0m%s\n' "$*"; }
bad() { printf '   \033[1;31mBAD \033[0m%s\n' "$*"; }

[ -f "$SQL" ] || { bad "no SQL at $SQL"; exit 1; }

say "applying $(basename "$SQL")"
# docker cp then -f, never `docker exec -i psql < file`: when this script is
# itself piped to a remote shell, -i makes docker eat the rest of it.
docker cp "$SQL" ny-postgres:/tmp/ratings-average.sql >/dev/null
if docker exec ny-postgres psql -U postgres -d atlas_dev \
     -v ON_ERROR_STOP=1 -f /tmp/ratings-average.sql; then
  ok "trigger installed and averages backfilled"
else
  bad "SQL failed — person.rating is unchanged"
  exit 1
fi

say "proving the trigger fires"
# Worth doing here rather than trusting the DDL: a trigger that exists and does
# not fire looks identical to one that works, right up until a rider rates a
# ride and nothing happens. This rates a real driver, checks the column moved,
# and puts it back exactly as it was.
#
# Written to a file and copied in, never piped: `docker exec` WITHOUT -i does
# not forward stdin at all, so a heredoc fed straight to it runs psql with no
# input and reports success having done nothing — which is exactly how this
# check silently passed on its first run. Adding -i is the other fix and it is
# the worse one, because -i inside a script that may itself be piped to a remote
# shell eats the rest of the script.
PROOF_SQL="$(mktemp)"
trap 'rm -f "$PROOF_SQL"' EXIT
cat >"$PROOF_SQL" <<'PROOF'
do $$
declare
  victim  char(36);
  was     double precision;
  became  double precision;
  -- Deterministic and exactly 36 characters, which is what rating.id is.
  probe   char(36) := rpad('trigger-proof', 36, '0');
begin
  select driver_id into victim from atlas_driver_offer_bpp.rating limit 1;
  if victim is null then
    raise notice 'no ratings on this server yet — nothing to prove against';
    return;
  end if;

  select rating into was from atlas_driver_offer_bpp.person where id = victim;

  insert into atlas_driver_offer_bpp.rating
        (id, ride_id, rating_value, created_at, updated_at, driver_id)
  values (probe, 'trigger-proof', 1, now(), now(), victim);

  select rating into became from atlas_driver_offer_bpp.person where id = victim;

  delete from atlas_driver_offer_bpp.rating where id = probe;

  if became is distinct from was then
    raise notice 'TRIGGER FIRES: % -> % on insert, and back on delete', was, became;
  else
    raise exception 'TRIGGER DID NOT FIRE: rating stayed at % after inserting a 1', was;
  end if;
end $$;
PROOF

docker cp "$PROOF_SQL" ny-postgres:/tmp/ratings-proof.sql >/dev/null
if docker exec ny-postgres psql -U postgres -d atlas_dev \
     -v ON_ERROR_STOP=1 -f /tmp/ratings-proof.sql; then
  ok "trigger verified"
else
  bad "the trigger is installed but did not fire — stars will stay empty"
  exit 1
fi

say "drivers a rider will now see stars for"
docker exec ny-postgres psql -U postgres -d atlas_dev -c \
  "SELECT p.first_name,
          p.rating,
          (SELECT count(*) FROM atlas_driver_offer_bpp.rating r
            WHERE r.driver_id = p.id) AS ratings
     FROM atlas_driver_offer_bpp.person p
    WHERE p.rating IS NOT NULL
    ORDER BY p.first_name;"

echo
echo "   From here it maintains itself. Rate a ride and the average moves"
echo "   before the next offer is built — there is nothing to schedule."
