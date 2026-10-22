#!/usr/bin/env bash
#
# Apply the tariff — and then make the backend read it.
#
# ── Why this is a script and not just `psql -f algeria-tariff.sql` ──────────
# **The SQL alone changes nothing a rider can see.** The driver service caches
# fare policies in Redis and does not notice a row changing underneath it. Run
# the SQL on its own and every statement reports success, the table holds the
# new numbers, and the app keeps quoting the old ones indefinitely. There is no
# error and nothing in any log.
#
# That cost half an hour on 2026-08-13: prices updated, estimates still 258.
#
# There are **two** caches and they are not named alike:
#
#   driver-offer:CachedQueries:FarePolicy:*        the fares themselves
#   driver-offer:CachedQueries:RestrictExtraFee:*  the cap on the driver's extra
#
# The second is spelled `RestrictExtraFee` while its table is
# `restricted_extra_fare` — different word, so a scan for `*Fare*` misses it
# entirely. Clearing only the first updates the prices and silently leaves the
# driver's permitted extra at its old value, which is exactly what happened.
#
# Deleting by pattern, never FLUSHALL: the same Redis holds the auth sessions
# and the OTP lockout counters, so flushing everything would sign out every
# tester and reset the brute-force protection.
#
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# Both countries unless one file is named (2026-10-22, the owner: « never change
# one country without the other »). Each file is keyed to its own merchant, so
# applying both is what keeps the two tariffs moving together.
if [ $# -gt 0 ]; then
  SQLS=("$1")
else
  SQLS=("$HERE/db/algeria-tariff.sql" "$HERE/db/mauritania-tariff.sql")
fi

say() { printf '\n\033[1m== %s\033[0m\n' "$*"; }
ok()  { printf '   \033[1;32mok  \033[0m%s\n' "$*"; }
bad() { printf '   \033[1;31mBAD \033[0m%s\n' "$*"; }

for SQL in "${SQLS[@]}"; do
  [ -f "$SQL" ] || { bad "no tariff at $SQL"; exit 1; }
done

# ONE transaction for every file: the countries change together or not at all.
# Each file wraps itself in BEGIN/COMMIT; those lines are taken out and the
# whole is wrapped once, so a failure in the second file rolls back the first.
COMBINED="$(mktemp)"
trap 'rm -f "$COMBINED"' EXIT
{
  echo 'BEGIN;'
  for SQL in "${SQLS[@]}"; do
    echo "-- ── $(basename "$SQL") ──"
    grep -vE '^[[:space:]]*(BEGIN|COMMIT)[[:space:]]*;[[:space:]]*$' "$SQL"
  done
  echo 'COMMIT;'
} > "$COMBINED"

say "applying $(for SQL in "${SQLS[@]}"; do basename "$SQL"; done | paste -sd' ' -), in one transaction"
# docker cp then -f, never `docker exec -i psql < file`: when this script is
# itself piped to a remote shell, -i makes docker eat the rest of it.
docker cp "$COMBINED" ny-postgres:/tmp/tariff.sql >/dev/null
if docker exec ny-postgres psql -U postgres -d atlas_dev \
     -v ON_ERROR_STOP=1 -f /tmp/tariff.sql; then
  ok "applied"
else
  bad "SQL failed — rolled back, nothing cleared: the old tariff is still live in every country"
  exit 1
fi

say "clearing the fare caches"
cleared=0
for pat in 'driver-offer:CachedQueries:FarePolicy:*' \
           'driver-offer:CachedQueries:RestrictExtraFee:*'; do
  for k in $(docker exec ny-redis redis-cli --scan --pattern "$pat"); do
    docker exec ny-redis redis-cli DEL "$k" >/dev/null
    cleared=$((cleared + 1))
  done
done
ok "$cleared key(s) cleared, $(docker exec ny-redis redis-cli DBSIZE | tr -d '\r') left in redis"

say "what a rider will now be quoted"
docker exec ny-postgres psql -U postgres -d atlas_dev -c \
  "SELECT CASE merchant_id WHEN 'favorit0-0000-0000-0000-00000favorit' THEN 'Mauritanie (MRU)'
                           WHEN 'algeria0-0000-0000-0000-00000algeria' THEN 'Algérie (DA)'
                           ELSE merchant_id END               AS country,
          CASE vehicle_variant WHEN 'HATCHBACK' THEN 'Waw / Herbin'
                               WHEN 'SEDAN'     THEN 'Voiture'
                               WHEN 'SUV'       THEN 'Fourgon'
                               ELSE 'Scooter' END             AS vehicle,
          base_distance_fare  AS start,
          per_extra_km_fare   AS per_km,
          dead_km_fare        AS pickup,
          driver_max_extra_fee AS max_extra
     FROM atlas_driver_offer_bpp.fare_policy
    -- Per merchant since 2026-09-13: two countries, two tariffs.
    WHERE merchant_id IN ('favorit0-0000-0000-0000-00000favorit',
                          'algeria0-0000-0000-0000-00000algeria')
    ORDER BY 1, 3;"

echo
echo "   Verify with a real search — the table being right is not the point,"
echo "   the estimate coming back changed is."
