#!/usr/bin/env bash
#
# Step 3 of the two-country plan (2026-09-13): Algeria beside Mauritania.
#
#   bash apply-two-countries.sh
#
# In order, and it stops at the first failure:
#   1. back up every table it touches          (nothing is applied without it)
#   2. two-countries-merchants.sql             (coverage, Algerian merchant, registry)
#   3. both tariffs, each keyed to its merchant
#   4. clear the caches that would hide 2 and 3 — by pattern, never FLUSHALL,
#      which would also drop every sign-in and the OTP lockout counters
#   5. restart the three services that read merchants and the registry
#   6. put the pilot's Algerian drivers on the map in Algiers
#   7. print what now exists
#
# Rollback: restore the backup's rows (it is data-only, the tables it names),
# then run this script's step 4 and 5 again.
#
set -uo pipefail
cd "$(dirname "$0")"

DZ=algeria0-0000-0000-0000-00000algeria
MR=favorit0-0000-0000-0000-00000favorit
# Algiers centre — where the app opens for an Algerian account.
LAT=36.7538
LON=3.0588

say() { printf '\n== %s\n' "$*"; }
ok()  { printf '   ok   %s\n' "$*"; }
bad() { printf '   BAD  %s\n' "$*"; }
pga() { docker exec ny-postgres psql -U postgres -d atlas_dev -At -c "$1"; }
pgt() { docker exec ny-postgres psql -U postgres -d atlas_dev -P pager=off -c "$1"; }

# ── 1 ───────────────────────────────────────────────────────────────────────
say "1. backup"
TS=$(date -u +%Y%m%dT%H%M%SZ)
mkdir -p backups && chmod 700 backups
BK="backups/pre-two-countries-$TS.sql"
if docker exec ny-postgres pg_dump -U postgres -d atlas_dev --data-only \
     -t atlas_app.merchant -t atlas_driver_offer_bpp.merchant \
     -t atlas_driver_offer_bpp.fare_policy -t atlas_driver_offer_bpp.restricted_extra_fare \
     -t atlas_driver_offer_bpp.merchant_service_config \
     -t atlas_driver_offer_bpp.merchant_service_usage_config \
     -t atlas_driver_offer_bpp.transporter_config -t atlas_driver_offer_bpp.operating_city \
     -t atlas_registry.subscriber -t atlas_driver_offer_bpp.person \
     -t atlas_driver_offer_bpp.vehicle -t atlas_driver_offer_bpp.driver_information \
     -t atlas_driver_offer_bpp.driver_location > "$BK" && [ -s "$BK" ]; then
  chmod 600 "$BK"
  ok "$BK ($(du -h "$BK" | cut -f1))"
else
  bad "backup failed — NOTHING applied"
  exit 1
fi

# ── 2 ───────────────────────────────────────────────────────────────────────
say "2. merchants, coverage, registry"
docker cp two-countries-merchants.sql ny-postgres:/tmp/two-countries.sql >/dev/null
if docker exec ny-postgres psql -U postgres -d atlas_dev -v ON_ERROR_STOP=1 -q \
     -f /tmp/two-countries.sql; then
  ok "applied"
else
  bad "SQL failed — the transaction rolled back, nothing changed"
  exit 1
fi

# ── 3 ───────────────────────────────────────────────────────────────────────
say "3. tariffs (apply-tariff.sh also clears the two fare caches)"
bash ./apply-tariff.sh ./algeria-tariff.sql    || { bad "Algerian tariff failed"; exit 1; }
bash ./apply-tariff.sh ./mauritania-tariff.sql || { bad "Mauritanian tariff failed"; exit 1; }

# ── 4 ───────────────────────────────────────────────────────────────────────
say "4. caches that hold merchants, their config, or the registry"
n=0
for pat in '*CachedQueries:Merchant*' '*TransporterConfig*' '*MerchantServiceConfig*' \
           '*MerchantServiceUsageConfig*' '*Geometry*' '*egistry*' '*ubscriber*'; do
  for k in $(docker exec ny-redis redis-cli --scan --pattern "$pat"); do
    echo "     del $k"
    docker exec ny-redis redis-cli DEL "$k" >/dev/null
    n=$((n + 1))
  done
done
ok "$n key(s) cleared"

# ── 5 ───────────────────────────────────────────────────────────────────────
say "5. restart the services that read them"
docker restart ny-rider ny-driver ny-beckn-gateway >/dev/null
for port in 8013 8016 8015; do
  for i in $(seq 1 45); do
    code=$(curl -s -o /dev/null -w '%{http_code}' "http://127.0.0.1:$port/" || true)
    [ "$code" != "000" ] && break
    sleep 2
  done
  [ "$code" != "000" ] && ok ":$port answering ($code)" || bad ":$port not answering after 90 s"
done

# ── 6 ───────────────────────────────────────────────────────────────────────
say "6. the Algerian drivers, on the map in Algiers"
pga "UPDATE atlas_driver_offer_bpp.driver_information di
        SET active = true, on_ride = false
       FROM atlas_driver_offer_bpp.person p
      WHERE p.id = di.driver_id AND p.merchant_id = '$DZ'
        AND di.enabled AND NOT di.blocked
        AND EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.vehicle v WHERE v.driver_id = p.id)" >/dev/null
pga "UPDATE atlas_driver_offer_bpp.driver_location dl
        SET lat = $LAT + (random() - 0.5) * 0.01,
            lon = $LON + (random() - 0.5) * 0.01,
            coordinates_calculated_at = now(), updated_at = now()
       FROM atlas_driver_offer_bpp.person p
      WHERE p.id = dl.driver_id AND p.merchant_id = '$DZ'" >/dev/null
pga "INSERT INTO atlas_driver_offer_bpp.driver_location
        (driver_id, lat, lon, point, coordinates_calculated_at, created_at, updated_at)
     SELECT p.id, c.lat, c.lon, ST_SetSRID(ST_Point(c.lon, c.lat), 4326), now(), now(), now()
       FROM atlas_driver_offer_bpp.person p
      CROSS JOIN LATERAL (SELECT $LAT + (random() - 0.5) * 0.01 AS lat,
                                 $LON + (random() - 0.5) * 0.01 AS lon) c
      WHERE p.merchant_id = '$DZ'
        AND NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.driver_location dl
                         WHERE dl.driver_id = p.id)" >/dev/null
# `point` is what the pool measures distance on; lat/lon are only for display.
pga "UPDATE atlas_driver_offer_bpp.driver_location dl
        SET point = ST_SetSRID(ST_Point(dl.lon, dl.lat), 4326)
       FROM atlas_driver_offer_bpp.person p
      WHERE p.id = dl.driver_id AND p.merchant_id = '$DZ'" >/dev/null

# ── 7 ───────────────────────────────────────────────────────────────────────
say "7. what exists now"
pgt "SELECT short_id, origin_restriction FROM atlas_app.merchant"
pgt "SELECT id, short_id, subscriber_id, origin_restriction FROM atlas_driver_offer_bpp.merchant ORDER BY id"
pgt "SELECT subscriber_id, subscriber_url FROM atlas_registry.subscriber WHERE subscriber_url LIKE '%8016%' ORDER BY 1"
for t in fare_policy restricted_extra_fare merchant_service_config merchant_service_usage_config transporter_config operating_city; do
  printf '   %-32s MR=%s  DZ=%s\n' "$t" \
    "$(pga "SELECT count(*) FROM atlas_driver_offer_bpp.$t WHERE merchant_id = '$MR'")" \
    "$(pga "SELECT count(*) FROM atlas_driver_offer_bpp.$t WHERE merchant_id = '$DZ'")"
done
pgt "SELECT p.mobile_country_code AS cc, v.variant, di.enabled, di.active,
            round(ST_Distance(dl.point::geography, ST_SetSRID(ST_Point($LON, $LAT), 4326)::geography)) AS m_from_algiers
       FROM atlas_driver_offer_bpp.person p
       JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
       LEFT JOIN atlas_driver_offer_bpp.vehicle v ON v.driver_id = p.id
       LEFT JOIN atlas_driver_offer_bpp.driver_location dl ON dl.driver_id = p.id
      WHERE p.merchant_id = '$DZ'
      ORDER BY v.variant NULLS LAST"
say "done — now prove it with a search in each country"
