-- What the app can actually say about a car, and who decides its category.
--
--     scp probe-vehicle-tags.sql ny:/tmp/
--     ssh ny 'docker cp /tmp/probe-vehicle-tags.sql ny-postgres:/tmp/ \
--             && docker exec ny-postgres psql -U postgres -d atlas_dev \
--                       -f /tmp/probe-vehicle-tags.sql'
--
-- Read-only. Written for the client's proposal of 2026-08-20: that the category
-- (economic / comfort / …) be assigned by the agency when a driver is taken on,
-- from the car's YEAR and BRAND, and that the passenger then choose an offer
-- showing brand, year, price and distance.
--
-- The question this answers is which parts of that are free and which need a
-- rebuild of the Haskell backend.

\echo ''
\echo '=== 1. every column the vehicle actually has ==='
\echo '    If there is no year here, the year cannot be stored or shown.'
SELECT column_name, data_type
  FROM information_schema.columns
 WHERE table_schema = 'atlas_driver_offer_bpp' AND table_name = 'vehicle'
 ORDER BY ordinal_position;

\echo ''
\echo '=== 2. the fleet as it stands ==='
SELECT variant, make, model, color, registration_no
  FROM atlas_driver_offer_bpp.vehicle
 ORDER BY variant, make NULLS LAST;

\echo ''
\echo '=== 3. how full the brand field is ==='
SELECT count(*) FILTER (WHERE make IS NOT NULL AND make <> '')  AS with_make,
       count(*) FILTER (WHERE model IS NOT NULL AND model <> '') AS with_model,
       count(*)                                                  AS vehicles
  FROM atlas_driver_offer_bpp.vehicle;

\echo ''
\echo '=== 4. the categories the backend knows, and nothing else ==='
\echo '    This is a compiled enum. A fifth name needs a rebuild.'
SELECT DISTINCT variant FROM atlas_driver_offer_bpp.vehicle ORDER BY 1;

\echo ''
\echo '=== 5. one price per category per merchant -- what screen 9 shows ==='
SELECT vehicle_variant, count(*) AS policies
  FROM atlas_driver_offer_bpp.fare_policy
 GROUP BY 1 ORDER BY 1;

\echo ''
\echo '=== 6. does the RIDER database keep any vehicle detail at all? ==='
\echo '    This decides whether the passenger can be shown a brand.'
SELECT table_name, column_name
  FROM information_schema.columns
 WHERE table_schema = 'atlas_app'
   AND (column_name ILIKE '%vehicle%' OR column_name ILIKE '%make%'
        OR column_name ILIKE '%model%')
 ORDER BY table_name, column_name;
