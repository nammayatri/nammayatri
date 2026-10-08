-- What a driver is actually offered, and what he does with it.
--
--     docker cp probe-driver-offers.sql ny-postgres:/tmp/
--     docker exec ny-postgres psql -U postgres -d atlas_dev -f /tmp/probe-driver-offers.sql
--
-- Read-only. Every number the D10/D11 screens are built on comes from here, so
-- it is a file rather than a shell one-liner: these figures have been quoted
-- into design documents twice and been wrong twice, both times because the
-- query was retyped slightly differently.
--
-- The one that keeps biting: **anchor the window on `created_at`, not on
-- `start_time`.** `start_time` is when the *rider* searched; the row is written
-- seconds later, and a whole batch-length later again for the second batch. On
-- `start_time` the same data spreads 12-40 s; on `created_at` it is exactly the
-- configured `singleBatchProcessTime`.

\echo ''
\echo '=== the answer window, anchored correctly (created_at) ==='
SELECT EXTRACT(EPOCH FROM (search_request_valid_till - created_at))::int AS window_s,
       count(*)
  FROM atlas_driver_offer_bpp.search_request_for_driver
 GROUP BY 1 ORDER BY 1;

\echo ''
\echo '=== the same data anchored wrongly (start_time) -- for comparison only ==='
SELECT EXTRACT(EPOCH FROM (search_request_valid_till - start_time))::int AS apparent_s,
       count(*)
  FROM atlas_driver_offer_bpp.search_request_for_driver
 GROUP BY 1 ORDER BY 1;

\echo ''
\echo '=== what the driver did with it ==='
SELECT coalesce(response, '(no answer)') AS response,
       count(*),
       round(100.0 * count(*) / sum(count(*)) OVER (), 1) AS pct
  FROM atlas_driver_offer_bpp.search_request_for_driver
 GROUP BY 1 ORDER BY 2 DESC;

\echo ''
\echo '=== the supplement ceiling actually sent, and the fare it came with ==='
SELECT driver_max_extra_fee AS max_extra,
       count(*),
       min(base_fare) AS base_min,
       max(base_fare) AS base_max
  FROM atlas_driver_offer_bpp.search_request_for_driver
 GROUP BY 1 ORDER BY 1;

\echo ''
\echo '=== the floor -- is it ever anything but zero? ==='
SELECT driver_min_extra_fee AS min_extra, count(*)
  FROM atlas_driver_offer_bpp.search_request_for_driver
 GROUP BY 1 ORDER BY 1;

\echo ''
\echo '=== has a supplement EVER been offered? ==='
-- The fleet simulator omits `offeredFare` entirely, so this path may never have
-- been exercised against this server. `fare_parameters.driver_selected_fare` is
-- where an accepted supplement lands.
SELECT coalesce(driver_selected_fare, 0) AS supplement, count(*)
  FROM atlas_driver_offer_bpp.fare_parameters
 GROUP BY 1 ORDER BY 1;

\echo ''
\echo '=== distance to the pickup, which sets what "3 min away" means ==='
SELECT round(avg(actual_distance_to_pickup))    AS avg_m,
       min(actual_distance_to_pickup)           AS min_m,
       max(actual_distance_to_pickup)           AS max_m,
       round(avg(duration_to_pickup))           AS avg_s
  FROM atlas_driver_offer_bpp.search_request_for_driver;
