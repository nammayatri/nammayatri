-- How many requests a driver has open AT THE SAME TIME, and whether a driver ever
-- held two active quotes at once (2026-11-25, for the drivers' waiting list).
--
--     docker cp probe-driver-overlap.sql ny-postgres:/tmp/
--     docker exec ny-postgres psql -U postgres -d atlas_dev -f /tmp/probe-driver-overlap.sql
--
-- Read-only, and AGGREGATES ONLY: no driver id, no search id, no place is printed.
-- The data is the pilot's: it includes the simulated cars and the virtual
-- passengers, so « how many at once » is a floor for real traffic, not a forecast.

\echo ''
\echo '=== the columns of the two tables (names only) ==='
SELECT table_name, string_agg(column_name, ', ' ORDER BY ordinal_position)
  FROM information_schema.columns
 WHERE table_schema = 'atlas_driver_offer_bpp'
   AND table_name IN ('driver_quote', 'search_request_for_driver')
 GROUP BY 1;

\echo ''
\echo '=== requests sent to drivers, and the span of the data ==='
SELECT count(*) AS requests,
       count(DISTINCT driver_id) AS drivers,
       min(created_at)::date AS first_day,
       max(created_at)::date AS last_day
  FROM atlas_driver_offer_bpp.search_request_for_driver;

\echo ''
\echo '=== open requests at the moment a new one arrived (1 = he had none other) ==='
WITH r AS (
  SELECT driver_id, search_request_id AS sid, created_at, search_request_valid_till AS till
    FROM atlas_driver_offer_bpp.search_request_for_driver
),
c AS (
  SELECT a.driver_id, a.sid, count(DISTINCT b.sid) AS open_now
    FROM r a
    JOIN r b ON a.driver_id = b.driver_id
            AND b.created_at <= a.created_at
            AND b.till > a.created_at
   GROUP BY 1, 2
)
SELECT open_now, count(*) AS arrivals FROM c GROUP BY 1 ORDER BY 1;
