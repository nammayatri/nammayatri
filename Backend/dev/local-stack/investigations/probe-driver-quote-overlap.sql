-- Did a driver ever hold two quotes at once on two different searches?
-- (2026-11-25, follow-up of probe-driver-overlap.sql, for the drivers' waiting list)
--
--     docker cp probe-driver-quote-overlap.sql ny-postgres:/tmp/
--     docker exec ny-postgres psql -U postgres -d atlas_dev -f /tmp/probe-driver-quote-overlap.sql
--
-- Read-only, AGGREGATES ONLY: counts, no id, no place.
-- The server's rule (Domain/Action/UI/Driver.hs, thereAreActiveQuotes) refuses a
-- driver's offer while he has any other ACTIVE quote, whatever the search. If the
-- rule holds on the running binary there should be no pair of quotes of one driver,
-- on two searches, whose validity windows overlap while the first was still active.
-- CAVEAT: `status` is the CURRENT status, not the status at the time, so a pair
-- whose first quote was already inactive (the rider took another driver) would be
-- allowed and still counts below: the first number is an upper bound, and ZERO is
-- the strong result. A non-zero number is then read with the status columns.

\echo ''
\echo '=== scale: quotes, and drivers who offered ==='
SELECT count(*) AS quotes, count(DISTINCT driver_id) AS drivers
  FROM atlas_driver_offer_bpp.driver_quote;

\echo ''
\echo '=== pairs of quotes of one driver on two searches whose windows overlap ==='
SELECT count(*) AS overlapping_pairs
  FROM atlas_driver_offer_bpp.driver_quote a
  JOIN atlas_driver_offer_bpp.driver_quote b
    ON a.driver_id = b.driver_id
   AND a.search_request_id <> b.search_request_id
   AND a.created_at < b.created_at
   AND a.valid_till > b.created_at;

\echo ''
\echo '=== the same pairs, split by the CURRENT status of the first quote ==='
SELECT a.status AS first_quote_status, count(*) AS pairs
  FROM atlas_driver_offer_bpp.driver_quote a
  JOIN atlas_driver_offer_bpp.driver_quote b
    ON a.driver_id = b.driver_id
   AND a.search_request_id <> b.search_request_id
   AND a.created_at < b.created_at
   AND a.valid_till > b.created_at
 GROUP BY 1 ORDER BY 1;
