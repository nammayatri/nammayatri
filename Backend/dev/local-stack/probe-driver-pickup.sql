-- Reaching the passenger: the code, the timings, and how it goes wrong.
--
--     scp probe-driver-pickup.sql ny:/tmp/
--     ssh ny 'docker cp /tmp/probe-driver-pickup.sql ny-postgres:/tmp/ \
--             && docker exec ny-postgres psql -U postgres -d atlas_dev \
--                       -f /tmp/probe-driver-pickup.sql'
--
-- Read-only. The third of the driver probes: offers (probe-driver-offers.sql),
-- the wait (probe-driver-wait.sql), and now the leg between winning a ride and
-- the passenger being aboard. Every figure on D13 comes from here.
--
-- The one that matters most is section 1. `POST /ui/driver/ride/{id}/start`
-- takes an OTP that `/openapi` does not mention at all — it publishes
-- `StartRideReq { point }`. A client built from that schema fails on every
-- single ride, in front of the passenger, with an error that does not name the
-- missing field. Section 1 is how we know the field is real and how wide it is.
--
-- Printing the codes is deliberate and safe: every ride below is finished, and
-- a spent OTP opens nothing. Do not copy this query into anything that runs
-- against live rides.

\echo ''
\echo '=== 1. the ride OTP: how many digits, and is it per ride ==='
SELECT length(otp) AS digits, count(*) FROM atlas_driver_offer_bpp.ride GROUP BY 1 ORDER BY 1;

\echo ''
\echo '=== 1b. distinct codes vs rides -- a constant here would be a security hole ==='
SELECT count(*) AS rides,
       count(DISTINCT otp) AS distinct_codes,
       min(otp) AS lowest,
       max(otp) AS highest
  FROM atlas_driver_offer_bpp.ride;

\echo ''
\echo '=== 1c. is it always numeric? (a letter would change the keyboard) ==='
SELECT count(*) FILTER (WHERE otp ~ '^[0-9]+$') AS all_digits,
       count(*)                                  AS total
  FROM atlas_driver_offer_bpp.ride;

\echo ''
\echo '=== 2. is the arrival actually being recorded? ==='
\echo '    driver_arrival_time is what POST .../arrived/pickup writes.'
SELECT count(*) FILTER (WHERE driver_arrival_time IS NOT NULL) AS arrival_recorded,
       count(*)                                                AS rides
  FROM atlas_driver_offer_bpp.ride;

\echo ''
\echo '=== 3. how long the driver spends on this screen ==='
\echo '    Ride created -> trip started. Anything over an hour is an abandoned'
\echo '    booking rather than a drive, and is excluded.'
SELECT count(*) AS n,
       min(d)::int AS fastest_s,
       round(percentile_cont(0.5) WITHIN GROUP (ORDER BY d)::numeric, 0) AS median_s,
       round(avg(d)::numeric, 0) AS mean_s,
       round(percentile_cont(0.9) WITHIN GROUP (ORDER BY d)::numeric, 0) AS p90_s,
       max(d)::int AS slowest_s
  FROM (SELECT EXTRACT(EPOCH FROM (trip_start_time - created_at)) AS d
          FROM atlas_driver_offer_bpp.ride
         WHERE trip_start_time IS NOT NULL
           AND trip_start_time - created_at < interval '1 hour') s;

\echo ''
\echo '=== 3b. and the ones excluded, so the exclusion is visible ==='
SELECT count(*) AS over_an_hour
  FROM atlas_driver_offer_bpp.ride
 WHERE trip_start_time IS NOT NULL
   AND trip_start_time - created_at >= interval '1 hour';

\echo ''
\echo '=== 4. how far the passenger actually is, on accepted requests ==='
SELECT count(*) AS n,
       round(avg(actual_distance_to_pickup)::numeric, 0) AS mean_m,
       max(actual_distance_to_pickup)::int               AS max_m,
       round(avg(duration_to_pickup)::numeric, 0)        AS mean_s_claimed
  FROM atlas_driver_offer_bpp.search_request_for_driver
 WHERE response = 'Accept';

\echo ''
\echo '=== 5. how rides end ==='
SELECT status, count(*) FROM atlas_driver_offer_bpp.ride GROUP BY 1 ORDER BY 2 DESC;

\echo ''
\echo '=== 5b. who cancelled, and what reason was recorded ==='
SELECT string_agg(column_name, ', ' ORDER BY ordinal_position)
  FROM information_schema.columns
 WHERE table_schema = 'atlas_driver_offer_bpp'
   AND table_name = 'booking_cancellation_reason';

\echo ''
\echo '=== 5c. the reasons themselves ==='
SELECT source,
       coalesce(reason_code, '(null)') AS reason_code,
       coalesce(left(additional_info, 30), '(null)') AS additional_info,
       count(*)
  FROM atlas_driver_offer_bpp.booking_cancellation_reason
 GROUP BY 1, 2, 3 ORDER BY 4 DESC;
