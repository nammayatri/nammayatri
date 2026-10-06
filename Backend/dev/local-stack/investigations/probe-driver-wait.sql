-- What happens to a driver's offer after he sends it.
--
--     scp probe-driver-wait.sql ny:/tmp/
--     ssh ny 'docker cp /tmp/probe-driver-wait.sql ny-postgres:/tmp/ \
--             && docker exec ny-postgres psql -U postgres -d atlas_dev \
--                       -f /tmp/probe-driver-wait.sql'
--
-- Read-only. The companion to probe-driver-offers.sql: that one measures the
-- window a driver has to *answer* in, this one measures everything after the
-- press -- how long the wait lasts, how it ends, and how often it ends in
-- nothing. Every figure on the D12 screen comes from here.
--
-- Note `docker exec` without -i, deliberately: with -i inside an ssh heredoc it
-- swallows the rest of the script from stdin. The SQL goes in as a file.
--
-- Three anchoring rules, all learned the hard way:
--
--   * The **offer** window is `driver_quote.valid_till - driver_quote.created_at`.
--     It is NOT the driver's answer window from probe-driver-offers.sql, and the
--     two have been confused. That one is `singleBatchProcessTime` and moved
--     from 10 s to 60 s on 20 August; this one is the quote's own life and is
--     set separately. They read the same today. That is a coincidence to
--     re-check after any config change, not a fact to build on -- section 1b
--     exists purely to keep checking it.
--
--   * A quote that "won" is one whose id appears as `booking.quote_id`. Do not
--     infer it from `driver_quote.status`: every quote that lost reads
--     `Inactive`, and so does every quote that merely expired.
--
--   * How a quote lost is in `search_request_for_driver.response`, not in the
--     quote. `Pulled` means another driver was chosen -- see section 5c.

\echo ''
\echo '=== 1. how long an offer lives: valid_till - created_at ==='
SELECT EXTRACT(EPOCH FROM (valid_till - created_at))::int AS offer_life_s,
       count(*)
  FROM atlas_driver_offer_bpp.driver_quote
 GROUP BY 1 ORDER BY 1;

\echo ''
\echo '=== 1b. and only the ones issued since the 60 s answer window went in ==='
\echo '    If this ever stops matching section 1, the two settings have come'
\echo '    apart and OFFER_LIFE_MS in the app is wrong.'
SELECT EXTRACT(EPOCH FROM (valid_till - created_at))::int AS offer_life_s,
       count(*)
  FROM atlas_driver_offer_bpp.driver_quote
 WHERE created_at >= TIMESTAMP '2026-08-20 00:00:00'
 GROUP BY 1 ORDER BY 1;

\echo ''
\echo '=== 2. how an offer ended: won, or nothing ==='
SELECT CASE WHEN b.quote_id IS NULL THEN 'came to nothing' ELSE 'became a booking' END AS outcome,
       count(*),
       round(100.0 * count(*) / sum(count(*)) OVER (), 1) AS pct
  FROM atlas_driver_offer_bpp.driver_quote q
  LEFT JOIN atlas_driver_offer_bpp.booking b ON b.quote_id = q.id
 GROUP BY 1 ORDER BY 2 DESC;

\echo ''
\echo '=== 2b. the same, but only offers whose window had time to close ==='
SELECT CASE WHEN b.quote_id IS NULL THEN 'came to nothing' ELSE 'became a booking' END AS outcome,
       count(*)
  FROM atlas_driver_offer_bpp.driver_quote q
  LEFT JOIN atlas_driver_offer_bpp.booking b ON b.quote_id = q.id
 WHERE q.valid_till < now()
 GROUP BY 1 ORDER BY 2 DESC;

\echo ''
\echo '=== 3. how long the passenger took to choose (booking - quote) ==='
SELECT EXTRACT(EPOCH FROM (b.created_at - q.created_at))::int AS decided_after_s,
       count(*)
  FROM atlas_driver_offer_bpp.driver_quote q
  JOIN atlas_driver_offer_bpp.booking b ON b.quote_id = q.id
 GROUP BY 1 ORDER BY 1;

\echo ''
\echo '=== 3b. the same as five numbers -- this decides the shape of the screen ==='
SELECT count(*) AS n,
       min(d)::int AS fastest_s,
       round(percentile_cont(0.5) WITHIN GROUP (ORDER BY d)::numeric, 1) AS median_s,
       round(avg(d)::numeric, 1) AS mean_s,
       round(percentile_cont(0.9) WITHIN GROUP (ORDER BY d)::numeric, 1) AS p90_s,
       max(d)::int AS slowest_s
  FROM (SELECT EXTRACT(EPOCH FROM (b.created_at - q.created_at)) AS d
          FROM atlas_driver_offer_bpp.driver_quote q
          JOIN atlas_driver_offer_bpp.booking b ON b.quote_id = q.id) s;

\echo ''
\echo '=== 3c. did any booking land AFTER the offer had expired? ==='
SELECT count(*) FILTER (WHERE b.created_at > q.valid_till) AS after_expiry,
       count(*)                                            AS total
  FROM atlas_driver_offer_bpp.driver_quote q
  JOIN atlas_driver_offer_bpp.booking b ON b.quote_id = q.id;

\echo ''
\echo '=== 4. how fast the ride appears once the passenger has chosen ==='
\echo '    This is the gap the app must wait out before calling an offer lost:'
\echo '    inside it the request is gone from nearbyRideRequest and no ride'
\echo '    exists yet, which is indistinguishable from losing.'
SELECT EXTRACT(EPOCH FROM (r.created_at - b.created_at))::int AS ride_after_booking_s,
       count(*)
  FROM atlas_driver_offer_bpp.booking b
  JOIN atlas_driver_offer_bpp.ride r ON r.booking_id = b.id
 GROUP BY 1 ORDER BY 1;

\echo ''
\echo '=== 4b. and from the driver''s press to the ride being visible ==='
SELECT EXTRACT(EPOCH FROM (r.created_at - q.created_at))::int AS ride_after_offer_s,
       count(*)
  FROM atlas_driver_offer_bpp.driver_quote q
  JOIN atlas_driver_offer_bpp.booking b ON b.quote_id = q.id
  JOIN atlas_driver_offer_bpp.ride r ON r.booking_id = b.id
 GROUP BY 1 ORDER BY 1;

\echo ''
\echo '=== 5. competition: offers per search request ==='
SELECT offers, count(*) AS searches
  FROM (SELECT search_request_id, count(*) AS offers
          FROM atlas_driver_offer_bpp.driver_quote
         GROUP BY 1) s
 GROUP BY 1 ORDER BY 1;

\echo ''
\echo '=== 5a. when two drivers competed: who won, at what price, from how far ==='
\echo '    NOTE both offers came from the simulator, which always takes the base'
\echo '    fare and sits where the script put it. The price gap is rounding'
\echo '    noise, not driver behaviour. Never show it to a driver as advice.'
SELECT CASE WHEN b.quote_id IS NULL THEN 'passed over' ELSE 'chosen' END AS outcome,
       count(*),
       round(avg(q.estimated_fare)::numeric, 0)     AS mean_fare,
       round(avg(q.distance_to_pickup)::numeric, 0) AS mean_m_to_pickup
  FROM atlas_driver_offer_bpp.driver_quote q
  LEFT JOIN atlas_driver_offer_bpp.booking b ON b.quote_id = q.id
 WHERE q.search_request_id IN (
         SELECT search_request_id FROM atlas_driver_offer_bpp.driver_quote
          GROUP BY 1 HAVING count(*) > 1)
 GROUP BY 1 ORDER BY 1;

\echo ''
\echo '=== 5c. HOW a lost offer was lost -- the server does record it ==='
\echo '    Pulled = another driver was chosen. Set by Confirm.hs, which also'
\echo '    sends those drivers an FCM CLEARED_FARE. No /ui/ route exposes it,'
\echo '    which is why the app has to infer the outcome instead.'
SELECT s.response,
       s.status AS request_status,
       q.status AS quote_status,
       count(*)
  FROM atlas_driver_offer_bpp.driver_quote q
  JOIN atlas_driver_offer_bpp.search_request_for_driver s
    ON s.id = q.search_request_for_driver_id
  LEFT JOIN atlas_driver_offer_bpp.booking b ON b.quote_id = q.id
 WHERE b.quote_id IS NULL
 GROUP BY 1,2,3 ORDER BY 4 DESC;

\echo ''
\echo '=== 5d. a request arriving while the driver already holds a live offer ==='
\echo '    The second card D12 stacks. The server does not prevent this.'
SELECT count(*) AS requests_during_a_live_offer,
       count(DISTINCT s.driver_id) AS drivers_affected
  FROM atlas_driver_offer_bpp.search_request_for_driver s
  JOIN atlas_driver_offer_bpp.driver_quote q
    ON q.driver_id = s.driver_id
   AND s.created_at > q.created_at
   AND s.created_at < q.valid_till;

\echo ''
\echo '=== 6. has a driver ever ended up with two rides at once? ==='
\echo '    Both must have real trip times -- a CANCELLED ride has none, and'
\echo '    filling that in with a guessed hour reports 32 phantom pairs.'
SELECT count(*) AS overlapping_pairs
  FROM atlas_driver_offer_bpp.ride a
  JOIN atlas_driver_offer_bpp.ride b
    ON b.driver_id = a.driver_id
   AND b.id <> a.id
   AND b.trip_start_time > a.trip_start_time
   AND b.trip_start_time < a.trip_end_time
 WHERE a.trip_start_time IS NOT NULL AND a.trip_end_time IS NOT NULL
   AND b.trip_start_time IS NOT NULL;

\echo ''
\echo '=== 6b. or two offers won inside one minute? ==='
SELECT count(*) AS pairs
  FROM atlas_driver_offer_bpp.driver_quote a
  JOIN atlas_driver_offer_bpp.booking ba ON ba.quote_id = a.id
  JOIN atlas_driver_offer_bpp.driver_quote b ON b.driver_id = a.driver_id AND b.id <> a.id
  JOIN atlas_driver_offer_bpp.booking bb ON bb.quote_id = b.id
 WHERE bb.created_at > ba.created_at
   AND bb.created_at < ba.created_at + interval '60 seconds';

\echo ''
\echo '=== 7. do drivers have an FCM device token at all? ==='
\echo '    Driver FCM config lives on transporter_config, NOT on merchant --'
\echo '    the rider side keeps it on merchant, and looking there finds nothing.'
SELECT count(*) FILTER (WHERE device_token IS NOT NULL) AS with_token,
       count(*)                                         AS drivers
  FROM atlas_driver_offer_bpp.person
 WHERE role = 'DRIVER';

\echo ''
\echo '=== 7b. and where the driver app sends push ==='
SELECT fcm_url, length(fcm_service_account) AS service_account_chars
  FROM atlas_driver_offer_bpp.transporter_config;
