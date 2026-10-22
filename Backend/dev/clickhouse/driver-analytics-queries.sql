/*
  Analytics on atlas_kafka.driver_analytics_5m (sessionizer + driver-performance).

  Rules for every query on this table:
  - Always sum(). Rows are written as deltas (a pop-up that gets answered
    writes neglected -1 / accepted +1), so a single raw row can be negative
    until ClickHouse merges it. Totals are never negative.
  - bucket is a 5-minute UTC bucket. For India days/hours use
    toTimeZone(bucket, 'Asia/Kolkata'); IST is UTC+5:30, so 5-minute buckets
    line up with IST hours exactly.
  - Times are milliseconds (online_*_ms); money is Decimal; averages are
    sum / count, never an average of averages. Average response time is
    response_secs_sum / (popups_accepted + popups_rejected).
  - Idle / unavailable / no-state time are not columns: split online_ms by
    the state columns (sumIf(online_ms, onRide), sumIf(online_ms, NOT enabled
    OR blocked), sumIf(online_ms, state_known = 0)).
  - The camelCase columns (mode, variant, onRide, selectedServiceTiers, ...)
    are the driver's state and part of the row key: a driver-bucket has one
    row per distinct state, so GROUP BY them directly to split any measure by
    state, or leave them out for per-driver totals. state_known = 0 rows had
    no state (columns NULL / []); state_at is when the state took effect
    (epoch ms, 0 when unknown). Never sum() them.

  Parameters use ClickHouse query parameters, e.g. over HTTP:
    curl -G localhost:8123 --data-urlencode "param_day=2026-10-13" \
         --data-urlencode "query=$(cat ...)"
  or in clickhouse-client: SET param_day = '2026-10-13';
  Local macOS ClickHouse 23.3 needs
    SETTINGS compile_expressions = 0, compile_aggregate_expressions = 0
  for some queries (JIT is unavailable there); prod does not.
*/


/* 1. Driver scorecard for one IST day: time, pop-ups, rides, money, rating. */
SELECT
    driver_id,
    round(sum(online_ms) / 3.6e6, 2)                                   AS online_h,
    round(sum(online_on_ride_ms) / 3.6e6, 2)                           AS on_ride_h,
    round(sum(online_on_ride_ms) / nullIf(sum(online_ms), 0), 3)       AS utilisation,
    sum(popups_sent)                                                   AS popups,
    round(sum(popups_accepted) / nullIf(sum(popups_sent), 0), 3)       AS accept_rate,
    round(sum(popups_neglected) / nullIf(sum(popups_sent), 0), 3)      AS neglect_rate,
    round(sum(response_secs_sum) / nullIf(sum(popups_accepted) + sum(popups_rejected), 0), 1) AS avg_response_s,
    sum(total_bookings)                                                AS bookings,
    sum(finished_rides)                                                AS finished,
    sum(rides_cancelled_by_driver)                                     AS cancelled_by_driver,
    sum(gross_earnings)                                                AS gross,
    round(sum(gross_earnings) / nullIf(sum(online_ms) / 3.6e6, 0), 1)  AS gross_per_online_h,
    round(sum(rating_sum) / nullIf(sum(rides_with_rating), 0), 2)      AS avg_rating
FROM atlas_kafka.driver_analytics_5m
WHERE toDate(toTimeZone(bucket, 'Asia/Kolkata')) = {day:Date}
  AND driver_id != ''
GROUP BY driver_id
ORDER BY gross DESC
LIMIT 100;


/* 2. City by IST hour: supply (drivers online, hours), demand handled (pop-ups, rides). */
SELECT
    merchant_operating_city_id                                    AS city,
    toStartOfHour(toTimeZone(bucket, 'Asia/Kolkata'))             AS ist_hour,
    uniqExactIf(driver_id, online_ms > 0)                         AS drivers_online,
    round(sum(online_ms) / 3.6e6, 1)                              AS online_h,
    round(sum(idle_ms) / 3.6e6, 1)                                AS idle_h,
    sum(popups_sent)                                              AS popups,
    round(sum(popups_accepted) / nullIf(sum(popups_sent), 0), 3)  AS accept_rate,
    sum(total_bookings)                                           AS bookings,
    sum(finished_rides)                                           AS finished,
    round(sum(finished_rides) / nullIf(sum(online_ms) / 3.6e6, 0), 2) AS rides_per_online_h
FROM
(
    /* per driver first, so drivers_online counts drivers with any online time in the hour */
    SELECT merchant_operating_city_id, bucket, driver_id,
           sum(online_ms) AS online_ms,
           /* idle: online, enabled, not blocked, not on a ride */
           sumIf(t.online_ms, state_known = 1 AND enabled AND NOT blocked AND NOT onRide) AS idle_ms,
           sum(popups_sent) AS popups_sent, sum(popups_accepted) AS popups_accepted,
           sum(total_bookings) AS total_bookings, sum(finished_rides) AS finished_rides
    FROM atlas_kafka.driver_analytics_5m AS t
    WHERE toDate(toTimeZone(bucket, 'Asia/Kolkata')) = {day:Date}
    GROUP BY merchant_operating_city_id, bucket, driver_id
)
GROUP BY city, ist_hour
ORDER BY city, ist_hour;


/* 3. Live board: last 2 hours, every 5 minutes, per city. */
SELECT
    merchant_operating_city_id                                    AS city,
    toTimeZone(bucket, 'Asia/Kolkata')                            AS ist_bucket,
    uniqExactIf(driver_id, online_ms > 0)                         AS drivers_online,
    uniqExactIf(driver_id, online_on_ride_ms > 0)                 AS drivers_on_ride,
    sum(popups_sent)                                              AS popups,
    sum(popups_accepted)                                          AS accepted,
    sum(total_bookings)                                           AS bookings,
    sum(finished_rides)                                           AS finished
FROM
(
    SELECT merchant_operating_city_id, bucket, driver_id,
           sum(online_ms) AS online_ms, sum(online_on_ride_ms) AS online_on_ride_ms,
           sum(popups_sent) AS popups_sent, sum(popups_accepted) AS popups_accepted,
           sum(total_bookings) AS total_bookings, sum(finished_rides) AS finished_rides
    FROM atlas_kafka.driver_analytics_5m
    WHERE bucket >= now() - INTERVAL 2 HOUR
    GROUP BY merchant_operating_city_id, bucket, driver_id
)
GROUP BY city, ist_bucket
ORDER BY city, ist_bucket DESC;


/* 4. Where online time goes, per city and IST day (shares of online time),
      split by the state columns. */
SELECT
    merchant_operating_city_id                                             AS city,
    toDate(toTimeZone(bucket, 'Asia/Kolkata'))                             AS ist_day,
    round(sum(online_ms) / 3.6e6, 1)                                       AS online_h,
    round(sum(online_on_ride_ms) / nullIf(sum(online_ms), 0), 3)           AS on_ride,
    round(sumIf(online_ms, state_known = 1 AND enabled AND NOT blocked AND NOT onRide)
          / nullIf(sum(online_ms), 0), 3)                                  AS idle,
    round(sumIf(online_ms, state_known = 1 AND (NOT enabled OR blocked) AND NOT onRide)
          / nullIf(sum(online_ms), 0), 3)                                  AS unavailable,
    round(sumIf(online_ms, state_known = 0) / nullIf(sum(online_ms), 0), 3) AS unknown_state
FROM atlas_kafka.driver_analytics_5m
WHERE bucket >= now() - INTERVAL 7 DAY
GROUP BY city, ist_day
ORDER BY city, ist_day;


/* 5. Funnel per city and IST day: pop-up -> accepted -> booking -> finished. */
SELECT
    merchant_operating_city_id                                       AS city,
    toDate(toTimeZone(bucket, 'Asia/Kolkata'))                       AS ist_day,
    sum(popups_sent)                                                 AS popups,
    sum(popups_accepted)                                             AS accepted,
    sum(popups_rejected)                                             AS rejected,
    sum(popups_neglected)                                            AS neglected,
    sum(total_bookings)                                              AS bookings,
    sum(finished_rides)                                              AS finished,
    sum(cancelled_rides)                                             AS cancelled,
    round(sum(finished_rides) / nullIf(sum(total_bookings), 0), 3)   AS completion_rate,
    round(sum(rides_cancelled_by_driver) / nullIf(sum(total_bookings), 0), 3) AS driver_cancel_rate,
    round(sum(rides_cancelled_by_user) / nullIf(sum(total_bookings), 0), 3)   AS user_cancel_rate
FROM atlas_kafka.driver_analytics_5m
WHERE bucket >= now() - INTERVAL 7 DAY
GROUP BY city, ist_day
ORDER BY city, ist_day;


/* 6. Drivers who ignore pop-ups most (at least 20 pop-ups in the day). */
SELECT
    driver_id,
    sum(popups_sent)                                                AS popups,
    sum(popups_neglected)                                           AS neglected,
    round(sum(popups_neglected) / sum(popups_sent), 3)              AS neglect_rate,
    round(sum(online_ms) / 3.6e6, 2)                                AS online_h
FROM atlas_kafka.driver_analytics_5m
WHERE toDate(toTimeZone(bucket, 'Asia/Kolkata')) = {day:Date}
  AND driver_id != ''
GROUP BY driver_id
HAVING popups >= 20
ORDER BY neglect_rate DESC
LIMIT 50;


/* 7. Drivers cancelling most (at least 3 bookings in the day). */
SELECT
    driver_id,
    sum(total_bookings)                                                      AS bookings,
    sum(rides_cancelled_by_driver)                                           AS cancelled_by_driver,
    round(sum(rides_cancelled_by_driver) / sum(total_bookings), 3)           AS driver_cancel_rate,
    sum(finished_rides)                                                      AS finished
FROM atlas_kafka.driver_analytics_5m
WHERE toDate(toTimeZone(bucket, 'Asia/Kolkata')) = {day:Date}
  AND driver_id != ''
GROUP BY driver_id
HAVING bookings >= 3
ORDER BY driver_cancel_rate DESC, bookings DESC
LIMIT 50;


/* 8. Earnings per city and IST day: gross, tips, driver additions. */
SELECT
    merchant_operating_city_id                                          AS city,
    toDate(toTimeZone(bucket, 'Asia/Kolkata'))                          AS ist_day,
    sum(gross_earnings)                                                 AS gross,
    sum(customer_tip)                                                   AS tips,
    sum(driver_addition)                                                AS driver_additions,
    round(sum(gross_earnings) / nullIf(sum(finished_rides), 0), 1)      AS gross_per_ride
FROM atlas_kafka.driver_analytics_5m
WHERE bucket >= now() - INTERVAL 7 DAY
GROUP BY city, ist_day
ORDER BY city, ist_day;


/* 9. Auto-accept vs normal bookings per IST day: how auto-accepted rides end. */
SELECT
    toDate(toTimeZone(bucket, 'Asia/Kolkata'))                                        AS ist_day,
    sum(auto_accept_search_requests)                                                  AS auto_accepted_popups,
    sum(auto_accept_bookings)                                                         AS auto_bookings,
    round(sum(auto_accept_finished_rides) / nullIf(sum(auto_accept_bookings), 0), 3)  AS auto_completion,
    round(sum(auto_accept_rides_cancelled_by_driver) / nullIf(sum(auto_accept_bookings), 0), 3) AS auto_driver_cancel,
    sum(total_bookings) - sum(auto_accept_bookings)                                   AS normal_bookings,
    round((sum(finished_rides) - sum(auto_accept_finished_rides))
          / nullIf(sum(total_bookings) - sum(auto_accept_bookings), 0), 3)            AS normal_completion,
    round((sum(rides_cancelled_by_driver) - sum(auto_accept_rides_cancelled_by_driver))
          / nullIf(sum(total_bookings) - sum(auto_accept_bookings), 0), 3)            AS normal_driver_cancel
FROM atlas_kafka.driver_analytics_5m
WHERE bucket >= now() - INTERVAL 7 DAY
GROUP BY ist_day
ORDER BY ist_day;


/* 10. Response time by IST hour of day (are drivers slower at night?). */
SELECT
    toHour(toTimeZone(bucket, 'Asia/Kolkata'))                         AS ist_hour_of_day,
    sum(popups_accepted) + sum(popups_rejected)                        AS answered,
    round(sum(response_secs_sum) / nullIf(answered, 0), 1)             AS avg_response_s,
    round(sum(popups_accepted) / nullIf(sum(popups_sent), 0), 3)       AS accept_rate
FROM atlas_kafka.driver_analytics_5m
WHERE bucket >= now() - INTERVAL 7 DAY
GROUP BY ist_hour_of_day
ORDER BY ist_hour_of_day;


/* 11. Data-quality checks: every count here should be 0 (unknown-state share should be small). */
SELECT
    countIf(popups < 0 OR accepted < 0 OR neglected < 0 OR bookings < 0 OR finished < 0) AS negative_totals,
    countIf(accepted + rejected + neglected != popups)                                AS popup_split_mismatch,
    countIf(finished + inprogress + cancelled > bookings)                             AS ride_status_overflow,
    round(sum(unknown_ms) / nullIf(sum(online_ms), 0), 4)                             AS unknown_state_share
FROM
(
    SELECT merchant_operating_city_id, bucket, driver_id,
           sum(popups_sent) AS popups, sum(popups_accepted) AS accepted,
           sum(popups_rejected) AS rejected, sum(popups_neglected) AS neglected,
           sum(total_bookings) AS bookings, sum(finished_rides) AS finished,
           sum(inprogress_rides) AS inprogress, sum(cancelled_rides) AS cancelled,
           sum(online_ms) AS online_ms, sumIf(t.online_ms, state_known = 0) AS unknown_ms
    FROM atlas_kafka.driver_analytics_5m AS t
    WHERE bucket >= now() - INTERVAL 1 DAY
    GROUP BY merchant_operating_city_id, bucket, driver_id
);


/* 12. Parity with the batch table for one IST day (prod, once both exist):
       per-metric totals from the stream vs cosmos.performance_analytics.
       Expected gaps: online time (stream counts real ping gaps, the batch
       counts 5-min snapshots), rides bucketed by ride time vs search time,
       and the batch's known bug (tips 00:00-05:30 IST). */
SELECT
    'stream' AS source,
    round(sum(online_ms) / 3.6e6) AS online_h, sum(popups_sent) AS popups,
    sum(popups_accepted) AS accepted, sum(total_bookings) AS bookings,
    sum(finished_rides) AS finished, sum(cancelled_rides) AS cancelled,
    toFloat64(sum(gross_earnings)) AS gross
FROM atlas_kafka.driver_analytics_5m
WHERE toDate(toTimeZone(bucket, 'Asia/Kolkata')) = {day:Date}
UNION ALL
SELECT
    'batch', round(sum(online_time)), sum(popups_sent), sum(popups_accepted),
    sum(total_bookings), sum(finished_rides), sum(cancelled_rides), sum(gross_earnings)
FROM cosmos.performance_analytics FINAL
WHERE toDate(local_time) = {day:Date};


/* 13. Online hours by the driver's mode and vehicle variant: each piece of
       online time sits in the row of the state the driver was in during it,
       so this groups the raw rows directly. One IST day. */
SELECT
    if(state_known = 0, '(no state)', ifNull(mode, '(null)'))       AS driver_mode,
    if(state_known = 0, '(no state)', ifNull(variant, '(null)'))    AS vehicle_variant,
    uniqExactIf(driver_id, online_ms > 0)                           AS drivers,
    round(sum(online_ms) / 3.6e6, 1)                                AS online_h,
    round(sum(online_on_ride_ms) / nullIf(sum(online_ms), 0), 3)    AS on_ride_share
FROM atlas_kafka.driver_analytics_5m
WHERE toDate(toTimeZone(bucket, 'Asia/Kolkata')) = {day:Date}
GROUP BY state_known, mode, variant
HAVING online_h > 0
ORDER BY online_h DESC;


/* 14. Pop-ups sent while the driver's state said ONLINE, in buckets where LTS
       had not a single ping from the driver: the app thinks the driver is
       online but no location reached LTS. Last 24 hours. */
SELECT
    merchant_operating_city_id                                    AS city,
    driver_id,
    count()                                                       AS buckets_without_pings,
    sum(online_state_popups)                                      AS popups_while_online
FROM
(
    SELECT merchant_operating_city_id, bucket, driver_id,
           sum(pings) AS pings,
           sumIf(popups_sent, state_known = 1 AND mode = 'ONLINE') AS online_state_popups
    FROM atlas_kafka.driver_analytics_5m
    WHERE bucket >= now() - INTERVAL 1 DAY
    GROUP BY merchant_operating_city_id, bucket, driver_id
)
WHERE pings = 0 AND online_state_popups > 0
GROUP BY city, driver_id
ORDER BY popups_while_online DESC
LIMIT 100;


/* 15. Time split per state for one driver and IST day: every distinct state
       the driver was in, when it first took effect, its online time, pings
       and pop-ups. */
SELECT
    if(state_known = 0, NULL, fromUnixTimestamp64Milli(toInt64(min(state_at)), 'UTC')) AS state_since,
    state_known, mode, onRide, active, enabled, blocked, subscribed, variant,
    selectedServiceTiers,
    round(sum(online_ms) / 6e4, 1)                                AS online_min,
    round(sum(online_on_ride_ms) / 6e4, 1)                        AS on_ride_min,
    sum(pings)                                                    AS pings,
    sum(popups_sent)                                              AS popups,
    sum(popups_accepted)                                          AS accepted,
    min(toTimeZone(bucket, 'Asia/Kolkata'))                       AS first_bucket_ist,
    max(toTimeZone(bucket, 'Asia/Kolkata'))                       AS last_bucket_ist
FROM atlas_kafka.driver_analytics_5m
WHERE toDate(toTimeZone(bucket, 'Asia/Kolkata')) = {day:Date}
  AND driver_id = {driver:String}
GROUP BY state_known, mode, onRide, active, enabled, blocked, subscribed, variant, selectedServiceTiers
ORDER BY state_known, first_bucket_ist;
