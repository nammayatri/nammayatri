/*
  Comments in this file must be block comments: the dev setup joins every line
  into one before piping it to clickhouse-client, so a line comment would
  swallow the rest of the file.
*/

/*
  driver_analytics_5m: one row per city x 5-minute UTC bucket x driver x
  distinct driver state: the driver was in that state during that bucket.
  Written by SESSIONIZER (kafka-consumers) through the driver-analytics-5m
  topic and driver_analytics_5m_queue (below):
    presence  from LTS pings: every ping whose mode is not OFFLINE marks the
              row of its bucket and state online.
    events    from driver-analytics-events (the driver app publishes one per
              pop-up sent / answered / pulled, ride assigned / started /
              completed / cancelled, rating): each adds its counts to the row
              of its own time and the state in force then.
  SummingMergeTree sums rows with the same key, so pods, late data and
  re-flushes add up; SESSIONIZER skips records it already flushed. Counts are
  at the event's time: a ride's booking, start, end and cancellation can fall
  in different buckets. popups_sent counts every pop-up sent, popups_pulled
  those pulled later; the views report sent minus pulled, and neglected as
  sent - pulled - accepted - rejected (exact over a day, approximate per
  bucket, as an answer can land in the next bucket). A changed rating adds
  only its difference to rating_sum.

  Online time. Not stored: a driver's online time is the number of distinct
  online buckets times 5 minutes,
    uniqExactIf(bucket, online = 1) * 5   (minutes)
  A bucket with a state change has a row per state, so count buckets, never
  rows. A session's first and last bucket count in full. last_ping is the
  bucket's newest ping (time, lat, lon); pings is the number of pings.

  State. state_id is a hash of the row's state (Processor/StateColumns.hs
  stateIdOf; 0 = unknown) and is the only state column in the key. The
  camelCase columns are that state's DriverPoolData values (column name = the
  key in the DRIVER_POOL_STATE writer's hash), identical for one state_id.
  SESSIONIZER marks a ping's row by the state at its st, and an event's row by
  the state at the event's time.
  state_known = 0 (all state columns NULL / []) when the writer had no state
  for that time. state_at is when the row's state took effect (the writer's
  history entry time), 1970-01-01 00:00:00 when state_known = 0.
  registrationNo is the vehicle number linked to the driver at that time.
  deviceToken and the row-key fields are not state columns.
  Always sum() the measures, grouping by the state columns you need (or by
  none of them for per-driver totals).

  Adding a state field later: add it to Processor/StateColumns.hs, then
    ALTER TABLE atlas_kafka.driver_analytics_5m
      ADD COLUMN newField SimpleAggregateFunction(any, Nullable(String));
  and the column to driver_analytics_5m_queue and driver_analytics_5m_mv. Old
  rows read it as NULL. Then add argMax lines for it to both views (CREATE OR
  REPLACE VIEW). Removing a state field: drop it from StateColumns.hs only;
  new rows write its default (NULL / []).
*/
CREATE TABLE IF NOT EXISTS atlas_kafka.driver_analytics_5m (
    `bucket`                                DateTime('UTC'),
    `merchant_operating_city_id`            String,
    `driver_id`                             String,
    `state_id`                              UInt64,
    `online`                                SimpleAggregateFunction(max, UInt8),
    `pings`                                 UInt64,
    `last_ping`                             SimpleAggregateFunction(max, Tuple(at DateTime64(3, 'UTC'), lat Float64, lon Float64)),
    `popups_sent`                           Int64,
    `popups_accepted`                       Int64,
    `popups_rejected`                       Int64,
    `popups_pulled`                         Int64,
    `response_secs_sum`                     Int64,
    `auto_accept_search_requests`           Int64,
    `total_bookings`                        Int64,
    `finished_rides`                        Int64,
    `rides_started`                         Int64,
    `cancelled_rides`                       Int64,
    `rides_cancelled_by_driver`             Int64,
    `rides_cancelled_by_user`               Int64,
    `auto_accept_bookings`                  Int64,
    `auto_accept_finished_rides`            Int64,
    `auto_accept_cancelled_rides`           Int64,
    `auto_accept_rides_cancelled_by_driver` Int64,
    `auto_accept_rides_cancelled_by_user`   Int64,
    `rides_with_rating`                     Int64,
    `rating_sum`                            Int64,
    `driver_addition`                       Decimal(18, 4),
    `customer_tip`                          Decimal(18, 4),
    `gross_earnings`                        Decimal(18, 4),
    `state_known`                           SimpleAggregateFunction(max, UInt8),
    `active`                                SimpleAggregateFunction(any, Nullable(Bool)),
    `onRide`                                SimpleAggregateFunction(any, Nullable(Bool)),
    `enabled`                               SimpleAggregateFunction(any, Nullable(Bool)),
    `blocked`                               SimpleAggregateFunction(any, Nullable(Bool)),
    `subscribed`                            SimpleAggregateFunction(any, Nullable(Bool)),
    `canSwitchToRental`                     SimpleAggregateFunction(any, Nullable(Bool)),
    `canSwitchToInterCity`                  SimpleAggregateFunction(any, Nullable(Bool)),
    `canSwitchToIntraCity`                  SimpleAggregateFunction(any, Nullable(Bool)),
    `forwardBatchingEnabled`                SimpleAggregateFunction(any, Nullable(Bool)),
    `isSpecialLocWarrior`                   SimpleAggregateFunction(any, Nullable(Bool)),
    `isPetModeEnabled`                      SimpleAggregateFunction(any, Nullable(Bool)),
    `chargesEnabled`                        SimpleAggregateFunction(any, Nullable(Bool)),
    `hasAdvanceBooking`                     SimpleAggregateFunction(any, Nullable(Bool)),
    `isDisabledReasonFlag`                  SimpleAggregateFunction(any, Nullable(Bool)),
    `hasRideStarted`                        SimpleAggregateFunction(any, Nullable(Bool)),
    `airConditioned`                        SimpleAggregateFunction(any, Nullable(Bool)),
    `mode`                                  SimpleAggregateFunction(any, LowCardinality(Nullable(String))),
    `onRideTripCategory`                    SimpleAggregateFunction(any, LowCardinality(Nullable(String))),
    `goHomeStatus`                          SimpleAggregateFunction(any, LowCardinality(Nullable(String))),
    `variant`                               SimpleAggregateFunction(any, LowCardinality(Nullable(String))),
    `rideBillingModel`                      SimpleAggregateFunction(any, LowCardinality(Nullable(String))),
    `enableForAirport`                      SimpleAggregateFunction(any, LowCardinality(Nullable(String))),
    `acUsageRestrictionType`                SimpleAggregateFunction(any, LowCardinality(Nullable(String))),
    `fleetOwnerId`                          SimpleAggregateFunction(any, LowCardinality(Nullable(String))),
    `cloudType`                             SimpleAggregateFunction(any, LowCardinality(Nullable(String))),
    `registrationNo`                        SimpleAggregateFunction(any, Nullable(String)),
    `latestScheduledPickup`                 SimpleAggregateFunction(any, LowCardinality(Nullable(String))),
    `selectedServiceTiers`                  SimpleAggregateFunction(any, Array(String)),
    `selectedAutoAcceptTiers`               SimpleAggregateFunction(any, Array(String)),
    `softBlockStiers`                       SimpleAggregateFunction(any, Array(String)),
    `vehicleTags`                           SimpleAggregateFunction(any, Array(String)),
    `driverTag`                             SimpleAggregateFunction(any, Array(String)),
    `tripDistanceMinThreshold`              SimpleAggregateFunction(any, Nullable(Int64)),
    `tripDistanceMaxThreshold`              SimpleAggregateFunction(any, Nullable(Int64)),
    `maxPickupRadius`                       SimpleAggregateFunction(any, Nullable(Int64)),
    `lastUpdatedAt`                         SimpleAggregateFunction(any, Nullable(DateTime64(3, 'UTC'))),
    `registrationNoChangedAt`               SimpleAggregateFunction(any, Nullable(DateTime64(3, 'UTC'))),
    `airConditionScore`                     SimpleAggregateFunction(any, Nullable(Float64)),
    `vehicleRating`                         SimpleAggregateFunction(any, Nullable(Float64)),
    `latestScheduledBooking`                SimpleAggregateFunction(any, Nullable(DateTime64(3, 'UTC'))),
    `tollRouteBlockedTill`                  SimpleAggregateFunction(any, Nullable(DateTime64(3, 'UTC'))),
    `state_at`                              SimpleAggregateFunction(max, DateTime64(3, 'UTC'))
) ENGINE = SummingMergeTree()
PARTITION BY toYYYYMMDD(bucket)
PRIMARY KEY (merchant_operating_city_id, bucket, driver_id)
ORDER BY (merchant_operating_city_id, bucket, driver_id, state_id);

/*
  driver_analytics_5m_queue: SESSIONIZER produces its rows to the
  driver-analytics-5m topic (JSONEachRow, several rows per message) and this
  Kafka engine table reads them; driver_analytics_5m_mv inserts them into
  driver_analytics_5m. Only the columns SESSIONIZER writes; the others take
  their defaults. Times arrive as text and are parsed in the view.
*/
CREATE TABLE IF NOT EXISTS atlas_kafka.driver_analytics_5m_queue (
    `bucket`                       DateTime('UTC'),
    `merchant_operating_city_id`   String,
    `driver_id`                    String,
    `state_id`                     UInt64,
    `online`                       UInt8,
    `pings`                        UInt64,
    `last_ping_at`                 Nullable(String),
    `lat`                          Nullable(Float64),
    `lon`                          Nullable(Float64),
    `popups_sent`                  Int64,
    `popups_accepted`              Int64,
    `popups_rejected`              Int64,
    `popups_pulled`                Int64,
    `response_secs_sum`            Int64,
    `auto_accept_search_requests`  Int64,
    `total_bookings`               Int64,
    `rides_started`                Int64,
    `finished_rides`               Int64,
    `cancelled_rides`              Int64,
    `rides_cancelled_by_driver`    Int64,
    `rides_cancelled_by_user`      Int64,
    `auto_accept_bookings`         Int64,
    `auto_accept_finished_rides`   Int64,
    `auto_accept_cancelled_rides`  Int64,
    `auto_accept_rides_cancelled_by_driver` Int64,
    `auto_accept_rides_cancelled_by_user` Int64,
    `rides_with_rating`            Int64,
    `rating_sum`                   Int64,
    `driver_addition`              Decimal(18, 4),
    `customer_tip`                 Decimal(18, 4),
    `gross_earnings`               Decimal(18, 4),
    `state_known`                  UInt8,
    `active`                       Nullable(Bool),
    `onRide`                       Nullable(Bool),
    `enabled`                      Nullable(Bool),
    `blocked`                      Nullable(Bool),
    `subscribed`                   Nullable(Bool),
    `canSwitchToRental`            Nullable(Bool),
    `canSwitchToInterCity`         Nullable(Bool),
    `canSwitchToIntraCity`         Nullable(Bool),
    `forwardBatchingEnabled`       Nullable(Bool),
    `isSpecialLocWarrior`          Nullable(Bool),
    `isPetModeEnabled`             Nullable(Bool),
    `chargesEnabled`               Nullable(Bool),
    `hasAdvanceBooking`            Nullable(Bool),
    `isDisabledReasonFlag`         Nullable(Bool),
    `hasRideStarted`               Nullable(Bool),
    `airConditioned`               Nullable(Bool),
    `mode`                         Nullable(String),
    `onRideTripCategory`           Nullable(String),
    `goHomeStatus`                 Nullable(String),
    `variant`                      Nullable(String),
    `rideBillingModel`             Nullable(String),
    `enableForAirport`             Nullable(String),
    `acUsageRestrictionType`       Nullable(String),
    `fleetOwnerId`                 Nullable(String),
    `cloudType`                    Nullable(String),
    `registrationNo`               Nullable(String),
    `latestScheduledPickup`        Nullable(String),
    `selectedServiceTiers`         Array(String),
    `selectedAutoAcceptTiers`      Array(String),
    `softBlockStiers`              Array(String),
    `vehicleTags`                  Array(String),
    `driverTag`                    Array(String),
    `tripDistanceMinThreshold`     Nullable(Int64),
    `tripDistanceMaxThreshold`     Nullable(Int64),
    `maxPickupRadius`              Nullable(Int64),
    `lastUpdatedAt`                Nullable(String),
    `registrationNoChangedAt`      Nullable(String),
    `airConditionScore`            Nullable(Float64),
    `vehicleRating`                Nullable(Float64),
    `latestScheduledBooking`       Nullable(String),
    `tollRouteBlockedTill`         Nullable(String),
    `state_at`                     Nullable(String)
) ENGINE = Kafka
SETTINGS
    kafka_broker_list     = 'localhost:29092',
    kafka_topic_list      = 'driver-analytics-5m',
    kafka_group_name      = 'ch-atlas_kafka-driver_analytics_5m',
    kafka_format          = 'JSONEachRow',
    kafka_num_consumers   = 1,
    kafka_skip_broken_messages = 100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_kafka.driver_analytics_5m_mv
TO atlas_kafka.driver_analytics_5m AS
SELECT
    bucket,
    merchant_operating_city_id,
    driver_id,
    state_id,
    online,
    pings,
    tuple(ifNull(parseDateTime64BestEffortOrNull(last_ping_at, 3, 'UTC'), toDateTime64(0, 3, 'UTC')), ifNull(lat, 0), ifNull(lon, 0)) AS last_ping,
    popups_sent,
    popups_accepted,
    popups_rejected,
    popups_pulled,
    response_secs_sum,
    auto_accept_search_requests,
    total_bookings,
    rides_started,
    finished_rides,
    cancelled_rides,
    rides_cancelled_by_driver,
    rides_cancelled_by_user,
    auto_accept_bookings,
    auto_accept_finished_rides,
    auto_accept_cancelled_rides,
    auto_accept_rides_cancelled_by_driver,
    auto_accept_rides_cancelled_by_user,
    rides_with_rating,
    rating_sum,
    driver_addition,
    customer_tip,
    gross_earnings,
    state_known,
    active,
    onRide,
    enabled,
    blocked,
    subscribed,
    canSwitchToRental,
    canSwitchToInterCity,
    canSwitchToIntraCity,
    forwardBatchingEnabled,
    isSpecialLocWarrior,
    isPetModeEnabled,
    chargesEnabled,
    hasAdvanceBooking,
    isDisabledReasonFlag,
    hasRideStarted,
    airConditioned,
    mode,
    onRideTripCategory,
    goHomeStatus,
    variant,
    rideBillingModel,
    enableForAirport,
    acUsageRestrictionType,
    fleetOwnerId,
    cloudType,
    registrationNo,
    latestScheduledPickup,
    selectedServiceTiers,
    selectedAutoAcceptTiers,
    softBlockStiers,
    vehicleTags,
    driverTag,
    tripDistanceMinThreshold,
    tripDistanceMaxThreshold,
    maxPickupRadius,
    parseDateTime64BestEffortOrNull(lastUpdatedAt, 3, 'UTC') AS lastUpdatedAt,
    parseDateTime64BestEffortOrNull(registrationNoChangedAt, 3, 'UTC') AS registrationNoChangedAt,
    airConditionScore,
    vehicleRating,
    parseDateTime64BestEffortOrNull(latestScheduledBooking, 3, 'UTC') AS latestScheduledBooking,
    parseDateTime64BestEffortOrNull(tollRouteBlockedTill, 3, 'UTC') AS tollRouteBlockedTill,
    ifNull(parseDateTime64BestEffortOrNull(state_at, 3, 'UTC'), toDateTime64(0, 3, 'UTC')) AS state_at
FROM atlas_kafka.driver_analytics_5m_queue;

/*
  driver_performance_5m: driver_analytics_5m with performance_analytics column
  names, per city x 5-minute bucket x driver. Sums in the inner query because
  unmerged parts can still hold several rows per key. Times are hours,
  averages are 0 when nothing was counted. avg_response_time divides by
  accepts + rejects: the ~0.4% of answers with no valid rendered_at /
  responded_at add 0 seconds, so it reads that much low.
  local_time is the bucket as a naive IST wall-clock DateTime, like PA's.
  A bucket is a row when the driver was online or had a pop-up or a ride in
  it, like PA's rows.
  The state columns are the driver's latest state in the bucket: each is
  argMax over the bucket's state rows by state_at (wrapped in a tuple, since
  argMax skips NULL values), state_at its time (NULL when no row of the bucket
  has a known state), states_in_bucket the number of distinct state rows
  (the unknown state counts as one).
*/
CREATE VIEW IF NOT EXISTS atlas_kafka.driver_performance_5m AS
SELECT
    bucket,
    toDateTime(toString(toTimeZone(bucket, 'Asia/Kolkata')), 'UTC') AS local_time,
    merchant_operating_city_id,
    driver_id,
    popups_sent_all - popups_pulled AS popups_sent,
    popups_accepted,
    popups_rejected,
    popups_sent_all - popups_pulled - popups_accepted - popups_rejected AS popups_neglected,
    popups_pulled,
    online_ms / 3600000 AS online_time,
    online_on_ride_ms / 3600000 AS driver_on_ride_time,
    if(popups_accepted + popups_rejected = 0, 0, response_secs_sum / (popups_accepted + popups_rejected)) AS avg_response_time,
    total_bookings,
    finished_rides,
    rides_started,
    cancelled_rides,
    rides_cancelled_by_driver,
    rides_cancelled_by_user,
    if(rides_with_rating = 0, 0, rating_sum / rides_with_rating) AS avg_rating,
    rides_with_rating,
    driver_addition,
    customer_tip,
    gross_earnings,
    auto_accept_search_requests,
    auto_accept_bookings,
    auto_accept_finished_rides,
    auto_accept_cancelled_rides,
    auto_accept_rides_cancelled_by_driver,
    auto_accept_rides_cancelled_by_user,
    active_s.1 AS active,
    onRide_s.1 AS onRide,
    enabled_s.1 AS enabled,
    blocked_s.1 AS blocked,
    subscribed_s.1 AS subscribed,
    canSwitchToRental_s.1 AS canSwitchToRental,
    canSwitchToInterCity_s.1 AS canSwitchToInterCity,
    canSwitchToIntraCity_s.1 AS canSwitchToIntraCity,
    forwardBatchingEnabled_s.1 AS forwardBatchingEnabled,
    isSpecialLocWarrior_s.1 AS isSpecialLocWarrior,
    isPetModeEnabled_s.1 AS isPetModeEnabled,
    chargesEnabled_s.1 AS chargesEnabled,
    hasAdvanceBooking_s.1 AS hasAdvanceBooking,
    isDisabledReasonFlag_s.1 AS isDisabledReasonFlag,
    hasRideStarted_s.1 AS hasRideStarted,
    airConditioned_s.1 AS airConditioned,
    mode_s.1 AS mode,
    onRideTripCategory_s.1 AS onRideTripCategory,
    goHomeStatus_s.1 AS goHomeStatus,
    variant_s.1 AS variant,
    rideBillingModel_s.1 AS rideBillingModel,
    enableForAirport_s.1 AS enableForAirport,
    acUsageRestrictionType_s.1 AS acUsageRestrictionType,
    fleetOwnerId_s.1 AS fleetOwnerId,
    cloudType_s.1 AS cloudType,
    registrationNo_s.1 AS registrationNo,
    latestScheduledPickup_s.1 AS latestScheduledPickup,
    selectedServiceTiers_s.1 AS selectedServiceTiers,
    selectedAutoAcceptTiers_s.1 AS selectedAutoAcceptTiers,
    softBlockStiers_s.1 AS softBlockStiers,
    vehicleTags_s.1 AS vehicleTags,
    driverTag_s.1 AS driverTag,
    tripDistanceMinThreshold_s.1 AS tripDistanceMinThreshold,
    tripDistanceMaxThreshold_s.1 AS tripDistanceMaxThreshold,
    maxPickupRadius_s.1 AS maxPickupRadius,
    lastUpdatedAt_s.1 AS lastUpdatedAt,
    registrationNoChangedAt_s.1 AS registrationNoChangedAt,
    airConditionScore_s.1 AS airConditionScore,
    vehicleRating_s.1 AS vehicleRating,
    latestScheduledBooking_s.1 AS latestScheduledBooking,
    tollRouteBlockedTill_s.1 AS tollRouteBlockedTill,
    if(toUnixTimestamp64Milli(state_at_max) = 0, NULL, toDateTime64(state_at_max, 3, 'UTC')) AS state_at,
    states_in_bucket
FROM
(
    SELECT
        bucket,
        merchant_operating_city_id,
        driver_id,
        max(online) * 300000 AS online_ms,
        maxIf(online, onRide = true) * 300000 AS online_on_ride_ms,
        sum(popups_sent) AS popups_sent_all,
        sum(popups_accepted) AS popups_accepted,
        sum(popups_rejected) AS popups_rejected,
        sum(popups_pulled) AS popups_pulled,
        sum(response_secs_sum) AS response_secs_sum,
        sum(auto_accept_search_requests) AS auto_accept_search_requests,
        sum(total_bookings) AS total_bookings,
        sum(finished_rides) AS finished_rides,
        sum(rides_started) AS rides_started,
        sum(cancelled_rides) AS cancelled_rides,
        sum(rides_cancelled_by_driver) AS rides_cancelled_by_driver,
        sum(rides_cancelled_by_user) AS rides_cancelled_by_user,
        sum(auto_accept_bookings) AS auto_accept_bookings,
        sum(auto_accept_finished_rides) AS auto_accept_finished_rides,
        sum(auto_accept_cancelled_rides) AS auto_accept_cancelled_rides,
        sum(auto_accept_rides_cancelled_by_driver) AS auto_accept_rides_cancelled_by_driver,
        sum(auto_accept_rides_cancelled_by_user) AS auto_accept_rides_cancelled_by_user,
        sum(rides_with_rating) AS rides_with_rating,
        sum(rating_sum) AS rating_sum,
        sum(driver_addition) AS driver_addition,
        sum(customer_tip) AS customer_tip,
        sum(gross_earnings) AS gross_earnings,
        argMax(tuple(active), state_at) AS active_s,
        argMax(tuple(onRide), state_at) AS onRide_s,
        argMax(tuple(enabled), state_at) AS enabled_s,
        argMax(tuple(blocked), state_at) AS blocked_s,
        argMax(tuple(subscribed), state_at) AS subscribed_s,
        argMax(tuple(canSwitchToRental), state_at) AS canSwitchToRental_s,
        argMax(tuple(canSwitchToInterCity), state_at) AS canSwitchToInterCity_s,
        argMax(tuple(canSwitchToIntraCity), state_at) AS canSwitchToIntraCity_s,
        argMax(tuple(forwardBatchingEnabled), state_at) AS forwardBatchingEnabled_s,
        argMax(tuple(isSpecialLocWarrior), state_at) AS isSpecialLocWarrior_s,
        argMax(tuple(isPetModeEnabled), state_at) AS isPetModeEnabled_s,
        argMax(tuple(chargesEnabled), state_at) AS chargesEnabled_s,
        argMax(tuple(hasAdvanceBooking), state_at) AS hasAdvanceBooking_s,
        argMax(tuple(isDisabledReasonFlag), state_at) AS isDisabledReasonFlag_s,
        argMax(tuple(hasRideStarted), state_at) AS hasRideStarted_s,
        argMax(tuple(airConditioned), state_at) AS airConditioned_s,
        argMax(tuple(mode), state_at) AS mode_s,
        argMax(tuple(onRideTripCategory), state_at) AS onRideTripCategory_s,
        argMax(tuple(goHomeStatus), state_at) AS goHomeStatus_s,
        argMax(tuple(variant), state_at) AS variant_s,
        argMax(tuple(rideBillingModel), state_at) AS rideBillingModel_s,
        argMax(tuple(enableForAirport), state_at) AS enableForAirport_s,
        argMax(tuple(acUsageRestrictionType), state_at) AS acUsageRestrictionType_s,
        argMax(tuple(fleetOwnerId), state_at) AS fleetOwnerId_s,
        argMax(tuple(cloudType), state_at) AS cloudType_s,
        argMax(tuple(registrationNo), state_at) AS registrationNo_s,
        argMax(tuple(latestScheduledPickup), state_at) AS latestScheduledPickup_s,
        argMax(tuple(selectedServiceTiers), state_at) AS selectedServiceTiers_s,
        argMax(tuple(selectedAutoAcceptTiers), state_at) AS selectedAutoAcceptTiers_s,
        argMax(tuple(softBlockStiers), state_at) AS softBlockStiers_s,
        argMax(tuple(vehicleTags), state_at) AS vehicleTags_s,
        argMax(tuple(driverTag), state_at) AS driverTag_s,
        argMax(tuple(tripDistanceMinThreshold), state_at) AS tripDistanceMinThreshold_s,
        argMax(tuple(tripDistanceMaxThreshold), state_at) AS tripDistanceMaxThreshold_s,
        argMax(tuple(maxPickupRadius), state_at) AS maxPickupRadius_s,
        argMax(tuple(lastUpdatedAt), state_at) AS lastUpdatedAt_s,
        argMax(tuple(registrationNoChangedAt), state_at) AS registrationNoChangedAt_s,
        argMax(tuple(airConditionScore), state_at) AS airConditionScore_s,
        argMax(tuple(vehicleRating), state_at) AS vehicleRating_s,
        argMax(tuple(latestScheduledBooking), state_at) AS latestScheduledBooking_s,
        argMax(tuple(tollRouteBlockedTill), state_at) AS tollRouteBlockedTill_s,
        max(state_at) AS state_at_max,
        uniqExact(tuple(state_known, active, onRide, enabled, blocked, subscribed, canSwitchToRental, canSwitchToInterCity, canSwitchToIntraCity, forwardBatchingEnabled, isSpecialLocWarrior, isPetModeEnabled, chargesEnabled, hasAdvanceBooking, isDisabledReasonFlag, hasRideStarted, airConditioned, mode, onRideTripCategory, goHomeStatus, variant, rideBillingModel, enableForAirport, acUsageRestrictionType, fleetOwnerId, cloudType, registrationNo, latestScheduledPickup, selectedServiceTiers, selectedAutoAcceptTiers, softBlockStiers, vehicleTags, driverTag, tripDistanceMinThreshold, tripDistanceMaxThreshold, maxPickupRadius, lastUpdatedAt, registrationNoChangedAt, airConditionScore, vehicleRating, latestScheduledBooking, tollRouteBlockedTill)) AS states_in_bucket
    FROM atlas_kafka.driver_analytics_5m
    GROUP BY bucket, merchant_operating_city_id, driver_id
)
WHERE online_ms > 0 OR popups_sent_all > 0 OR popups_pulled > 0 OR popups_accepted > 0 OR popups_rejected > 0 OR total_bookings > 0 OR rides_started > 0 OR finished_rides > 0 OR cancelled_rides > 0 OR rides_with_rating > 0 OR rating_sum != 0;

/*
  driver_performance_hourly: the same metrics per driver x IST hour, PA's
  output grain. local_time is the IST hour as a naive wall-clock DateTime,
  like PA's. A driver seen in several cities in one hour gets the largest
  city id. Like PA's rows, an hour is a row only when the driver was online
  or had a pop-up or a ride in it.
  The state columns are the driver's latest state in the hour, in any city
  (argMax by state_at), state_at its time (NULL when none), states_in_hour
  the number of distinct states in the hour.
*/
CREATE VIEW IF NOT EXISTS atlas_kafka.driver_performance_hourly AS
SELECT
    local_time,
    merchant_operating_city_id,
    driver_id,
    popups_sent_all - popups_pulled AS popups_sent,
    popups_accepted,
    popups_rejected,
    popups_sent_all - popups_pulled - popups_accepted - popups_rejected AS popups_neglected,
    popups_pulled,
    online_ms / 3600000 AS online_time,
    online_on_ride_ms / 3600000 AS driver_on_ride_time,
    if(popups_accepted + popups_rejected = 0, 0, response_secs_sum / (popups_accepted + popups_rejected)) AS avg_response_time,
    total_bookings,
    finished_rides,
    rides_started,
    cancelled_rides,
    rides_cancelled_by_driver,
    rides_cancelled_by_user,
    if(rides_with_rating = 0, 0, rating_sum / rides_with_rating) AS avg_rating,
    rides_with_rating,
    driver_addition,
    customer_tip,
    gross_earnings,
    auto_accept_search_requests,
    auto_accept_bookings,
    auto_accept_finished_rides,
    auto_accept_cancelled_rides,
    auto_accept_rides_cancelled_by_driver,
    auto_accept_rides_cancelled_by_user,
    active_s.1 AS active,
    onRide_s.1 AS onRide,
    enabled_s.1 AS enabled,
    blocked_s.1 AS blocked,
    subscribed_s.1 AS subscribed,
    canSwitchToRental_s.1 AS canSwitchToRental,
    canSwitchToInterCity_s.1 AS canSwitchToInterCity,
    canSwitchToIntraCity_s.1 AS canSwitchToIntraCity,
    forwardBatchingEnabled_s.1 AS forwardBatchingEnabled,
    isSpecialLocWarrior_s.1 AS isSpecialLocWarrior,
    isPetModeEnabled_s.1 AS isPetModeEnabled,
    chargesEnabled_s.1 AS chargesEnabled,
    hasAdvanceBooking_s.1 AS hasAdvanceBooking,
    isDisabledReasonFlag_s.1 AS isDisabledReasonFlag,
    hasRideStarted_s.1 AS hasRideStarted,
    airConditioned_s.1 AS airConditioned,
    mode_s.1 AS mode,
    onRideTripCategory_s.1 AS onRideTripCategory,
    goHomeStatus_s.1 AS goHomeStatus,
    variant_s.1 AS variant,
    rideBillingModel_s.1 AS rideBillingModel,
    enableForAirport_s.1 AS enableForAirport,
    acUsageRestrictionType_s.1 AS acUsageRestrictionType,
    fleetOwnerId_s.1 AS fleetOwnerId,
    cloudType_s.1 AS cloudType,
    registrationNo_s.1 AS registrationNo,
    latestScheduledPickup_s.1 AS latestScheduledPickup,
    selectedServiceTiers_s.1 AS selectedServiceTiers,
    selectedAutoAcceptTiers_s.1 AS selectedAutoAcceptTiers,
    softBlockStiers_s.1 AS softBlockStiers,
    vehicleTags_s.1 AS vehicleTags,
    driverTag_s.1 AS driverTag,
    tripDistanceMinThreshold_s.1 AS tripDistanceMinThreshold,
    tripDistanceMaxThreshold_s.1 AS tripDistanceMaxThreshold,
    maxPickupRadius_s.1 AS maxPickupRadius,
    lastUpdatedAt_s.1 AS lastUpdatedAt,
    registrationNoChangedAt_s.1 AS registrationNoChangedAt,
    airConditionScore_s.1 AS airConditionScore,
    vehicleRating_s.1 AS vehicleRating,
    latestScheduledBooking_s.1 AS latestScheduledBooking,
    tollRouteBlockedTill_s.1 AS tollRouteBlockedTill,
    if(toUnixTimestamp64Milli(state_at_max) = 0, NULL, toDateTime64(state_at_max, 3, 'UTC')) AS state_at,
    states_in_hour
FROM
(
    SELECT
        toDateTime(toString(toStartOfHour(toTimeZone(bucket, 'Asia/Kolkata'))), 'UTC') AS local_time,
        driver_id,
        max(merchant_operating_city_id) AS merchant_operating_city_id,
        uniqExactIf(bucket, online = 1) * 300000 AS online_ms,
        uniqExactIf(bucket, online = 1 AND onRide = true) * 300000 AS online_on_ride_ms,
        sum(popups_sent) AS popups_sent_all,
        sum(popups_accepted) AS popups_accepted,
        sum(popups_rejected) AS popups_rejected,
        sum(popups_pulled) AS popups_pulled,
        sum(response_secs_sum) AS response_secs_sum,
        sum(auto_accept_search_requests) AS auto_accept_search_requests,
        sum(total_bookings) AS total_bookings,
        sum(finished_rides) AS finished_rides,
        sum(rides_started) AS rides_started,
        sum(cancelled_rides) AS cancelled_rides,
        sum(rides_cancelled_by_driver) AS rides_cancelled_by_driver,
        sum(rides_cancelled_by_user) AS rides_cancelled_by_user,
        sum(auto_accept_bookings) AS auto_accept_bookings,
        sum(auto_accept_finished_rides) AS auto_accept_finished_rides,
        sum(auto_accept_cancelled_rides) AS auto_accept_cancelled_rides,
        sum(auto_accept_rides_cancelled_by_driver) AS auto_accept_rides_cancelled_by_driver,
        sum(auto_accept_rides_cancelled_by_user) AS auto_accept_rides_cancelled_by_user,
        sum(rides_with_rating) AS rides_with_rating,
        sum(rating_sum) AS rating_sum,
        sum(driver_addition) AS driver_addition,
        sum(customer_tip) AS customer_tip,
        sum(gross_earnings) AS gross_earnings,
        argMax(tuple(active), state_at) AS active_s,
        argMax(tuple(onRide), state_at) AS onRide_s,
        argMax(tuple(enabled), state_at) AS enabled_s,
        argMax(tuple(blocked), state_at) AS blocked_s,
        argMax(tuple(subscribed), state_at) AS subscribed_s,
        argMax(tuple(canSwitchToRental), state_at) AS canSwitchToRental_s,
        argMax(tuple(canSwitchToInterCity), state_at) AS canSwitchToInterCity_s,
        argMax(tuple(canSwitchToIntraCity), state_at) AS canSwitchToIntraCity_s,
        argMax(tuple(forwardBatchingEnabled), state_at) AS forwardBatchingEnabled_s,
        argMax(tuple(isSpecialLocWarrior), state_at) AS isSpecialLocWarrior_s,
        argMax(tuple(isPetModeEnabled), state_at) AS isPetModeEnabled_s,
        argMax(tuple(chargesEnabled), state_at) AS chargesEnabled_s,
        argMax(tuple(hasAdvanceBooking), state_at) AS hasAdvanceBooking_s,
        argMax(tuple(isDisabledReasonFlag), state_at) AS isDisabledReasonFlag_s,
        argMax(tuple(hasRideStarted), state_at) AS hasRideStarted_s,
        argMax(tuple(airConditioned), state_at) AS airConditioned_s,
        argMax(tuple(mode), state_at) AS mode_s,
        argMax(tuple(onRideTripCategory), state_at) AS onRideTripCategory_s,
        argMax(tuple(goHomeStatus), state_at) AS goHomeStatus_s,
        argMax(tuple(variant), state_at) AS variant_s,
        argMax(tuple(rideBillingModel), state_at) AS rideBillingModel_s,
        argMax(tuple(enableForAirport), state_at) AS enableForAirport_s,
        argMax(tuple(acUsageRestrictionType), state_at) AS acUsageRestrictionType_s,
        argMax(tuple(fleetOwnerId), state_at) AS fleetOwnerId_s,
        argMax(tuple(cloudType), state_at) AS cloudType_s,
        argMax(tuple(registrationNo), state_at) AS registrationNo_s,
        argMax(tuple(latestScheduledPickup), state_at) AS latestScheduledPickup_s,
        argMax(tuple(selectedServiceTiers), state_at) AS selectedServiceTiers_s,
        argMax(tuple(selectedAutoAcceptTiers), state_at) AS selectedAutoAcceptTiers_s,
        argMax(tuple(softBlockStiers), state_at) AS softBlockStiers_s,
        argMax(tuple(vehicleTags), state_at) AS vehicleTags_s,
        argMax(tuple(driverTag), state_at) AS driverTag_s,
        argMax(tuple(tripDistanceMinThreshold), state_at) AS tripDistanceMinThreshold_s,
        argMax(tuple(tripDistanceMaxThreshold), state_at) AS tripDistanceMaxThreshold_s,
        argMax(tuple(maxPickupRadius), state_at) AS maxPickupRadius_s,
        argMax(tuple(lastUpdatedAt), state_at) AS lastUpdatedAt_s,
        argMax(tuple(registrationNoChangedAt), state_at) AS registrationNoChangedAt_s,
        argMax(tuple(airConditionScore), state_at) AS airConditionScore_s,
        argMax(tuple(vehicleRating), state_at) AS vehicleRating_s,
        argMax(tuple(latestScheduledBooking), state_at) AS latestScheduledBooking_s,
        argMax(tuple(tollRouteBlockedTill), state_at) AS tollRouteBlockedTill_s,
        max(state_at) AS state_at_max,
        uniqExact(tuple(state_known, active, onRide, enabled, blocked, subscribed, canSwitchToRental, canSwitchToInterCity, canSwitchToIntraCity, forwardBatchingEnabled, isSpecialLocWarrior, isPetModeEnabled, chargesEnabled, hasAdvanceBooking, isDisabledReasonFlag, hasRideStarted, airConditioned, mode, onRideTripCategory, goHomeStatus, variant, rideBillingModel, enableForAirport, acUsageRestrictionType, fleetOwnerId, cloudType, registrationNo, latestScheduledPickup, selectedServiceTiers, selectedAutoAcceptTiers, softBlockStiers, vehicleTags, driverTag, tripDistanceMinThreshold, tripDistanceMaxThreshold, maxPickupRadius, lastUpdatedAt, registrationNoChangedAt, airConditionScore, vehicleRating, latestScheduledBooking, tollRouteBlockedTill)) AS states_in_hour
    FROM atlas_kafka.driver_analytics_5m
    GROUP BY local_time, driver_id
)
WHERE online_ms > 0 OR popups_sent_all > 0 OR popups_pulled > 0 OR popups_accepted > 0 OR popups_rejected > 0 OR total_bookings > 0 OR rides_started > 0 OR finished_rides > 0 OR cancelled_rides > 0 OR rides_with_rating > 0 OR rating_sum != 0;
