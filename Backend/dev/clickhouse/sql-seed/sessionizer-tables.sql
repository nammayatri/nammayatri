/*
  Comments in this file must be block comments: the dev setup joins every line
  into one before piping it to clickhouse-client, so a line comment would
  swallow the rest of the file.
*/

/*
  driver_analytics_5m: one row per city x 5-minute UTC bucket x driver x
  distinct driver state. Two writers (kafka-consumers):
    SESSIONIZER          online time from LTS pings (UInt64 ms / ping counts).
    DRIVER_PERFORMANCE   performance_analytics metrics from drainer CDC topics.
  SummingMergeTree sums rows with the same key, so both writers, late data and
  re-flushes add up. DRIVER_PERFORMANCE retracts an earlier contribution with
  negative deltas, so its counts are Int64 and its money Decimal.
  non_replicated_deduplication_window lets insert_deduplication_token drop a
  re-sent identical batch on this non-replicated engine.

  State. The camelCase columns are the driver's DriverPoolData state (column
  name = the key in the DRIVER_POOL_STATE writer's hash) and are part of the
  sorting key: a bucket holds one row per distinct state the driver was in,
  each with its own online time, pings and counts. SESSIONIZER credits each
  piece of online time to the state in force during it and each ping to the
  state at its st; DRIVER_PERFORMANCE keys a pop-up or ride by the state at
  the event's own time (SRD / ride created_at), so
  later versions and retractions land in the same row. state_known = 0 (all
  state columns NULL / []) when the writer had no state for that time.
  state_at is the epoch ms at which the row's state took effect (the writer's
  history entry time), 0 when state_known = 0. deviceToken, registrationNo and
  the row-key fields are not state columns.
  PRIMARY KEY is only (city, bucket, driver), so the index stays small;
  allow_nullable_key lets the Nullable state columns sit in the sorting key.
  Always sum() the measures, grouping by the state columns you need (or by
  none of them for per-driver totals).

  Adding a state field later: add it to Processor/StateColumns.hs, then
    ALTER TABLE atlas_kafka.driver_analytics_5m
      ADD COLUMN newField Nullable(String),
      MODIFY ORDER BY (merchant_operating_city_id, bucket, driver_id,
                       state_known, <the current state columns>, newField);
  A column added in the same ALTER can be appended to the sorting key without
  rewriting any data; old rows read it as NULL. Then add argMax lines for it
  to both views (CREATE OR REPLACE VIEW).
  Removing a state field: drop it from StateColumns.hs only. The column stays
  in the table and the key; new rows write its default (NULL / []).
*/
CREATE TABLE IF NOT EXISTS atlas_kafka.driver_analytics_5m (
    `bucket`                                DateTime('UTC'),
    `merchant_operating_city_id`            String,
    `driver_id`                             String,
    `online_ms`                             UInt64,
    `online_on_ride_ms`                     UInt64,
    `pings`                                 UInt64,
    `pings_unknown_state`                   UInt64,
    `popups_sent`                           Int64,
    `popups_accepted`                       Int64,
    `popups_rejected`                       Int64,
    `popups_neglected`                      Int64,
    `response_secs_sum`                     Int64,
    `auto_accept_search_requests`           Int64,
    `total_bookings`                        Int64,
    `finished_rides`                        Int64,
    `inprogress_rides`                      Int64,
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
    `state_known`                           UInt8,
    `active`                                Nullable(Bool),
    `onRide`                                Nullable(Bool),
    `enabled`                               Nullable(Bool),
    `blocked`                               Nullable(Bool),
    `subscribed`                            Nullable(Bool),
    `canSwitchToRental`                     Nullable(Bool),
    `canSwitchToInterCity`                  Nullable(Bool),
    `canSwitchToIntraCity`                  Nullable(Bool),
    `forwardBatchingEnabled`                Nullable(Bool),
    `isSpecialLocWarrior`                   Nullable(Bool),
    `isPetModeEnabled`                      Nullable(Bool),
    `chargesEnabled`                        Nullable(Bool),
    `hasAdvanceBooking`                     Nullable(Bool),
    `isDisabledReasonFlag`                  Nullable(Bool),
    `hasRideStarted`                        Nullable(Bool),
    `airConditioned`                        Nullable(Bool),
    `mode`                                  LowCardinality(Nullable(String)),
    `onRideTripCategory`                    LowCardinality(Nullable(String)),
    `goHomeStatus`                          LowCardinality(Nullable(String)),
    `variant`                               LowCardinality(Nullable(String)),
    `rideBillingModel`                      LowCardinality(Nullable(String)),
    `enableForAirport`                      LowCardinality(Nullable(String)),
    `acUsageRestrictionType`                LowCardinality(Nullable(String)),
    `fleetOwnerId`                          LowCardinality(Nullable(String)),
    `cloudType`                             LowCardinality(Nullable(String)),
    `latestScheduledPickup`                 LowCardinality(Nullable(String)),
    `selectedServiceTiers`                  Array(LowCardinality(String)),
    `selectedAutoAcceptTiers`               Array(LowCardinality(String)),
    `softBlockStiers`                       Array(LowCardinality(String)),
    `vehicleTags`                           Array(LowCardinality(String)),
    `driverTag`                             Array(LowCardinality(String)),
    `tripDistanceMinThreshold`              Nullable(Int64),
    `tripDistanceMaxThreshold`              Nullable(Int64),
    `maxPickupRadius`                       Nullable(Int64),
    `lastUpdatedAt`                         Nullable(Int64),
    `airConditionScore`                     Nullable(Float64),
    `vehicleRating`                         Nullable(Float64),
    `latestScheduledBooking`                Nullable(DateTime64(3, 'UTC')),
    `tollRouteBlockedTill`                  Nullable(DateTime64(3, 'UTC')),
    `state_at`                              SimpleAggregateFunction(max, UInt64)
) ENGINE = SummingMergeTree()
PARTITION BY toYYYYMMDD(bucket)
PRIMARY KEY (merchant_operating_city_id, bucket, driver_id)
ORDER BY (merchant_operating_city_id, bucket, driver_id, state_known,
          active, onRide, enabled, blocked, subscribed, canSwitchToRental,
          canSwitchToInterCity, canSwitchToIntraCity, forwardBatchingEnabled,
          isSpecialLocWarrior, isPetModeEnabled, chargesEnabled, hasAdvanceBooking,
          isDisabledReasonFlag, hasRideStarted, airConditioned, mode,
          onRideTripCategory, goHomeStatus, variant, rideBillingModel,
          enableForAirport, acUsageRestrictionType, fleetOwnerId, cloudType,
          latestScheduledPickup, selectedServiceTiers, selectedAutoAcceptTiers,
          softBlockStiers, vehicleTags, driverTag, tripDistanceMinThreshold,
          tripDistanceMaxThreshold, maxPickupRadius, lastUpdatedAt,
          airConditionScore, vehicleRating, latestScheduledBooking,
          tollRouteBlockedTill)
SETTINGS allow_nullable_key = 1, non_replicated_deduplication_window = 1000;

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
    popups_sent,
    popups_accepted,
    popups_rejected,
    popups_neglected,
    online_ms / 3600000 AS online_time,
    online_on_ride_ms / 3600000 AS driver_on_ride_time,
    if(popups_accepted + popups_rejected = 0, 0, response_secs_sum / (popups_accepted + popups_rejected)) AS avg_response_time,
    total_bookings,
    finished_rides,
    inprogress_rides,
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
    airConditionScore_s.1 AS airConditionScore,
    vehicleRating_s.1 AS vehicleRating,
    latestScheduledBooking_s.1 AS latestScheduledBooking,
    tollRouteBlockedTill_s.1 AS tollRouteBlockedTill,
    if(state_at_ms = 0, NULL, fromUnixTimestamp64Milli(toInt64(state_at_ms), 'UTC')) AS state_at,
    states_in_bucket
FROM
(
    SELECT
        bucket,
        merchant_operating_city_id,
        driver_id,
        sum(online_ms) AS online_ms,
        sum(online_on_ride_ms) AS online_on_ride_ms,
        sum(popups_sent) AS popups_sent,
        sum(popups_accepted) AS popups_accepted,
        sum(popups_rejected) AS popups_rejected,
        sum(popups_neglected) AS popups_neglected,
        sum(response_secs_sum) AS response_secs_sum,
        sum(auto_accept_search_requests) AS auto_accept_search_requests,
        sum(total_bookings) AS total_bookings,
        sum(finished_rides) AS finished_rides,
        sum(inprogress_rides) AS inprogress_rides,
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
        argMax(tuple(airConditionScore), state_at) AS airConditionScore_s,
        argMax(tuple(vehicleRating), state_at) AS vehicleRating_s,
        argMax(tuple(latestScheduledBooking), state_at) AS latestScheduledBooking_s,
        argMax(tuple(tollRouteBlockedTill), state_at) AS tollRouteBlockedTill_s,
        max(state_at) AS state_at_ms,
        uniqExact(tuple(state_known, active, onRide, enabled, blocked, subscribed, canSwitchToRental, canSwitchToInterCity, canSwitchToIntraCity, forwardBatchingEnabled, isSpecialLocWarrior, isPetModeEnabled, chargesEnabled, hasAdvanceBooking, isDisabledReasonFlag, hasRideStarted, airConditioned, mode, onRideTripCategory, goHomeStatus, variant, rideBillingModel, enableForAirport, acUsageRestrictionType, fleetOwnerId, cloudType, latestScheduledPickup, selectedServiceTiers, selectedAutoAcceptTiers, softBlockStiers, vehicleTags, driverTag, tripDistanceMinThreshold, tripDistanceMaxThreshold, maxPickupRadius, lastUpdatedAt, airConditionScore, vehicleRating, latestScheduledBooking, tollRouteBlockedTill)) AS states_in_bucket
    FROM atlas_kafka.driver_analytics_5m
    GROUP BY bucket, merchant_operating_city_id, driver_id
)
WHERE online_ms > 0 OR popups_sent > 0 OR total_bookings > 0;

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
    popups_sent,
    popups_accepted,
    popups_rejected,
    popups_neglected,
    online_ms / 3600000 AS online_time,
    online_on_ride_ms / 3600000 AS driver_on_ride_time,
    if(popups_accepted + popups_rejected = 0, 0, response_secs_sum / (popups_accepted + popups_rejected)) AS avg_response_time,
    total_bookings,
    finished_rides,
    inprogress_rides,
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
    airConditionScore_s.1 AS airConditionScore,
    vehicleRating_s.1 AS vehicleRating,
    latestScheduledBooking_s.1 AS latestScheduledBooking,
    tollRouteBlockedTill_s.1 AS tollRouteBlockedTill,
    if(state_at_ms = 0, NULL, fromUnixTimestamp64Milli(toInt64(state_at_ms), 'UTC')) AS state_at,
    states_in_hour
FROM
(
    SELECT
        toDateTime(toString(toStartOfHour(toTimeZone(bucket, 'Asia/Kolkata'))), 'UTC') AS local_time,
        driver_id,
        max(merchant_operating_city_id) AS merchant_operating_city_id,
        sum(online_ms) AS online_ms,
        sum(online_on_ride_ms) AS online_on_ride_ms,
        sum(popups_sent) AS popups_sent,
        sum(popups_accepted) AS popups_accepted,
        sum(popups_rejected) AS popups_rejected,
        sum(popups_neglected) AS popups_neglected,
        sum(response_secs_sum) AS response_secs_sum,
        sum(auto_accept_search_requests) AS auto_accept_search_requests,
        sum(total_bookings) AS total_bookings,
        sum(finished_rides) AS finished_rides,
        sum(inprogress_rides) AS inprogress_rides,
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
        argMax(tuple(airConditionScore), state_at) AS airConditionScore_s,
        argMax(tuple(vehicleRating), state_at) AS vehicleRating_s,
        argMax(tuple(latestScheduledBooking), state_at) AS latestScheduledBooking_s,
        argMax(tuple(tollRouteBlockedTill), state_at) AS tollRouteBlockedTill_s,
        max(state_at) AS state_at_ms,
        uniqExact(tuple(state_known, active, onRide, enabled, blocked, subscribed, canSwitchToRental, canSwitchToInterCity, canSwitchToIntraCity, forwardBatchingEnabled, isSpecialLocWarrior, isPetModeEnabled, chargesEnabled, hasAdvanceBooking, isDisabledReasonFlag, hasRideStarted, airConditioned, mode, onRideTripCategory, goHomeStatus, variant, rideBillingModel, enableForAirport, acUsageRestrictionType, fleetOwnerId, cloudType, latestScheduledPickup, selectedServiceTiers, selectedAutoAcceptTiers, softBlockStiers, vehicleTags, driverTag, tripDistanceMinThreshold, tripDistanceMaxThreshold, maxPickupRadius, lastUpdatedAt, airConditionScore, vehicleRating, latestScheduledBooking, tollRouteBlockedTill)) AS states_in_hour
    FROM atlas_kafka.driver_analytics_5m
    GROUP BY local_time, driver_id
)
WHERE online_ms > 0 OR popups_sent > 0 OR total_bookings > 0;
