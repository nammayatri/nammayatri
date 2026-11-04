-- ClickHouse: atlas_kafka.ScheduledRideTopDrivers — one row per top-drivers batch of a scheduled ride
--
-- Written by SharedLogic/ScheduledRideTopDrivers.hs (dynamic-offer-driver-app), which pushes one JSON message per batch
-- to the Kafka topic `scheduled-ride-top-drivers`. selectedCount is how many drivers got the request after filtering.
-- Additive: new tables only. Replace the broker list below with the environment's Kafka brokers.
-- createdAt arrives as an ISO string, so the queue keeps it as a String and the view converts it.

CREATE TABLE IF NOT EXISTS atlas_kafka.ScheduledRideTopDrivers (
    `searchTryId`                    String,
    `merchantOperatingCityId`        LowCardinality(String),
    `batchNumber`                    Int32,
    `minutesToPickup`                Int32,
    `candidateCount`                 Int32,
    `withinRadiusCount`              Int32,
    `passedStatsCount`               Int32,
    `selectedCount`                  Int32,
    `selectedDriverIds`              Array(String),
    `minRatingRequired`              Float64,
    `maxCancellationRatePctAllowed`  Float64,
    `minCompletedRidesRequired`      Int32,
    `maxPickupDistanceMeters`        Nullable(Int32),
    `usedDefaults`                   Bool,
    `createdAt`                      DateTime64(3)
) ENGINE = MergeTree()
PARTITION BY toYYYYMM(createdAt)
ORDER BY (merchantOperatingCityId, createdAt, searchTryId)
TTL toDateTime(createdAt) + INTERVAL 1 YEAR;

CREATE TABLE IF NOT EXISTS atlas_kafka.ScheduledRideTopDrivers_queue (
    `searchTryId`                    String,
    `merchantOperatingCityId`        String,
    `batchNumber`                    Int32,
    `minutesToPickup`                Int32,
    `candidateCount`                 Int32,
    `withinRadiusCount`              Int32,
    `passedStatsCount`               Int32,
    `selectedCount`                  Int32,
    `selectedDriverIds`              Array(String),
    `minRatingRequired`              Float64,
    `maxCancellationRatePctAllowed`  Float64,
    `minCompletedRidesRequired`      Int32,
    `maxPickupDistanceMeters`        Nullable(Int32),
    `usedDefaults`                   Bool,
    `createdAt`                      String
) ENGINE = Kafka
SETTINGS
    kafka_broker_list     = 'localhost:29092',
    kafka_topic_list      = 'scheduled-ride-top-drivers',
    kafka_group_name      = 'ch-atlas_kafka-ScheduledRideTopDrivers',
    kafka_format          = 'JSONEachRow',
    kafka_num_consumers   = 1,
    kafka_skip_broken_messages = 100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_kafka.ScheduledRideTopDrivers_mv
TO atlas_kafka.ScheduledRideTopDrivers AS
SELECT
    searchTryId, merchantOperatingCityId, batchNumber, minutesToPickup,
    candidateCount, withinRadiusCount, passedStatsCount, selectedCount, selectedDriverIds,
    minRatingRequired, maxCancellationRatePctAllowed, minCompletedRidesRequired,
    maxPickupDistanceMeters, usedDefaults,
    parseDateTime64BestEffort(createdAt, 3) AS createdAt
FROM atlas_kafka.ScheduledRideTopDrivers_queue;

-- Check after a scheduled ride runs:
--   SELECT searchTryId, batchNumber, candidateCount, selectedCount, createdAt
--   FROM atlas_kafka.ScheduledRideTopDrivers ORDER BY createdAt DESC LIMIT 20;
