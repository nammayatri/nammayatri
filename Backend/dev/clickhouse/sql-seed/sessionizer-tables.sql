-- Driver sessionizer output (kafka-consumers SESSIONIZER). One row per
-- 5-minute bucket x city x driver; SummingMergeTree adds rows for the same key,
-- so late data and re-flushes simply sum. Durations are in milliseconds.
-- non_replicated_deduplication_window lets insert_deduplication_token drop a
-- re-sent identical batch on this non-replicated engine.

CREATE TABLE IF NOT EXISTS atlas_kafka.driver_activity_5m (
    `bucket`                     DateTime('UTC'),
    `merchant_operating_city_id` String,
    `driver_id`                  String,
    `online_ms`                  UInt64,
    `online_on_ride_ms`          UInt64,
    `online_idle_ms`             UInt64,
    `online_unavailable_ms`      UInt64,
    `online_unknown_state_ms`    UInt64,
    `pings`                      UInt64,
    `pings_unknown_state`        UInt64
) ENGINE = SummingMergeTree()
PARTITION BY toYYYYMMDD(bucket)
ORDER BY (merchant_operating_city_id, bucket, driver_id)
SETTINGS non_replicated_deduplication_window = 1000;

CREATE TABLE IF NOT EXISTS atlas_kafka.driver_events_5m (
    `bucket`                     DateTime('UTC'),
    `merchant_operating_city_id` String,
    `driver_id`                  String,
    `event_type`                 String,
    `sub_type`                   String,
    `events`                     UInt64
) ENGINE = SummingMergeTree()
PARTITION BY toYYYYMMDD(bucket)
ORDER BY (merchant_operating_city_id, bucket, event_type, sub_type, driver_id)
SETTINGS non_replicated_deduplication_window = 1000;
