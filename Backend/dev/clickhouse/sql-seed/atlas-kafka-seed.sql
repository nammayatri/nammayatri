CREATE TABLE atlas_kafka.driver_eda_kafka (
    `driver_id` String,
    `rid` Nullable(String),
    `ts` DateTime64(3) DEFAULT now(),
    `acc` Nullable(String),
    `rideStatus` Nullable(String),
    `lat` Nullable(String),
    `lon` Nullable(String),
    `mid` Nullable(String),
    `updated_at` Nullable(String),
    `created_at` Nullable(String),
    `on_ride` Nullable(String),
    `active` Nullable(String),
    `partition_date` Date,
    `date` DateTime DEFAULT now()
) ENGINE = MergeTree() PRIMARY KEY (ts);

-- DriverHealthCheck --------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_kafka.DriverHealthCheck (
    `driver_id`           String,
    `ts`                  DateTime64(3) DEFAULT now(),
    `merchant_op_city_id` Nullable(String),
    `ping_count`          Nullable(Int32),
    `mode`                Nullable(String),
    `eventType`           Nullable(String)
) ENGINE = MergeTree() PRIMARY KEY (ts);

CREATE TABLE IF NOT EXISTS atlas_kafka.DriverHealthCheck_queue (
    `driver_id`           String,
    `ts`                  DateTime64(3),
    `merchant_op_city_id` Nullable(String),
    `ping_count`          Nullable(Int32),
    `mode`                Nullable(String),
    `eventType`           Nullable(String)
) ENGINE = Kafka
SETTINGS
    kafka_broker_list     = 'localhost:29092',
    kafka_topic_list      = 'driver-health-check-ping',
    kafka_group_name      = 'ch-atlas_kafka-DriverHealthCheck',
    kafka_format          = 'JSONEachRow',
    kafka_num_consumers   = 1,
    kafka_skip_broken_messages = 100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_kafka.DriverHealthCheck_mv
TO atlas_kafka.DriverHealthCheck AS
SELECT driver_id, ts, merchant_op_city_id, ping_count, mode, eventType
FROM atlas_kafka.DriverHealthCheck_queue;

-- DriverOnlineSession ---------------------------------------------------------
-- App-pushed (driver-app, pushToKafka) completed driver online sessions.
-- Sessions crossing merchant-local midnight are split into one row per day.
CREATE TABLE IF NOT EXISTS atlas_kafka.DriverOnlineSession (
    `driver_id`          String,
    `merchant_id`        String,
    `merchant_op_city_id` String,
    `online_at`          DateTime64(3),
    `offline_at`         DateTime64(3),
    `duration_seconds`   Int64,
    `merchant_local_date` Date,
    `event_type`         String
) ENGINE = MergeTree() PRIMARY KEY (driver_id, merchant_local_date, online_at);

CREATE TABLE IF NOT EXISTS atlas_kafka.DriverOnlineSession_queue (
    `driver_id`          String,
    `merchant_id`        String,
    `merchant_op_city_id` String,
    `online_at`          DateTime64(3),
    `offline_at`         DateTime64(3),
    `duration_seconds`   Int64,
    `merchant_local_date` Date,
    `event_type`         String
) ENGINE = Kafka
SETTINGS
    kafka_broker_list     = 'localhost:29092',
    kafka_topic_list      = 'driver-online-sessions',
    kafka_group_name      = 'ch-atlas_kafka-DriverOnlineSession',
    kafka_format          = 'JSONEachRow',
    kafka_num_consumers   = 1,
    kafka_skip_broken_messages = 100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_kafka.DriverOnlineSession_mv
TO atlas_kafka.DriverOnlineSession AS
SELECT driver_id, merchant_id, merchant_op_city_id, online_at, offline_at, duration_seconds, merchant_local_date, event_type
FROM atlas_kafka.DriverOnlineSession_queue;
