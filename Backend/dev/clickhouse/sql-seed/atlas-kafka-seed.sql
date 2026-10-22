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

-- LegalNotificationEvents --------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_kafka.LegalNotificationEvents (
    `event_id`                   String,
    `occurred_at`                DateTime64(3) DEFAULT now64(),
    `merchant_id`                LowCardinality(String),
    `merchant_operating_city_id` LowCardinality(String),
    `policy_doc_id`              String,
    `policy_type`                LowCardinality(String),
    `entity_type`                LowCardinality(String),
    `policy_version`             String,
    `person_id`                  String,
    `channel`                    LowCardinality(String),
    `status`                     LowCardinality(String),
    `error_code`                 Nullable(String),
    `error_message`              Nullable(String),
    `batch_id`                   String,
    `version`                    DateTime64(3) DEFAULT now64()
) ENGINE = ReplacingMergeTree(version)
PARTITION BY toYYYYMM(occurred_at)
ORDER BY (merchant_id, policy_doc_id, person_id, channel, event_id)
TTL toDateTime(occurred_at) + INTERVAL 3 YEAR;

CREATE TABLE IF NOT EXISTS atlas_kafka.LegalNotificationEvents_queue (
    `event_id`                   String,
    `occurred_at`                DateTime64(3),
    `merchant_id`                String,
    `merchant_operating_city_id` String,
    `policy_doc_id`              String,
    `policy_type`                String,
    `entity_type`                String,
    `policy_version`             String,
    `person_id`                  String,
    `channel`                    String,
    `status`                     String,
    `error_code`                 Nullable(String),
    `error_message`              Nullable(String),
    `batch_id`                   String
) ENGINE = Kafka
SETTINGS
    kafka_broker_list     = 'localhost:29092',
    kafka_topic_list      = 'legal-notification-events',
    kafka_group_name      = 'ch-atlas_kafka-LegalNotificationEvents',
    kafka_format          = 'JSONEachRow',
    kafka_num_consumers   = 1,
    kafka_skip_broken_messages = 100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_kafka.DriverOnlineSession_mv
TO atlas_kafka.DriverOnlineSession AS
SELECT driver_id, merchant_id, merchant_op_city_id, online_at, offline_at, duration_seconds, merchant_local_date, event_type
FROM atlas_kafka.DriverOnlineSession_queue;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_kafka.LegalNotificationEvents_mv
TO atlas_kafka.LegalNotificationEvents AS
SELECT event_id, occurred_at, merchant_id, merchant_operating_city_id, policy_doc_id,
       policy_type, entity_type, policy_version, person_id, channel,
       status, error_code, error_message, batch_id
FROM atlas_kafka.LegalNotificationEvents_queue;

-- Behaviour metrics: one row per nudge / warn / block / communication a behaviour
-- rulebook decides on. Produced by Lib.BehaviorEngine.EventLog (topic
-- behaviour-consequence-events); read by the control center's Behaviour Programmes metrics.
CREATE TABLE IF NOT EXISTS atlas_kafka.behaviour_consequence_events (
    `event_id`                   String,
    `occurred_at`                DateTime64(3, 'UTC'),
    `merchant_operating_city_id` LowCardinality(String),
    `caller_app`                 LowCardinality(String),
    `entity_type`                LowCardinality(String),
    `entity_id`                  String,
    `behaviour`                  LowCardinality(String),
    `rulebook_version`           Nullable(Int32),
    `action_type`                LowCardinality(String),
    `kind`                       LowCardinality(String),
    `consequence_type`           LowCardinality(String),
    `reason_tag`                 Nullable(String),
    `template_key`               Nullable(String),
    `block_hours`                Nullable(Int32),
    `source`                     LowCardinality(String),
    `extra`                      String
) ENGINE = MergeTree()
PARTITION BY toYYYYMM(occurred_at)
ORDER BY (merchant_operating_city_id, behaviour, consequence_type, occurred_at)
TTL toDateTime(occurred_at) + INTERVAL 2 YEAR;

CREATE TABLE IF NOT EXISTS atlas_kafka.behaviour_consequence_events_queue (
    `event_id`                   String,
    `occurred_at`                DateTime64(3, 'UTC'),
    `merchant_operating_city_id` String,
    `caller_app`                 String,
    `entity_type`                String,
    `entity_id`                  String,
    `behaviour`                  String,
    `rulebook_version`           Nullable(Int32),
    `action_type`                String,
    `kind`                       String,
    `consequence_type`           String,
    `reason_tag`                 Nullable(String),
    `template_key`               Nullable(String),
    `block_hours`                Nullable(Int32),
    `source`                     String,
    `extra`                      String
) ENGINE = Kafka
SETTINGS
    kafka_broker_list     = 'localhost:29092',
    kafka_topic_list      = 'behaviour-consequence-events',
    kafka_group_name      = 'ch-atlas_kafka-behaviour_consequence_events',
    kafka_format          = 'JSONEachRow',
    kafka_num_consumers   = 1,
    kafka_skip_broken_messages = 100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_kafka.behaviour_consequence_events_mv
TO atlas_kafka.behaviour_consequence_events AS
SELECT event_id, occurred_at, merchant_operating_city_id, caller_app, entity_type, entity_id,
       behaviour, rulebook_version, action_type, kind, consequence_type, reason_tag,
       template_key, block_hours, source, extra
FROM atlas_kafka.behaviour_consequence_events_queue;
