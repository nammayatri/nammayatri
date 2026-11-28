-- ClickHouse: atlas_driver_offer_bpp.daily_stats — online_duration (seconds)
--
-- Read by Storage/Clickhouse/DailyStats.hs (operator login-hrs / inactive KPIs).
-- Postgres atlas_driver_offer_bpp.daily_stats already has online_duration; CH seed
-- and deployed schemas must match before setting analyticsConfig.onlineDurationClickhouseFrom.
--
-- Kafka engine tables do not pick up ADD COLUMN for new JSON fields reliably —
-- drop + recreate queue and MV after altering the destination table.
-- On a replicated/distributed setup, run with ON CLUSTER <cluster>.

ALTER TABLE atlas_driver_offer_bpp.daily_stats
    ADD COLUMN IF NOT EXISTS `online_duration` Nullable(Int64);

DROP VIEW IF EXISTS atlas_driver_offer_bpp.daily_stats_mv;
DROP TABLE IF EXISTS atlas_driver_offer_bpp.daily_stats_queue;

CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.daily_stats_queue (
    `id` String,
    `driver_id` String,
    `merchant_local_date` Date,
    `total_earnings` Float64,
    `total_distance` Int64,
    `num_rides` Int64,
    `cancellation_charges` Float64,
    `bonus_earnings` Float64,
    `online_duration` Nullable(Int64),
    `updated_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='adob-sessionizer-daily_stats', kafka_group_name='ch-atlas_driver_offer_bpp-daily_stats', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_driver_offer_bpp.daily_stats_mv
TO atlas_driver_offer_bpp.daily_stats AS
SELECT id, driver_id, merchant_local_date, total_earnings, total_distance, num_rides, cancellation_charges, bonus_earnings, online_duration, updated_at AS version
FROM atlas_driver_offer_bpp.daily_stats_queue;

-- Backfill: new rows come from sessionizer JSON once online_duration is in the
-- Postgres → Kafka payload. Historical CH rows stay NULL until re-drained or
-- onlineDurationClickhouseFrom is set after a backfill; until then KPIs use PG.
