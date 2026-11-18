-- ClickHouse: atlas_driver_offer_bpp.fleet_operator_daily_stats — GST / VAT / TDS per day, split cash vs online
--
-- Read by Storage/Clickhouse/FleetOperatorDailyStats.hs (dynamic-offer-driver-app) and by control-center's
-- fleet analytics. Net earnings subtract the kinds listed in transporter_config.analytics_config
-- .netEarningsDeductions (GST, VAT, TDS, PlatformFee). Matches the Postgres columns added in
-- dev/migrations-read-only/dynamic-offer-driver-app/fleet_operator_daily_stats.sql.
--
-- Run BEFORE deploying the driver app that reads these columns: its earnings queries select them.
-- Rows written before the deploy keep NULL here, so for those dates net only subtracts platform fees.

-- 1. Target table. Additive and safe on the live table.
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats
    ADD COLUMN IF NOT EXISTS `cash_gst` Nullable(Float64),
    ADD COLUMN IF NOT EXISTS `online_gst` Nullable(Float64),
    ADD COLUMN IF NOT EXISTS `cash_vat` Nullable(Float64),
    ADD COLUMN IF NOT EXISTS `online_vat` Nullable(Float64),
    ADD COLUMN IF NOT EXISTS `cash_tds` Nullable(Float64),
    ADD COLUMN IF NOT EXISTS `online_tds` Nullable(Float64);

-- 2. Ingestion. Where the table is fed from the Kafka engine table + materialized view (as in
-- sql-seed/atlas-driver-offer-bpp-seed.sql), the queue and the view must carry the columns too, or new
-- values never arrive. A Kafka engine table cannot be altered: detach the view, recreate the queue with the
-- six columns, recreate the view with them in its SELECT, then attach. The seed has the final definitions.
--
--   DROP VIEW IF EXISTS atlas_driver_offer_bpp.fleet_operator_daily_stats_mv;
--   DROP TABLE IF EXISTS atlas_driver_offer_bpp.fleet_operator_daily_stats_queue;
--   -- CREATE TABLE ... fleet_operator_daily_stats_queue (...) and CREATE MATERIALIZED VIEW ... from the seed
--
-- The consumer group is kept (same kafka_group_name), so it resumes from its committed offset.
-- Replicated / multi-shard clusters: run with ON CLUSTER <cluster>.
