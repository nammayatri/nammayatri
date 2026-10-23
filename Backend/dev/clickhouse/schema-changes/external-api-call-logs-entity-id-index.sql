-- ClickHouse: atlas_kafka.external_api_call_logs — skip index on entityId
--
-- Read by Storage/Clickhouse/ExternalApiCallLogs.hs (dynamic-offer-driver-app), which backs the dashboard
-- GET /ride/{rideId}/estimatedRoute: the maps calls logged against a booking's transaction id.
--
-- The table is ORDER BY (id, createdAt), so a lookup by entityId can't use the primary key and scans every
-- granule of the week's partition. A bloom filter lets ClickHouse skip the granules that can't hold the id.
-- Additive and safe to run on the live table; the code works without it, only slower.

ALTER TABLE atlas_kafka.external_api_call_logs
    ADD INDEX IF NOT EXISTS idx_entity_id entityId TYPE bloom_filter(0.01) GRANULARITY 1;

-- The index applies to new parts only. Build it for the rows already there (at most 8 days, the table TTL),
-- as a background mutation:
ALTER TABLE atlas_kafka.external_api_call_logs MATERIALIZE INDEX idx_entity_id;

-- Replicated table: the ALTER replicates within a shard. On a multi-shard cluster, run both statements with
-- ON CLUSTER <cluster>.
--
-- Check it is used (look for idx_entity_id under "Skip" with far fewer granules than the partition has):
--   EXPLAIN indexes = 1
--   SELECT count() FROM atlas_kafka.external_api_call_logs
--   WHERE entityId = '<transaction id>' AND createdAt > now() - INTERVAL 1 DAY;
