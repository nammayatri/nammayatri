-- 1575's unique index and 1429's (tier, stage) constraint have no fare_class_id, so they reject class and routeTag rows
-- beside the base row; 1590 added the replacement.
DROP INDEX IF EXISTS atlas_app.idx_frfs_gtfs_stage_fare_uq;
ALTER TABLE atlas_app.frfs_gtfs_stage_fare DROP CONSTRAINT IF EXISTS frfs_gtfs_stage_fare_vehicle_service_tier_id_stage_key;
