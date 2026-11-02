-- 1575's unique index has no fare_class_id, so it rejects class rows beside the base row; 1582 added the replacement.
DROP INDEX IF EXISTS atlas_app.idx_frfs_gtfs_stage_fare_uq;
