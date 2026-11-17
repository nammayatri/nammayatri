-- Fare-class prices sit beside the routeTag rows for the same tier/stage; a tier fixes the city and vehicle type.
-- Created before 1591 drops the older uniques; its key contains theirs, so existing rows always satisfy it.
CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS idx_frfs_gtfs_stage_fare_tier_uq
    ON atlas_app.frfs_gtfs_stage_fare
    (vehicle_service_tier_id, stage, COALESCE(upper(btrim(route_tag)), ''), COALESCE(fare_class_id, ''));
