-- The fare lookup reads one city/vehicle/stage; row count now multiplies by route tag.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_frfs_gtfs_stage_fare_city_vehicle_stage
    ON atlas_app.frfs_gtfs_stage_fare USING btree (merchant_operating_city_id, vehicle_type, stage);

-- Tag key mirrors normalizedRouteTagOf (upper + trim, null and blank alike) so the index enforces what the matcher believes.
CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS idx_frfs_gtfs_stage_fare_uq
    ON atlas_app.frfs_gtfs_stage_fare
    (merchant_operating_city_id, vehicle_type, stage, vehicle_service_tier_id, COALESCE(upper(btrim(route_tag)), ''));
