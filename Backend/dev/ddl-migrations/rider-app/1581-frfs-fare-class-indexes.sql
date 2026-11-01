-- 1429's (tier, stage) constraint would block class and routeTag rows beside the base row.
ALTER TABLE atlas_app.frfs_gtfs_stage_fare DROP CONSTRAINT IF EXISTS frfs_gtfs_stage_fare_vehicle_service_tier_id_stage_key;

-- Fare-class prices sit beside the routeTag rows for the same tier/stage; a tier fixes the city and vehicle type.
CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS idx_frfs_gtfs_stage_fare_tier_uq
    ON atlas_app.frfs_gtfs_stage_fare
    (vehicle_service_tier_id, stage, COALESCE(upper(btrim(route_tag)), ''), COALESCE(fare_class_id, ''));

-- One class per match-key combination within a tier; route_tag mirrors normalizedRouteTagOf.
CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS idx_frfs_fare_class_keys_uq
    ON atlas_app.frfs_fare_class
    (vehicle_service_tier_id, COALESCE(upper(btrim(route_tag)), ''), COALESCE(vehicle_tag, ''), COALESCE(vehicle_variant, ''));
