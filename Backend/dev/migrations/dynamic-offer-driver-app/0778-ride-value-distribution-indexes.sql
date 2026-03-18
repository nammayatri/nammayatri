-- Ride Value Distribution feature indexes
-- Optimize ride value aggregation queries for fleet owners

-- Composite index for fleet ride value lookups by date range
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_ride_fleet_owner_created
    ON atlas_driver_offer_bpp.ride(fleet_owner_id, created_at DESC)
    WHERE status = 'COMPLETED' AND fleet_owner_id IS NOT NULL;

-- Covering index for driver earnings aggregation
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_ride_fleet_driver_earnings
    ON atlas_driver_offer_bpp.ride(fleet_owner_id, driver_id, created_at)
    INCLUDE (fare, total_fare, currency)
    WHERE status = 'COMPLETED' AND fleet_owner_id IS NOT NULL;
