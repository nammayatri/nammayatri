-- Fleet Driver Directory feature: indexes for efficient directory lookups

-- Partial index on person table for directory-eligible drivers
-- Speeds up the base filter: role = DRIVER in a specific operating city
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_person_directory_lookup
    ON atlas_driver_offer_bpp.person(merchant_operating_city_id, role)
    WHERE role = 'DRIVER';

-- Partial index on fleet_driver_association for active associations
-- Speeds up the left-join exclusion of already-linked drivers
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_fda_fleet_driver_active
    ON atlas_driver_offer_bpp.fleet_driver_association(fleet_owner_id, driver_id)
    WHERE is_active = true;
