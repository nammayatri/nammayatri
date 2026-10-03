-- NOTE: dont need to run these queries
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN IF NOT EXISTS min_allowed_trip_distance integer;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN IF NOT EXISTS max_allowed_trip_distance integer;-- NOTE: dont need to run these queries
-- NOTE: dont need to run these queries
