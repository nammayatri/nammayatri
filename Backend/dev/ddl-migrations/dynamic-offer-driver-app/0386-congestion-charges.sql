-- NOTE: dont need to run these queries
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN IF NOT EXISTS congestion_charge_multiplier double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN IF NOT EXISTS congestion_charge int;-- NOTE: dont need to run these queries
-- NOTE: dont need to run these queries
