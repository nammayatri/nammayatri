-- NOTE: dont need to run these queries
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN IF NOT EXISTS per_stop_charge double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN IF NOT EXISTS stop_charges double precision;-- NOTE: dont need to run these queries
-- NOTE: dont need to run these queries
