-- NOTE: dont need to run these queries

ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN IF NOT EXISTS scheduling_charge double precision;

ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN IF NOT EXISTS scheduling_charge JSON;
-- NOTE: dont need to run these queries
