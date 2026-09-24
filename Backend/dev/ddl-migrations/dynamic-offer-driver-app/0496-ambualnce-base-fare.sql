-- NOTE: dont need to run these queries
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN IF NOT EXISTS base_distance int NOT NULL Default 5000;
-- NOTE: dont need to run these queries
