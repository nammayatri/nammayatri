-- NOTE: dont need to run these queries
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN IF NOT EXISTS free_wating_time integer;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN IF NOT EXISTS waiting_charge JSON;
-- NOTE: dont need to run these queries
