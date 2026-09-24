-- NOTE: dont need to run these queries
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN IF NOT EXISTS free_waiting_time integer NOT NULL Default 3;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN IF NOT EXISTS waiting_charge JSON NOT NULL Default '{"contents":1,"tag":"PerMinuteWaitingCharge"}';-- NOTE: dont need to run these queries
-- NOTE: dont need to run these queries
