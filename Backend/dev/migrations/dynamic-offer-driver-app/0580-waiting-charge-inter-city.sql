ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN free_wating_time integer;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN waiting_charge JSON;

UPDATE atlas_driver_offer_bpp.fare_policy_inter_city_details set free_waiting_time = 5;
UPDATE atlas_driver_offer_bpp.fare_policy_inter_city_details set waiting_charge = '{"contents":1.5,"tag":"PerMinuteWaitingCharge"}';
