ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN free_wating_time integer;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN waiting_charge JSON;

-- Don't run in prod --
UPDATE atlas_driver_offer_bpp.fare_policy_inter_city_details set free_wating_time = 5;
UPDATE atlas_driver_offer_bpp.fare_policy_inter_city_details set waiting_charge = '{"contents":1.5,"tag":"PerMinuteWaitingCharge"}';
