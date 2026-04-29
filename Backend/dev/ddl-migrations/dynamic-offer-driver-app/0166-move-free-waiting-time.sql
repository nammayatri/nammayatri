ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN free_wating_time integer;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN free_wating_time integer;

-- ONLY FOR LOCAL
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN waiting_time_estimated_threshold int;

-------------------------------------------------------------------------------------------
-------------------------------DROPS-------------------------------------------------------
-------------------------------------------------------------------------------------------

ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ALTER COLUMN free_wating_time SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ALTER COLUMN free_wating_time SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.transporter_config DROP COLUMN waiting_time_estimated_threshold;
