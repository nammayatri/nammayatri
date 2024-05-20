ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN is_customer_preffered_search_route boolean;
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN is_blocked_route boolean;
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN toll_names text[];

--ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN toll_names text[];

----------------------------------------------------------------------------------------
---------------Add The Step Values Accordingly To Current Frontend Configs---------------
----------------------------------------------------------------------------------------
ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ADD COLUMN step_fee int NOT NULL DEFAULT 0;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ADD COLUMN default_step_fee int NOT NULL DEFAULT 0;

----------------------------------------------------------------------------------------
---------------------------------Run After Prod Release---------------------------------
----------------------------------------------------------------------------------------
--ALTER TABLE atlas_driver_offer_bpp.transporter_config DROP COLUMN allow_autos_on_toll_route;
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone DROP COLUMN driver_min_fee;
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone DROP COLUMN driver_max_fee;
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone DROP COLUMN driver_pick_up_charge;