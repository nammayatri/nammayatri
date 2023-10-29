ALTER TABLE atlas_app.merchant ALTER COLUMN driver_offer_base_url DROP NOT NULL;
UPDATE atlas_app.merchant SET driver_offer_base_url = NULL;

ALTER TABLE atlas_app.merchant ALTER COLUMN driver_offer_api_key DROP NOT NULL;
UPDATE atlas_app.merchant SET driver_offer_api_key = NULL;

-------------------------------------------------------------------------------------------
-------------------------------AFTER_FULL_ROLL_OUT-----------------------------------------
-------------------------------------------------------------------------------------------

-- ALTER TABLE atlas_app.merchant DROP COLUMN driver_offer_base_url;
-- ALTER TABLE atlas_app.merchant DROP COLUMN driver_offer_api_key;