ALTER TABLE atlas_driver_offer_bpp.merchant ALTER COLUMN internal_api_key DROP NOT NULL;
UPDATE atlas_driver_offer_bpp.merchant SET internal_api_key = NULL;

-------------------------------------------------------------------------------------------
-------------------------------AFTER_FULL_ROLL_OUT-----------------------------------------
-------------------------------------------------------------------------------------------

-- ALTER TABLE atlas_driver_offer_bpp.merchant DROP COLUMN internal_api_key;