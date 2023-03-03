ALTER TABLE atlas_driver_offer_bpp.merchant ADD COLUMN internal_api_key varchar(128);
UPDATE atlas_driver_offer_bpp.merchant SET internal_api_key='test-api-key-for-dev-env';
ALTER TABLE atlas_driver_offer_bpp.merchant ALTER COLUMN internal_api_key SET NOT NULL;
