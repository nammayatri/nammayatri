ALTER TABLE atlas_app.merchant ADD COLUMN driver_offer_base_url text;
ALTER TABLE atlas_app.merchant ADD COLUMN driver_offer_api_key varchar(128);
UPDATE atlas_app.merchant SET driver_offer_base_url = 'http://localhost:8016';
UPDATE atlas_app.merchant SET driver_offer_api_key = 'test-api-key-for-dev-env';
ALTER TABLE atlas_app.merchant ALTER COLUMN driver_offer_base_url SET NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN driver_offer_api_key SET NOT NULL;
