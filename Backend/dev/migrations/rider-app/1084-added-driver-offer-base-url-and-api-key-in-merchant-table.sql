ALTER TABLE atlas_app.merchant ADD COLUMN driver_offer_base_url text;
ALTER TABLE atlas_app.merchant ADD COLUMN driver_offer_api_key varchar(128);
ALTER TABLE atlas_app.merchant ADD COLUMN driver_offer_merchant_id varchar(255);
UPDATE atlas_app.merchant SET driver_offer_base_url = 'https://d56d-2402-3a80-8c6-80b7-3409-4ac5-5220-4449.ngrok-free.app';
UPDATE atlas_app.merchant SET driver_offer_api_key = 'test-api-key-for-dev-env';
UPDATE atlas_app.merchant SET driver_offer_merchant_id = '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f';
ALTER TABLE atlas_app.merchant ALTER COLUMN driver_offer_base_url SET NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN driver_offer_api_key SET NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN driver_offer_merchant_id SET NOT NULL;
