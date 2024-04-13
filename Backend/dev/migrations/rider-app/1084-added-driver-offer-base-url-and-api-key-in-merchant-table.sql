UPDATE atlas_app.merchant SET driver_offer_base_url = 'http://localhost:8016';
UPDATE atlas_app.merchant SET driver_offer_api_key = 'test-api-key-for-dev-env';
UPDATE atlas_app.merchant SET driver_offer_merchant_id = 'favorit0-0000-0000-0000-00000favorit';
ALTER TABLE atlas_app.merchant ALTER COLUMN driver_offer_base_url SET NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN driver_offer_api_key SET NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN driver_offer_merchant_id SET NOT NULL;
