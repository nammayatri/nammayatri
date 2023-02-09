ALTER TABLE atlas_app.merchant ADD COLUMN fcm_url text;
ALTER TABLE atlas_app.merchant ADD COLUMN fcm_json_path text;
ALTER TABLE atlas_app.merchant ADD COLUMN fcm_redis_token_key_prefix text;

-- test data for dev
UPDATE atlas_app.merchant
SET fcm_url =  'http://localhost:4545/',
    fcm_json_path = 'dummy-fcm.json',
    fcm_redis_token_key_prefix = id
WHERE id = 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51';

ALTER TABLE atlas_app.merchant ALTER COLUMN fcm_url SET NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN fcm_redis_token_key_prefix SET NOT NULL;
