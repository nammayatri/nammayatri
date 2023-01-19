ALTER TABLE atlas_app.merchant ADD COLUMN fcm_url text;
ALTER TABLE atlas_app.merchant ADD COLUMN fcm_json_path text;
ALTER TABLE atlas_app.merchant ADD COLUMN fcm_redis_token_key_prefix text;

-- test data for dev
UPDATE atlas_app.merchant
SET fcm_url =  'http://localhost:4545/',
    fcm_json_path = 'dummy-fcm.json',
    fcm_redis_token_key_prefix = id;

ALTER TABLE atlas_app.merchant ALTER COLUMN fcm_url SET NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN fcm_redis_token_key_prefix SET NOT NULL;
