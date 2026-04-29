ALTER TABLE atlas_app.merchant ADD COLUMN fcm_url text;
ALTER TABLE atlas_app.merchant ADD COLUMN fcm_json_path text;
ALTER TABLE atlas_app.merchant ADD COLUMN fcm_redis_token_key_prefix text;

ALTER TABLE atlas_app.merchant ALTER COLUMN fcm_url SET NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN fcm_redis_token_key_prefix SET NOT NULL;
