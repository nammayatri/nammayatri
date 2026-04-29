ALTER TABLE atlas_app.merchant RENAME COLUMN fcm_json_path TO fcm_service_account;

ALTER TABLE atlas_app.merchant ALTER COLUMN fcm_service_account SET NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN fcm_url SET NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN fcm_redis_token_key_prefix SET NOT NULL;