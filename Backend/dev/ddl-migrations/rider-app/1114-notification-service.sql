 -- FCM | PayTM
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN notify_person SET NOT NULL;

-------------------------------------------------------------------------------------------
-------------------------------DROPS-------------------------------------------------------
-------------------------------------------------------------------------------------------
ALTER TABLE atlas_app.merchant DROP COLUMN fcm_url;
ALTER TABLE atlas_app.merchant DROP COLUMN fcm_service_account;
ALTER TABLE atlas_app.merchant DROP COLUMN fcm_redis_token_key_prefix;