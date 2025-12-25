UPDATE atlas_app.merchant_service_usage_config SET sms_providers_priority_list='{"MyValueFirst", "ExotelSms"}';
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN sms_providers_priority_list SET NOT NULL;
