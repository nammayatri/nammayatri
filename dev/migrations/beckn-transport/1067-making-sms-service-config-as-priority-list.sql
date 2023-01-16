ALTER TABLE atlas_transporter.merchant_service_usage_config DROP COLUMN send_s_m_s;
ALTER TABLE atlas_transporter.merchant_service_usage_config ADD COLUMN sms_providers_priority_list text[];
UPDATE atlas_transporter.merchant_service_usage_config SET sms_providers_priority_list='{"MyValueFirst", "ExotelSms"}';
ALTER TABLE atlas_transporter.merchant_service_usage_config ALTER COLUMN sms_providers_priority_list SET NOT NULL;
