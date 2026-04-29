

-- DROP Not Null for updates
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN sms_providers_priority_list DROP NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN whatsapp_providers_priority_list DROP NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN initiate_call DROP NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN notify_person DROP NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN get_distances_for_cancel_ride DROP NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN enable_dashboard_sms DROP NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN aadhaar_verification_service DROP NOT NULL;
