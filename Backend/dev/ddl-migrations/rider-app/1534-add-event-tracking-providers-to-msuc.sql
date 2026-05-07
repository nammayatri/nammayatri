
ALTER TABLE atlas_app.merchant_service_usage_config
  ADD COLUMN IF NOT EXISTS event_tracking_providers TEXT[] DEFAULT NULL;
