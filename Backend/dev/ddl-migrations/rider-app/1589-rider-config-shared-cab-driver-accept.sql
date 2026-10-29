ALTER TABLE atlas_app.rider_config ADD COLUMN IF NOT EXISTS shared_cab_driver_accept_enabled boolean;
ALTER TABLE atlas_app.rider_config ADD COLUMN IF NOT EXISTS shared_cab_offer_timeout_sec integer;
ALTER TABLE atlas_app.rider_config ADD COLUMN IF NOT EXISTS shared_cab_offer_retry_timeout_sec integer;
