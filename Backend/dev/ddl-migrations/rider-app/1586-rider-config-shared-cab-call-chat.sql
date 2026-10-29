ALTER TABLE atlas_app.rider_config ADD COLUMN IF NOT EXISTS shared_cab_max_cab_speed_mps integer;
ALTER TABLE atlas_app.rider_config ADD COLUMN IF NOT EXISTS shared_cab_call_enabled boolean;
ALTER TABLE atlas_app.rider_config ADD COLUMN IF NOT EXISTS shared_cab_chat_enabled boolean;
