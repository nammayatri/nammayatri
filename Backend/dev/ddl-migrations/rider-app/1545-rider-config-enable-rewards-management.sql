ALTER TABLE atlas_app.rider_config
  ADD COLUMN IF NOT EXISTS enable_rewards_management BOOLEAN NOT NULL DEFAULT FALSE;
