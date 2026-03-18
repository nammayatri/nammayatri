-- Add versioning and scheduling fields to subscription_config for plan lifecycle management
ALTER TABLE atlas_driver_offer_bpp.subscription_config
  ADD COLUMN IF NOT EXISTS version INT DEFAULT 1,
  ADD COLUMN IF NOT EXISTS scheduled_activation_date TIMESTAMP WITH TIME ZONE,
  ADD COLUMN IF NOT EXISTS scheduled_deactivation_date TIMESTAMP WITH TIME ZONE,
  ADD COLUMN IF NOT EXISTS parent_plan_id TEXT,
  ADD COLUMN IF NOT EXISTS benefits TEXT;
