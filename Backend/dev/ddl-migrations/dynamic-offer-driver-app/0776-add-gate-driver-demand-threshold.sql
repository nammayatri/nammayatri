-- Drop the single-int threshold columns from the earlier version of this
-- migration; they are replaced by the per-variant json columns below. -- For master
ALTER TABLE atlas_driver_offer_bpp.gate_info DROP COLUMN IF EXISTS min_driver_threshold;
ALTER TABLE atlas_driver_offer_bpp.gate_info DROP COLUMN IF EXISTS demand_threshold;

-- Existing fields kept (used by skip-counter / FCM cooldown logic).
ALTER TABLE atlas_driver_offer_bpp.gate_info ADD COLUMN notification_cooldown_in_sec integer;
ALTER TABLE atlas_driver_offer_bpp.gate_info ADD COLUMN max_ride_skips_before_queue_removal integer;

-- Per vehicle variant thresholds (jsonb: {"AUTO_RICKSHAW": 2, "SEDAN": 3, ...})
-- with per-gate defaults that apply when a variant key is missing in the json. -- For master
ALTER TABLE atlas_driver_offer_bpp.gate_info ADD COLUMN min_driver_thresholds_json text;
ALTER TABLE atlas_driver_offer_bpp.gate_info ADD COLUMN max_driver_thresholds_json text;
ALTER TABLE atlas_driver_offer_bpp.gate_info ADD COLUMN demand_thresholds_json text;
ALTER TABLE atlas_driver_offer_bpp.gate_info ADD COLUMN default_min_driver_threshold integer;
ALTER TABLE atlas_driver_offer_bpp.gate_info ADD COLUMN default_max_driver_threshold integer;
ALTER TABLE atlas_driver_offer_bpp.gate_info ADD COLUMN default_demand_threshold integer;
