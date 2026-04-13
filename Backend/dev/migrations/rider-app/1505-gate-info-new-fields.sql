-- Drop the single-int threshold columns from the earlier version of this
-- migration; they are replaced by the per-variant json columns below. -- For master
ALTER TABLE atlas_app.gate_info DROP COLUMN IF EXISTS min_driver_threshold;
ALTER TABLE atlas_app.gate_info DROP COLUMN IF EXISTS demand_threshold;

-- Existing fields kept (used by skip-counter / FCM cooldown logic).
ALTER TABLE atlas_app.gate_info ADD COLUMN notification_cooldown_in_sec integer;
ALTER TABLE atlas_app.gate_info ADD COLUMN max_ride_skips_before_queue_removal integer;

-- Per vehicle variant thresholds (jsonb: {"AUTO_RICKSHAW": 2, "SEDAN": 3, ...})
-- with per-gate defaults that apply when a variant key is missing in the json. -- For master
ALTER TABLE atlas_app.gate_info ADD COLUMN min_driver_thresholds_json text;
ALTER TABLE atlas_app.gate_info ADD COLUMN max_driver_thresholds_json text;
ALTER TABLE atlas_app.gate_info ADD COLUMN demand_thresholds_json text;
ALTER TABLE atlas_app.gate_info ADD COLUMN default_min_driver_threshold integer;
ALTER TABLE atlas_app.gate_info ADD COLUMN default_max_driver_threshold integer;
ALTER TABLE atlas_app.gate_info ADD COLUMN default_demand_threshold integer;

-- Seed small sane defaults so the gate can fire notifications out of the box.
-- The *_json columns hold per-variant overrides; the default_* int columns are the
-- fallback when a variant key is missing in the json. Ops can edit either later. -- For master
UPDATE atlas_app.gate_info
SET min_driver_thresholds_json = '{"AUTO_RICKSHAW":2,"SEDAN":2,"HATCHBACK":2,"SUV":2}',
    max_driver_thresholds_json = '{"AUTO_RICKSHAW":4,"SEDAN":4,"HATCHBACK":4,"SUV":4}',
    demand_thresholds_json = '{"AUTO_RICKSHAW":2,"SEDAN":2,"HATCHBACK":2,"SUV":2}',
    default_min_driver_threshold = 2,
    default_max_driver_threshold = 4,
    default_demand_threshold = 2
WHERE can_queue_up_on_gate = true;
