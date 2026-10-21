-- Per-zone override of rider_config.enable_one_way_scheduled_rides; NULL follows the city.
ALTER TABLE atlas_app.special_location ADD COLUMN IF NOT EXISTS enable_one_way_scheduled_rides boolean;
