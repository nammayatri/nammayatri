-- migrations-read-only/rider-app/rider_config.sql adds this column too and runs first
-- on a fresh DB, so this must not fail when it already exists.
ALTER TABLE atlas_app.rider_config ADD COLUMN IF NOT EXISTS auto_favourite_driver_on_five_star_rating boolean;
