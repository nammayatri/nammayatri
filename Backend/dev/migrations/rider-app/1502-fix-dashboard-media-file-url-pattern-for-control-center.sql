-- Fix dashboard_media_file_url_pattern: backfill NULL values
-- This URL pattern will be used for ride-summary links sent to Kapture

-- Backfill existing NULL/blank rows with control-center ride summary URL
UPDATE atlas_app.rider_config
SET dashboard_media_file_url_pattern = 'https://control-center.moving.tech/ops/rides/<RIDE_ID>'
WHERE dashboard_media_file_url_pattern IS NULL
   OR trim(dashboard_media_file_url_pattern) = '';
