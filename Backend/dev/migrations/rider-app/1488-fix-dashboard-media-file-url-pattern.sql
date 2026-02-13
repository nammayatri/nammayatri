-- Fix dashboard_media_file_url_pattern: backfill NULL values
-- This URL pattern will be used for media files (especially SOS audio recordings) sent to Kapture dashboard

-- Backfill existing NULL/blank rows with production dashboard URL
UPDATE atlas_app.rider_config
SET dashboard_media_file_url_pattern = 'https://dashboard.moving.tech/bap/media-viewer?filePath=<FILE_PATH>&rid=<RIDE_SHORT_ID>&cid=<CUSTOMER_ID>'
WHERE dashboard_media_file_url_pattern IS NULL
   OR trim(dashboard_media_file_url_pattern) = '';
