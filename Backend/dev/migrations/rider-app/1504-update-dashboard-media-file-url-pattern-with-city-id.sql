-- Update dashboard_media_file_url_pattern to include merchant + city for control-center SOS links.
-- Dynamic segment supports both ride and non-ride SOS redirects.

ALTER TABLE atlas_app.rider_config
ALTER COLUMN dashboard_media_file_url_pattern
SET DEFAULT 'https://control-center.moving.tech/ops/<RIDES_OR_SOS>/<ID>?merchant=<MERCHANT_SHORT_ID>&city=<CITY_CODE>';

UPDATE atlas_app.rider_config
SET dashboard_media_file_url_pattern = 'https://control-center.moving.tech/ops/<RIDES_OR_SOS>/<ID>?merchant=<MERCHANT_SHORT_ID>&city=<CITY_CODE>'
WHERE dashboard_media_file_url_pattern IS NULL
   OR trim(dashboard_media_file_url_pattern) = ''
   OR dashboard_media_file_url_pattern = 'https://control-center.moving.tech/ops/rides/<RIDE_ID>';
