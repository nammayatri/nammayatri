-- Update dashboard_media_file_url_pattern to include merchant + city for control-center SOS links.
-- Dynamic segment supports both ride and non-ride SOS redirects.

ALTER TABLE atlas_app.rider_config
ALTER COLUMN dashboard_media_file_url_pattern
SET DEFAULT 'https://control-center.moving.tech/ops/<RIDES_OR_SOS>/<ID>?merchant=<MERCHANT_SHORT_ID>&city=<CITY_CODE>';
