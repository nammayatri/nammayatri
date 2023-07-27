ALTER TABLE atlas_app.saved_location ALTER COLUMN place_id type text;
ALTER TABLE atlas_app.search_request_location ALTER COLUMN place_id type text;
ALTER TABLE atlas_app.booking_location ALTER COLUMN place_id type text;
ALTER TABLE atlas_app.place_name_cache ALTER COLUMN place_id type text;