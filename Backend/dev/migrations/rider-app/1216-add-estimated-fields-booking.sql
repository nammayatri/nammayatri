ALTER TABLE atlas_app.booking ADD COLUMN estimated_distance double precision;
ALTER TABLE atlas_app.booking ADD COLUMN estimated_duration integer;

ALTER TABLE atlas_app.search_request ADD COLUMN rider_preferred_option text;