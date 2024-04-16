ALTER TABLE atlas_app.search_request ADD COLUMN language character varying(255);
ALTER TABLE atlas_app.booking_location ADD COLUMN ward character varying(255);
ALTER TABLE atlas_app.saved_location ADD COLUMN ward character varying(255);