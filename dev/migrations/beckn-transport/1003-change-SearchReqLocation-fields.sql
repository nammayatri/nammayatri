ALTER TABLE atlas_transporter.driver_location RENAME COLUMN long TO lon;

ALTER TABLE atlas_transporter.search_request_location RENAME COLUMN long TO lon;

ALTER TABLE atlas_transporter.search_request_location ADD COLUMN street character varying(255);
ALTER TABLE atlas_transporter.search_request_location ADD COLUMN door character varying(255);
ALTER TABLE atlas_transporter.search_request_location ADD COLUMN city character varying(255);
ALTER TABLE atlas_transporter.search_request_location ADD COLUMN state character varying(255);
ALTER TABLE atlas_transporter.search_request_location ADD COLUMN country character varying(255);
ALTER TABLE atlas_transporter.search_request_location ADD COLUMN building character varying(255);
ALTER TABLE atlas_transporter.search_request_location ADD COLUMN area_code character varying(255);
ALTER TABLE atlas_transporter.search_request_location ADD COLUMN area character varying(255);