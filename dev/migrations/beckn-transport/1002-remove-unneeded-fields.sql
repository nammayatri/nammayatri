ALTER TABLE atlas_transporter.search_request DROP COLUMN vehicle_variant;

ALTER TABLE atlas_transporter.search_request_location DROP COLUMN district;
ALTER TABLE atlas_transporter.search_request_location DROP COLUMN city;
ALTER TABLE atlas_transporter.search_request_location DROP COLUMN state;
ALTER TABLE atlas_transporter.search_request_location DROP COLUMN country;
ALTER TABLE atlas_transporter.search_request_location DROP COLUMN pincode;
ALTER TABLE atlas_transporter.search_request_location DROP COLUMN address;