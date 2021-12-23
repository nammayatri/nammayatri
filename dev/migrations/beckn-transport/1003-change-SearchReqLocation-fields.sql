ALTER TABLE atlas_transporter.driver_location RENAME COLUMN long TO lon;

ALTER TABLE atlas_transporter.search_request_location RENAME COLUMN long TO lon;

ALTER TABLE atlas_transporter.search_request_location DROP COLUMN district;
ALTER TABLE atlas_transporter.search_request_location DROP COLUMN pincode;

ALTER TABLE atlas_transporter.search_request_location ADD COLUMN street character varying(255);
UPDATE atlas_transporter.search_request_location AS T1 SET street = (T1.address :: json) ->> 'street';
ALTER TABLE atlas_transporter.search_request_location ADD COLUMN door character varying(255);
UPDATE atlas_transporter.search_request_location AS T1 SET door = (T1.address :: json) ->> 'door';
ALTER TABLE atlas_transporter.search_request_location ADD COLUMN building character varying(255);
UPDATE atlas_transporter.search_request_location AS T1 SET building = (T1.address :: json) ->> 'building';
ALTER TABLE atlas_transporter.search_request_location ADD COLUMN area_code character varying(255);
UPDATE atlas_transporter.search_request_location AS T1 SET area_code = (T1.address :: json) ->> 'areaCode';
ALTER TABLE atlas_transporter.search_request_location ADD COLUMN area character varying(255);
UPDATE atlas_transporter.search_request_location AS T1 SET area = (T1.address :: json) ->> 'area';

ALTER TABLE atlas_transporter.search_request_location DROP COLUMN address;