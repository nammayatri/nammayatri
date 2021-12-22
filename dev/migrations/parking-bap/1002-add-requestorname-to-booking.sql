ALTER TABLE atlas_parking.booking ADD COLUMN requestor_name character varying(255);
ALTER TABLE atlas_parking.booking DROP COLUMN additional_info;