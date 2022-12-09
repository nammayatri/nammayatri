ALTER TABLE atlas_transporter.driver_information ADD COLUMN blocked boolean;

UPDATE atlas_transporter.driver_information SET blocked = false;

ALTER TABLE atlas_transporter.driver_information ALTER COLUMN blocked SET NOT NULL;