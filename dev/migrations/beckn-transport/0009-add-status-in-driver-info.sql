ALTER TABLE atlas_transporter.person DROP COLUMN status;
ALTER TABLE atlas_transporter.driver_information ADD COLUMN enabled boolean;
UPDATE atlas_transporter.driver_information SET enabled = True;
ALTER TABLE atlas_transporter.driver_information ALTER COLUMN enabled SET NOT NULL;

ALTER TABLE atlas_transporter.person RENAME COLUMN verified TO is_new;
UPDATE atlas_transporter.person AS T1 SET is_new = NOT T1.is_new;