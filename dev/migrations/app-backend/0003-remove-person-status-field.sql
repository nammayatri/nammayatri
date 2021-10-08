ALTER TABLE atlas_app.person DROP COLUMN status;

ALTER TABLE atlas_app.person RENAME COLUMN verified TO is_new;
UPDATE atlas_app.person AS T1 SET is_new = NOT T1.is_new;