ALTER TABLE atlas_app.issues ALTER COLUMN contact_email DROP NOT NULL;

UPDATE atlas_app.issues AS T1 SET description = '' WHERE T1.description IS NULL;

ALTER TABLE atlas_app.issues ALTER COLUMN description SET NOT NULL;