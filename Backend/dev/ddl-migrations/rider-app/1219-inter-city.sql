

ALTER TABLE atlas_app.merchant ALTER COLUMN state SET NOT NULL;

ALTER TABLE atlas_app.geometry ADD COLUMN state text;

ALTER TABLE atlas_app.geometry ALTER COLUMN state SET NOT NULL;