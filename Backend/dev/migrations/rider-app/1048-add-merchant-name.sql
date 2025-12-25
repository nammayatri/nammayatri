UPDATE atlas_app.merchant SET name =  short_id;

ALTER TABLE atlas_app.merchant ALTER COLUMN name SET NOT NULL;
