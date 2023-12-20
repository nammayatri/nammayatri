ALTER TABLE atlas_app.merchant ADD COLUMN fallback_short_id text;
UPDATE atlas_app.merchant SET fallback_short_id = short_id;