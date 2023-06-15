ALTER TABLE atlas_app.merchant ALTER COLUMN cipher_text DROP NOT NULL;
UPDATE atlas_app.merchant SET cipher_text = NULL;
