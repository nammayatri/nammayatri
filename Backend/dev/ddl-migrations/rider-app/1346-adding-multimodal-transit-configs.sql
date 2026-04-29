

-- ONLY FOR MASTER AND PROD
ALTER TABLE atlas_app.journey ALTER COLUMN modes TYPE text[] USING modes::text[];