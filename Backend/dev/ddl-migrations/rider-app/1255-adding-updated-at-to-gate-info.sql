ALTER TABLE atlas_app.special_location
ADD COLUMN updated_at timestamp;

ALTER TABLE atlas_app.special_location
ALTER COLUMN updated_at SET NOT NULL;