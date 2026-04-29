ALTER TABLE atlas_app.gate_info
ADD COLUMN updated_at timestamp;

ALTER TABLE atlas_app.gate_info
ALTER COLUMN updated_at SET NOT NULL;

ALTER TABLE atlas_app.special_location
ADD COLUMN updated_at timestamp;

ALTER TABLE atlas_app.special_location
ALTER COLUMN updated_at SET NOT NULL;