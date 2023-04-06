ALTER TABLE atlas_app.special_location ADD COLUMN merchant_id character(36) REFERENCES atlas_app.merchant (id);

UPDATE atlas_app.special_location SET merchant_id = 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51';

ALTER TABLE atlas_app.special_location ALTER COLUMN merchant_id SET NOT NULL;
