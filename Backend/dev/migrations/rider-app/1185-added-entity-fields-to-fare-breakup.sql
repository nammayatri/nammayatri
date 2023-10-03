ALTER TABLE atlas_app.fare_breakup ADD COLUMN entity_id character (36);
ALTER TABLE atlas_app.fare_breakup ADD COLUMN entity_type character varying (255) NOT NULL;