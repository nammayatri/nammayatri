

ALTER TABLE atlas_app.geometry
    DROP COLUMN id,
    ADD COLUMN id character(36) DEFAULT atlas_app.uuid_generate_v4() NOT NULL;