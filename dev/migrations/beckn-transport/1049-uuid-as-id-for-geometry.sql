ALTER TABLE atlas_transporter.geometry
    DROP COLUMN id,
    ADD COLUMN id character(36) DEFAULT atlas_transporter.uuid_generate_v4() NOT NULL;
