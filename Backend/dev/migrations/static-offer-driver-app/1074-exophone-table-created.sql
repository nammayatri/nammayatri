CREATE TABLE atlas_transporter.exophone (
    id character(36) PRIMARY KEY NOT NULL,
    merchant_id character(36) NOT NULL REFERENCES atlas_transporter.merchant (id),
    primary_phone character varying(255) NOT NULL,
    backup_phone character varying(255) NOT NULL,
    is_primary_down boolean NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT  exophone_unique_primary_phone UNIQUE (primary_phone),
    CONSTRAINT  exophone_unique_backup_phone UNIQUE (backup_phone)
);

INSERT INTO atlas_transporter.exophone (id, merchant_id, primary_phone, backup_phone, is_primary_down)
    (SELECT
        atlas_transporter.uuid_generate_v4(),
        T1.id,
        unnest (T1.exo_phones),
        unnest (T1.exo_phones),
        false
    FROM atlas_transporter.merchant AS T1)
    ON CONFLICT DO NOTHING;

ALTER TABLE atlas_transporter.merchant DROP COLUMN exo_phones;

ALTER TABLE atlas_transporter.booking RENAME COLUMN provider_exo_phone TO primary_exophone;
