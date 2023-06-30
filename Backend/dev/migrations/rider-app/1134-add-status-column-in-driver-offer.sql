ALTER TABLE atlas_app.driver_offer
    ADD COLUMN status character varying(255) DEFAULT 'ACTIVE' NOT NULL,
    ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;

