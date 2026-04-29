CREATE TABLE atlas_driver_offer_bpp.error_messages_translations (
    id VARCHAR(36) PRIMARY KEY,
    error_type text NOT NULL,
    language text NOT NULL,
    error_message text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);

