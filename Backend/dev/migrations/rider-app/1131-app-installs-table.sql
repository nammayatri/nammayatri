CREATE TABLE atlas_app.app_installs (
    id character(36) NOT NULL PRIMARY KEY,
    device_token character varying(255) NOT NULL,
    source character varying(255) NOT NULL,
    merchant_id character(36) NOT NULL REFERENCES atlas_app.merchant (id),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    platform character(36),
    app_version character(36),
    bundle_version character(36),
    CONSTRAINT unique_merchantId_deviceToken_source UNIQUE (merchant_id, device_token, source)
);
