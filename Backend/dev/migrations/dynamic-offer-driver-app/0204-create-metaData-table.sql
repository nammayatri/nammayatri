CREATE TABLE atlas_driver_offer_bpp.meta_data (
    driver_id character(36) PRIMARY KEY,
    device text,
    device_o_s text,
    device_date_time timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    app_permissions text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);
