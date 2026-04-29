CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.driver_availability (
    id character(36) PRIMARY KEY NOT NULL,
    driver_id character varying(255) NOT NULL,
    merchant_id character varying(255) NOT NULL,
    total_available_time integer NOT NULL,
    last_available_time timestamp NOT NULL,
    bucket_start_time timestamp with time zone NOT NULL,
    bucket_end_time timestamp with time zone NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);
