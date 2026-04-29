CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.driver_fee
(   id character(36) NOT NULL PRIMARY KEY,
    short_id character(36) NOT NULL,
    driver_id character varying(255) NOT NULL REFERENCES atlas_driver_offer_bpp.person(id),
    total_earnings INT NOT NULL,
    num_rides integer NOT NULL,
    govt_charges integer NOT NULL,
    platform_fee integer NOT NULL,
    cgst numeric (30,2) NOT NULL,
    sgst numeric (30,2) NOT NULL,
    pay_by timestamp with time zone NOT NULL,
    start_time timestamp with time zone NOT NULL,
    end_time timestamp with time zone NOT NULL,
    status character varying(255) NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);
ALTER TABLE atlas_driver_offer_bpp.driver_fee OWNER TO atlas_driver_offer_bpp_user;