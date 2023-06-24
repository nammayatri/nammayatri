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

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN subscribed boolean;
UPDATE atlas_driver_offer_bpp.driver_information SET subscribed = true;
ALTER TABLE atlas_driver_offer_bpp.driver_information ALTER COLUMN subscribed SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_payment_cycle_duration int NOT NULL DEFAULT 86400; -- 24 hrs
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_payment_cycle_start_time int NOT NULL DEFAULT 36000; -- 10 AM
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_payment_cycle_buffer int NOT NULL DEFAULT 14400; -- 4 hrs (2 PM)
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_payment_reminder_interval int NOT NULL DEFAULT 1800; -- 30 mins
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN time_diff_from_utc int NOT NULL DEFAULT 19800; -- +5:30

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN payment_pending boolean;
UPDATE atlas_driver_offer_bpp.driver_information SET payment_pending = false;
ALTER TABLE atlas_driver_offer_bpp.driver_information ALTER COLUMN payment_pending SET NOT NULL;