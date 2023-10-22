CREATE TABLE atlas_driver_offer_bpp.quote_rental (
id character(36) NOT NULL PRIMARY KEY,
search_request_id character(36) REFERENCES atlas_driver_offer_bpp.search_request (id) NOT NULL,
provider_id character(36) REFERENCES atlas_driver_offer_bpp.merchant (id) NOT NULL,
fare_policy_id character(36) REFERENCES atlas_driver_offer_bpp.fare_policy (id) NOT NULL,
base_distance integer NOT NULL,
base_duration integer NOT NULL,
base_fare integer NOT NULL,
fare_parameters_id character(36) REFERENCES atlas_driver_offer_bpp.fare_parameters(id) NOT NULL,
estimated_finish_time timestamp with time zone NOT NULL,
vehicle_variant character varying(255) NOT NULL,
valid_till timestamp NOT NULL,
created_at timestamp NOT NULL,
updated_at timestamp NOT NULL
);

ALTER TABLE atlas_driver_offer_bpp.quote_rental OWNER TO atlas_driver_offer_bpp_user;

-- adding odometer readings in ride for rental
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN odometer_start_reading double precision;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN odometer_end_reading double precision;

-- adding ride_type in ride table for rental
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN ride_type character varying(36);

--to identify rental or ondemand ride
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN tag character varying(36) default 'ON_DEMAND';

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN odometer_start_reading_image_path CHARACTER VARYING(255);
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN odometer_end_reading_image_path CHARACTER VARYING(255);

ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN single_batch_process_time_rental bigint DEFAULT 60;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN allocate_rental_ride_time_diff bigint DEFAULT 900;

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN search_request_tag character varying(36) default 'ON_DEMAND';
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN booking_id character(36);

ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN tag character varying(36) default 'ON_DEMAND';
ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN estimate_id DROP NOT NULL;
