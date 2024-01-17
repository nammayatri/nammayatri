ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN odometer_start_reading double precision;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN odometer_end_reading double precision;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN end_ride_otp char(4);

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN opt_for_rental boolean NOT NULL DEFAULT false; -- only for local
UPDATE atlas_driver_offer_bpp.driver_information SET opt_for_rental = true where merchant_id= 'favorit0-0000-0000-0000-00000favorit'; -- YS

INSERT INTO atlas_driver_offer_bpp.fare_policy (id, night_shift_start, night_shift_end, created_at, updated_at, min_allowed_trip_distance, max_allowed_trip_distance, service_charge, govt_charges, fare_policy_type, description) VALUES ('71b52524-e773-03dc-5853-290132ce6fd5', '22:00:00', '06:00:00', now(), now(), 0, 100000, null, null, 'Rental', 'Yatri Sathi SEDAN Rental');
INSERT INTO atlas_driver_offer_bpp.fare_policy (id, night_shift_start, night_shift_end, created_at, updated_at, min_allowed_trip_distance, max_allowed_trip_distance, service_charge, govt_charges, fare_policy_type, description) VALUES ('51b42524-e113-03dc-5453-290032ce6fd5', '22:00:00', '06:00:00', now(), now(), 0, 100000, null, null, 'Rental', 'Yatri Sathi SUV Rental');

INSERT INTO atlas_driver_offer_bpp.fare_product (id, merchant_id, fare_policy_id, vehicle_variant, area, flow, merchant_operating_city_id) VALUES ('294abc7f-9cc9-e3t3-3c8b-7721c6f1809f', 'favorit0-0000-0000-0000-00000favorit', '71b52524-e773-03dc-5853-290132ce6fd5', 'SEDAN', 'Default', 'RENTAL',(SELECT id from atlas_driver_offer_bpp.merchant_operating_city where merchant_id = 'favorit0-0000-0000-0000-00000favorit'));
INSERT INTO atlas_driver_offer_bpp.fare_product (id, merchant_id, fare_policy_id, vehicle_variant, area, flow, merchant_operating_city_id) VALUES ('394abc7f-9cc9-e3t3-3c8b-7721c6f1809f', 'favorit0-0000-0000-0000-00000favorit', '51b42524-e113-03dc-5453-290032ce6fd5', 'SUV', 'Default', 'RENTAL',(SELECT id from atlas_driver_offer_bpp.merchant_operating_city where merchant_id = 'favorit0-0000-0000-0000-00000favorit'));
INSERT INTO atlas_driver_offer_bpp.fare_product (id, merchant_id, fare_policy_id, vehicle_variant, area, flow, merchant_operating_city_id) VALUES ('494abc7f-9cc9-e3t3-3c8b-7721c6f1809f', 'favorit0-0000-0000-0000-00000favorit', '51b42524-e113-03dc-5453-290032ce6fd5', 'AUTO_RICKSHAW', 'Default', 'RENTAL',(SELECT id from atlas_driver_offer_bpp.merchant_operating_city where merchant_id = 'favorit0-0000-0000-0000-00000favorit'));

CREATE TABLE atlas_driver_offer_bpp.fare_policy_rental_details (
  id serial PRIMARY KEY,
  fare_policy_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.fare_policy(id),
  base_fare integer NOT NULL,
  per_hour_charge integer NOT NULL,
  per_hour_free_kms integer NOT NULL,
  per_extra_km_rate integer NOT NULL,
  night_shift_charge JSON NOT NULL
);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details OWNER TO atlas_driver_offer_bpp_user;

CREATE TABLE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers (
  id serial PRIMARY KEY,
  fare_policy_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.fare_policy(id),
  ride_duration integer NOT NULL,
  buffer_kms integer NOT NULL,
  CONSTRAINT fp_rental_details_distance_buffers_unique_ride_duration UNIQUE (fare_policy_id, ride_duration)
);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers OWNER TO atlas_driver_offer_bpp_user;

CREATE TABLE atlas_driver_offer_bpp.fare_parameters_rental_details (
  fare_parameters_id character(36) PRIMARY KEY NOT NULL REFERENCES atlas_driver_offer_bpp.fare_parameters(id),
  time_based_fare integer NOT NULL,
  extra_dist_fare integer NOT NULL
);
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details OWNER TO atlas_driver_offer_bpp_user;

INSERT INTO atlas_driver_offer_bpp.fare_policy_rental_details (fare_policy_id, base_fare, per_hour_charge, per_hour_free_kms, per_extra_km_rate, night_shift_charge) VALUES
('71b52524-e773-03dc-5853-290132ce6fd5', 360, 180, 10, 18, '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json),
('51b42524-e113-03dc-5453-290032ce6fd5', 440, 220, 10, 22, '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json);

INSERT INTO atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers (fare_policy_id, ride_duration, buffer_kms) VALUES
('71b52524-e773-03dc-5853-290132ce6fd5', 0, 4),
('71b52524-e773-03dc-5853-290132ce6fd5', 5, 8),
('51b42524-e113-03dc-5453-290032ce6fd5', 0, 4),
('51b42524-e113-03dc-5453-290032ce6fd5', 5, 8);

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN rental_requests BOOLEAN NOT NULL DEFAULT false;
UPDATE atlas_driver_offer_bpp.transporter_config SET rental_requests = true where merchant_id = 'favorit0-0000-0000-0000-00000favorit'; -- YS

CREATE TABLE atlas_driver_offer_bpp.quote_rental (
id character(36) NOT NULL PRIMARY KEY,
search_request_id character(36) REFERENCES atlas_driver_offer_bpp.search_request (id) NOT NULL,
provider_id character(36) REFERENCES atlas_driver_offer_bpp.merchant (id) NOT NULL,
fare_policy_id character(36) REFERENCES atlas_driver_offer_bpp.fare_policy (id) NOT NULL,
vehicle_variant character varying(255) NOT NULL,
valid_till timestamp NOT NULL,
fare_parameters_id character(36) REFERENCES atlas_driver_offer_bpp.fare_parameters(id) NOT NULL,
estimated_finish_time timestamp with time zone NOT NULL,
base_distance integer NOT NULL,
base_duration integer NOT NULL,
base_fare integer NOT NULL,
created_at timestamp NOT NULL,
updated_at timestamp NOT NULL
);

ALTER TABLE atlas_driver_offer_bpp.quote_rental OWNER TO atlas_driver_offer_bpp_user;

-- adding ride_type in ride table for rental
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN ride_type character varying(36);

--to identify rental or ondemand ride
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN tag character varying(36) default 'ON_DEMAND';

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN odometer_start_reading_image_id CHARACTER VARYING(255);
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN odometer_end_reading_image_id CHARACTER VARYING(255);

ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN single_batch_process_time_rental bigint DEFAULT 60;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN allocate_rental_ride_time_diff bigint DEFAULT 1800;

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN search_request_tag character varying(36) default 'ON_DEMAND';
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN booking_id character(36);

ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN tag character varying(36) default 'ON_DEMAND';
ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN estimate_id DROP NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN next_stop_loc_id character(36);

ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN search_request_tag character varying(36) NOT NULL default 'ON_DEMAND';
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN allocate_rental_ride_time_diff bigint[] DEFAULT '{}'; -- 1200, 300

INSERT INTO atlas_driver_offer_bpp.driver_pool_config (
    id,
    merchant_id,
    merchant_operating_city_id,
    min_radius_of_search,
    max_radius_of_search,
    radius_step_size,
    driver_position_info_expiry,
    actual_distance_threshold,
    max_driver_quotes_required,
    driver_quote_limit,
    driver_request_count_limit,
    driver_batch_size,
    distance_based_batch_split,
    max_number_of_batches,
    max_parallel_search_requests,
    pool_sorting_type,
    single_batch_process_time,
    single_batch_process_time_rental,
    trip_distance,
    radius_shrink_value_for_drivers_on_ride,
    driver_to_destination_distance_threshold,
    driver_to_destination_duration,
    created_at,
    updated_at,
    vehicle_variant,
    search_request_tag,
    allocate_rental_ride_time_diff
)
SELECT
    atlas_driver_offer_bpp.uuid_generate_v4() as id,
    merchant_id,
    merchant_operating_city_id,
    min_radius_of_search,
    max_radius_of_search,
    radius_step_size,
    driver_position_info_expiry,
    actual_distance_threshold,
    max_driver_quotes_required,
    driver_quote_limit,
    driver_request_count_limit,
    driver_batch_size,
    distance_based_batch_split,
    max_number_of_batches,
    max_parallel_search_requests,
    pool_sorting_type,
    single_batch_process_time,
    single_batch_process_time_rental,
    trip_distance,
    radius_shrink_value_for_drivers_on_ride,
    driver_to_destination_distance_threshold,
    driver_to_destination_duration,
    created_at,
    updated_at,
    vehicle_variant,
    'RENTAL' as search_request_tag,
    '{1200,300}' as allocate_rental_ride_time_diff
FROM
    atlas_driver_offer_bpp.driver_pool_config
WHERE
    merchant_operating_city_id = '745b475b-0e5a-2633-d1a5-7e8de8f1403d'; -- Kolkata
