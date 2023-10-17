ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN odometer_start_reading int;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN odometer_end_reading int;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN end_ride_otp char(4);

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN opt_for_rental boolean NOT NULL DEFAULT false; -- only for local
UPDATE atlas_driver_offer_bpp.driver_information SET opt_for_rental = true where merchant_id= 'favorit0-0000-0000-0000-00000favorit'; -- YS

INSERT INTO atlas_driver_offer_bpp.fare_policy (id, night_shift_start, night_shift_end, created_at, updated_at, min_allowed_trip_distance, max_allowed_trip_distance, service_charge, govt_charges, fare_policy_type, description) VALUES ('71b52524-e773-03dc-5853-290132ce6fd5', '22:00:00', '06:00:00', now(), now(), 0, 100000, null, null, 'Rental', 'Yatri Sathi SEDAN Rental');
INSERT INTO atlas_driver_offer_bpp.fare_policy (id, night_shift_start, night_shift_end, created_at, updated_at, min_allowed_trip_distance, max_allowed_trip_distance, service_charge, govt_charges, fare_policy_type, description) VALUES ('51b42524-e113-03dc-5453-290032ce6fd5', '22:00:00', '06:00:00', now(), now(), 0, 100000, null, null, 'Rental', 'Yatri Sathi SUV Rental');

INSERT INTO atlas_driver_offer_bpp.fare_product (id, merchant_id, fare_policy_id, vehicle_variant, area, flow) VALUES ('294abc7f-9cc9-e3t3-3c8b-7721c6f1809f', 'favorit0-0000-0000-0000-00000favorit', '71b52524-e773-03dc-5853-290132ce6fd5', 'SEDAN', 'Default', 'RENTAL');
INSERT INTO atlas_driver_offer_bpp.fare_product (id, merchant_id, fare_policy_id, vehicle_variant, area, flow) VALUES ('394abc7f-9cc9-e3t3-3c8b-7721c6f1809f', 'favorit0-0000-0000-0000-00000favorit', '51b42524-e113-03dc-5453-290032ce6fd5', 'SUV', 'Default', 'RENTAL');

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
  CONSTRAINT fare_policy_rental_details_distance_buffers_unique_ride_duration UNIQUE (fare_policy_id, ride_duration)
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
UPDATE atlas_driver_offer_bpp.transporter_config SET rental_requests = true where merchant_id = 'favorit0-0000-0000-0000-00000favorit' -- YS
