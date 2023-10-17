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

-- ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ALTER COLUMN platform_fee_charge TYPE json; -- only for local

-- INSERT INTO atlas_driver_offer_bpp.fare_policy_slabs_details_slab (fare_policy_id, start_distance, base_fare, waiting_charge, night_shift_charge, free_wating_time, platform_fee_charge, platform_fee_cgst, platform_fee_sgst, max_duration, max_distance_buffer, per_extra_km_fare, per_extra_min_fare) VALUES
-- ('71b52524-e773-03dc-5853-290132ce6fd5', 0, 348, '{"contents":0,"tag":"ConstantWaitingCharge"}', '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json, 0, '{"contents":0,"tag":"ConstantPlatformFee"}' :: json, 0, 0, 60, 4000, 18, 3),
-- ('71b52524-e773-03dc-5853-290132ce6fd5', 10000, 670, '{"contents":0,"tag":"ConstantWaitingCharge"}', '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json, 0, '{"contents":0,"tag":"ConstantPlatformFee"}' :: json, 0, 0, 120, 4000, 18, 3),
-- ('71b52524-e773-03dc-5853-290132ce6fd5', 20000, 1044, '{"contents":0,"tag":"ConstantWaitingCharge"}', '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json, 0, '{"contents":0,"tag":"ConstantPlatformFee"}' :: json, 0, 0, 180, 4000, 18, 3),
-- ('71b52524-e773-03dc-5853-290132ce6fd5', 30000, 1310, '{"contents":0,"tag":"ConstantWaitingCharge"}', '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json, 0, '{"contents":0,"tag":"ConstantPlatformFee"}' :: json, 0, 0, 240, 4000, 18, 3),
-- ('71b52524-e773-03dc-5853-290132ce6fd5', 40000, 1711, '{"contents":0,"tag":"ConstantWaitingCharge"}', '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json, 0, '{"contents":0,"tag":"ConstantPlatformFee"}' :: json, 0, 0, 300, 4000, 18, 3),
-- ('71b52524-e773-03dc-5853-290132ce6fd5', 50000, 1909, '{"contents":0,"tag":"ConstantWaitingCharge"}', '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json, 0, '{"contents":0,"tag":"ConstantPlatformFee"}' :: json, 0, 0, 360, 8000, 18, 3),
-- ('71b52524-e773-03dc-5853-290132ce6fd5', 60000, 2270, '{"contents":0,"tag":"ConstantWaitingCharge"}', '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json, 0, '{"contents":0,"tag":"ConstantPlatformFee"}' :: json, 0, 0, 420, 8000, 18, 3),
-- ('71b52524-e773-03dc-5853-290132ce6fd5', 70000, 2545, '{"contents":0,"tag":"ConstantWaitingCharge"}', '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json, 0, '{"contents":0,"tag":"ConstantPlatformFee"}' :: json, 0, 0, 480, 8000, 18, 3),
-- ('71b52524-e773-03dc-5853-290132ce6fd5', 80000, 2800, '{"contents":0,"tag":"ConstantWaitingCharge"}', '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json, 0, '{"contents":0,"tag":"ConstantPlatformFee"}' :: json, 0, 0, 540, 8000, 18, 3),
-- ('71b52524-e773-03dc-5853-290132ce6fd5', 90000, 3181, '{"contents":0,"tag":"ConstantWaitingCharge"}', '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json, 0, '{"contents":0,"tag":"ConstantPlatformFee"}' :: json, 0, 0, 600, 8000, 18, 3),
-- ('71b52524-e773-03dc-5853-290132ce6fd5', 100000, 3450, '{"contents":0,"tag":"ConstantWaitingCharge"}', '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json, 0, '{"contents":0,"tag":"ConstantPlatformFee"}' :: json, 0, 0, 660, 8000, 18, 3);

-- INSERT INTO atlas_driver_offer_bpp.fare_policy_slabs_details_slab (fare_policy_id, start_distance, base_fare, waiting_charge, night_shift_charge, free_wating_time, platform_fee_charge, platform_fee_cgst, platform_fee_sgst, max_duration, max_distance_buffer, per_extra_km_fare, per_extra_min_fare) VALUES
-- ('51b42524-e113-03dc-5453-290032ce6fd5', 0, 492, '{"contents":0,"tag":"ConstantWaitingCharge"}', '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json, 0, 0, '{"contents":0,"tag":"ConstantPlatformFee"}' :: json, 0, 60, 4000, 18, 3),
-- ('51b42524-e113-03dc-5453-290032ce6fd5', 10000, 846, '{"contents":0,"tag":"ConstantWaitingCharge"}', '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json, 0, '{"contents":0,"tag":"ConstantPlatformFee"}' :: json, 0, 0, 120, 4000, 18, 3),
-- ('51b42524-e113-03dc-5453-290032ce6fd5', 20000, 1044, '{"contents":0,"tag":"ConstantWaitingCharge"}', '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json, 0, '{"contents":0,"tag":"ConstantPlatformFee"}' :: json, 0, 0, 180, 4000, 18, 3),
-- ('51b42524-e113-03dc-5453-290032ce6fd5', 30000, 1387, '{"contents":0,"tag":"ConstantWaitingCharge"}', '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json, 0, '{"contents":0,"tag":"ConstantPlatformFee"}' :: json, 0, 0, 240, 4000, 18, 3),
-- ('51b42524-e113-03dc-5453-290032ce6fd5', 40000, 1710, '{"contents":0,"tag":"ConstantWaitingCharge"}', '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json, 0, '{"contents":0,"tag":"ConstantPlatformFee"}' :: json, 0, 0, 300, 4000, 18, 3),
-- ('51b42524-e113-03dc-5453-290032ce6fd5', 50000, 2090, '{"contents":0,"tag":"ConstantWaitingCharge"}', '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json, 0, '{"contents":0,"tag":"ConstantPlatformFee"}' :: json, 0, 0, 360, 8000, 18, 3),
-- ('51b42524-e113-03dc-5453-290032ce6fd5', 60000, 2411, '{"contents":0,"tag":"ConstantWaitingCharge"}', '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json, 0, '{"contents":0,"tag":"ConstantPlatformFee"}' :: json, 0, 0, 420, 8000, 18, 3),
-- ('51b42524-e113-03dc-5453-290032ce6fd5', 70000, 2813, '{"contents":0,"tag":"ConstantWaitingCharge"}', '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json, 0, '{"contents":0,"tag":"ConstantPlatformFee"}' :: json, 0, 0, 480, 8000, 18, 3),
-- ('51b42524-e113-03dc-5453-290032ce6fd5', 80000, 3215, '{"contents":0,"tag":"ConstantWaitingCharge"}', '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json, 0, '{"contents":0,"tag":"ConstantPlatformFee"}' :: json, 0, 0, 540, 8000, 18, 3),
-- ('51b42524-e113-03dc-5453-290032ce6fd5', 90000, 3570, '{"contents":0,"tag":"ConstantWaitingCharge"}', '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json, 0, '{"contents":0,"tag":"ConstantPlatformFee"}' :: json, 0, 0, 600, 8000, 18, 3),
-- ('51b42524-e113-03dc-5453-290032ce6fd5', 100000, 3959, '{"contents":0,"tag":"ConstantWaitingCharge"}', '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json, 0, '{"contents":0,"tag":"ConstantPlatformFee"}' :: json, 0, 0, 660, 8000, 18, 3);

-- -- Merchant should be YS
-- INSERT INTO atlas_driver_offer_bpp.fare_policy_slabs_details_slab (id, merchant_id, vehicle_variant, night_shift_start, night_shift_end, night_shift_rate, min_allowed_trip_distance, max_allowed_trip_distance, service_charge, fare_slabs, created_at, updated_at) VALUES (388, 'favorit0-0000-0000-0000-00000favorit', 'SEDAN', , '0000-01-01T22:00:00Z', '0000-01-01T06:00:00Z' , 1, 2, 38000, 0, Array ['Slab { startMeters = 0, endMeters = 10000, fare = 348, maxDuration = 60, maxDistanceBuffer = 4000}', 'Slab { startMeters = 10000, endMeters = 20000, fare = 670, maxDuration = 120, maxDistanceBuffer = 4000}', 'Slab { startMeters = 20000, endMeters = 30000, fare = 1044, maxDuration = 180, maxDistanceBuffer = 4000}', 'Slab { startMeters = 30000, endMeters = 40000, fare = 1310, maxDuration = 240, maxDistanceBuffer = 4000}', 'Slab { startMeters = 40000, endMeters = 50000, fare = 1711, maxDuration = 300, maxDistanceBuffer = 4000}','Slab { startMeters = 50000, endMeters = 60000, fare = 1909, maxDuration = 360, maxDistanceBuffer = 8000}','Slab { startMeters = 60000, endMeters = 70000, fare = 2270, maxDuration = 420, maxDistanceBuffer = 8000}', 'Slab { startMeters = 70000, endMeters = 80000, fare = 2545, maxDuration = 490, maxDistanceBuffer = 8000}', 'Slab { startMeters = 80000, endMeters = 90000, fare = 2800, maxDuration = 560, maxDistanceBuffer = 8000}', 'Slab { startMeters = 90000, endMeters = 100000, fare = 3181, maxDuration = 630, maxDistanceBuffer = 8000}'], now(), now());

-- INSERT INTO atlas_driver_offer_bpp.fare_policy_slabs_details_slab (id, merchant_id, vehicle_variant, night_shift_start, night_shift_end, night_shift_rate, min_allowed_trip_distance, max_allowed_trip_distance, service_charge, fare_slabs, created_at, updated_at) VALUES ('q7j77f37-90e5-4g65-8fd6-4b62af35640a', 'favorit0-0000-0000-0000-00000favorit', 'SUV', , '0000-01-01T22:00:00Z', '0000-01-01T06:00:00Z' , 1, 2, 38000, 0, Array ['Slab { startMeters = 0, endMeters = 10000, fare = 492, maxDuration = 60, maxDistanceBuffer = 4000}', 'Slab { startMeters = 10000, endMeters = 20000, fare = 846, maxDuration = 120, maxDistanceBuffer = 4000}', 'Slab { startMeters = 20000, endMeters = 30000, fare = 1187, maxDuration = 180, maxDistanceBuffer = 4000}', 'Slab { startMeters = 30000, endMeters = 40000, fare = 1710, maxDuration = 240, maxDistanceBuffer = 4000}', 'Slab { startMeters = 40000, endMeters = 50000, fare = 2090, maxDuration = 300, maxDistanceBuffer = 4000}','Slab { startMeters = 50000, endMeters = 60000, fare = 2411, maxDuration = 360, maxDistanceBuffer = 8000}','Slab { startMeters = 60000, endMeters = 70000, fare = 2813, maxDuration = 420, maxDistanceBuffer = 8000}', 'Slab { startMeters = 70000, endMeters = 80000, fare = 3215, maxDuration = 490, maxDistanceBuffer = 8000}', 'Slab { startMeters = 80000, endMeters = 90000, fare = 3570, maxDuration = 560, maxDistanceBuffer = 8000}', 'Slab { startMeters = 90000, endMeters = 100000, fare = 3959, maxDuration = 630, maxDistanceBuffer = 8000}'], now(), now());

