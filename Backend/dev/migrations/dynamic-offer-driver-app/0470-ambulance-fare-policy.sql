-- fare_parameters_ambulance_details
CREATE TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details();

ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD COLUMN fare_parameters_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD COLUMN dist_based_fare numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD COLUMN platform_fee numeric(30, 2);
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD COLUMN cgst numeric(30, 2);
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD COLUMN sgst numeric(30, 2);
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD COLUMN currency character varying(255) NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD PRIMARY KEY (fare_parameters_id);

-- fare_policy_ambulance_details_slab
CREATE TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab();

ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN id serial PRIMARY KEY;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN fare_policy_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN vehicle_age int NOT NULL; -- months(should we still take numeric?)
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN base_fare numeric(30, 2) NOT NULL; -- value?
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN per_km_rate numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN currency character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN night_shift_charge json;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN waiting_charge json;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN free_waiting_time integer;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN platform_fee_charge json;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN platform_fee_cgst double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN platform_fee_sgst double precision;

-- Ambulance fare policies
-- AMBULANCE_TAXI
insert into atlas_driver_offer_bpp.fare_policy
(id, night_shift_start, night_shift_end, created_at, updated_at, min_allowed_trip_distance, max_allowed_trip_distance, service_charge, govt_charges, fare_policy_type, description, per_minute_ride_extra_time_charge, congestion_charge_multiplier, parking_charge, currency, service_charge_amount, congestion_charge, toll_charges)
values (atlas_driver_offer_bpp.uuid_generate_v4(), '22:00:00', '05:00:00', now(), now(), 0, 100000, null, null, 'Ambulance', 'Ambulance Fare Policy Kolkata (Ambulance Eco)', null, null, null, 'INR', null, null, null);

insert into atlas_driver_offer_bpp.fare_policy_ambulance_details_slab(fare_policy_id, vehicle_age, base_fare, per_km_rate, currency, night_shift_charge, waiting_charge, free_waiting_time)
values
((select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance Eco)'), 0, 0, 39.24, 'INR', '{"contents": 1.15, "tag": "ProgressiveNightShiftCharge"}', '{"contents":2,"tag":"PerMinuteWaitingCharge"}', 8),
((select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance Eco)'), 60, 0, 29.43, 'INR', '{"contents": 1.15, "tag": "ProgressiveNightShiftCharge"}', '{"contents":2,"tag":"PerMinuteWaitingCharge"}', 8),
((select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance Eco)'), 120, 0, 28.53, 'INR', '{"contents": 1.15, "tag": "ProgressiveNightShiftCharge"}', '{"contents":2,"tag":"PerMinuteWaitingCharge"}', 8);

-- AMBULANCE_TAXI_OXY
insert into atlas_driver_offer_bpp.fare_policy
(id, night_shift_start, night_shift_end, created_at, updated_at, min_allowed_trip_distance, max_allowed_trip_distance, service_charge, govt_charges, fare_policy_type, description, per_minute_ride_extra_time_charge, congestion_charge_multiplier, parking_charge, currency, service_charge_amount, congestion_charge, toll_charges)
values (atlas_driver_offer_bpp.uuid_generate_v4(), '22:00:00', '05:00:00', now(), now(), 0, 100000, null, null, 'Ambulance', 'Ambulance Fare Policy Kolkata (Ambulance Eco Oxy)', null, null, null, 'INR', null, null, null);

insert into atlas_driver_offer_bpp.fare_policy_ambulance_details_slab(fare_policy_id, vehicle_age, base_fare, per_km_rate, currency, night_shift_charge, waiting_charge, free_waiting_time)
values
((select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance Eco Oxy)'), 0, 0, 47.25, 'INR', '{"contents": 1.15, "tag": "ProgressiveNightShiftCharge"}', '{"contents":2,"tag":"PerMinuteWaitingCharge"}', 8),
((select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance Eco Oxy)'), 60, 0, 32.49, 'INR', '{"contents": 1.15, "tag": "ProgressiveNightShiftCharge"}', '{"contents":2,"tag":"PerMinuteWaitingCharge"}', 8),
((select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance Eco Oxy)'), 120, 0, 31.14, 'INR', '{"contents": 1.15, "tag": "ProgressiveNightShiftCharge"}', '{"contents":2,"tag":"PerMinuteWaitingCharge"}', 8);

-- AMBULANCE_AC
insert into atlas_driver_offer_bpp.fare_policy
(id, night_shift_start, night_shift_end, created_at, updated_at, min_allowed_trip_distance, max_allowed_trip_distance, service_charge, govt_charges, fare_policy_type, description, per_minute_ride_extra_time_charge, congestion_charge_multiplier, parking_charge, currency, service_charge_amount, congestion_charge, toll_charges)
values (atlas_driver_offer_bpp.uuid_generate_v4(), '22:00:00', '05:00:00', now(), now(), 0, 100000, null, null, 'Ambulance', 'Ambulance Fare Policy Kolkata (Ambulance AC)', null, null, null, 'INR', null, null, null);

insert into atlas_driver_offer_bpp.fare_policy_ambulance_details_slab(fare_policy_id, vehicle_age, base_fare, per_km_rate, currency, night_shift_charge, waiting_charge, free_waiting_time)
values
((select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance AC)'), 0, 0, 43.6, 'INR', '{"contents": 1.15, "tag": "ProgressiveNightShiftCharge"}', '{"contents":2,"tag":"PerMinuteWaitingCharge"}', 8),
((select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance AC)'), 60, 0, 32.7, 'INR', '{"contents": 1.15, "tag": "ProgressiveNightShiftCharge"}', '{"contents":2,"tag":"PerMinuteWaitingCharge"}', 8),
((select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance AC)'), 120, 0, 31.7, 'INR', '{"contents": 1.15, "tag": "ProgressiveNightShiftCharge"}', '{"contents":2,"tag":"PerMinuteWaitingCharge"}', 8);

-- AMBULANCE_AC_OXY
insert into atlas_driver_offer_bpp.fare_policy
(id, night_shift_start, night_shift_end, created_at, updated_at, min_allowed_trip_distance, max_allowed_trip_distance, service_charge, govt_charges, fare_policy_type, description, per_minute_ride_extra_time_charge, congestion_charge_multiplier, parking_charge, currency, service_charge_amount, congestion_charge, toll_charges)
values (atlas_driver_offer_bpp.uuid_generate_v4(), '22:00:00', '05:00:00', now(), now(), 0, 100000, null, null, 'Ambulance', 'Ambulance Fare Policy Kolkata (Ambulance AC Oxy)', null, null, null, 'INR', null, null, null);

insert into atlas_driver_offer_bpp.fare_policy_ambulance_details_slab(fare_policy_id, vehicle_age, base_fare, per_km_rate, currency, night_shift_charge, waiting_charge, free_waiting_time)
values
((select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance AC Oxy)'), 0, 0, 52.5, 'INR', '{"contents": 1.15, "tag": "ProgressiveNightShiftCharge"}', '{"contents":2,"tag":"PerMinuteWaitingCharge"}', 8),
((select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance AC Oxy)'), 60, 0, 36.1, 'INR', '{"contents": 1.15, "tag": "ProgressiveNightShiftCharge"}', '{"contents":2,"tag":"PerMinuteWaitingCharge"}', 8),
((select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance AC Oxy)'), 120, 0, 34.6, 'INR', '{"contents": 1.15, "tag": "ProgressiveNightShiftCharge"}', '{"contents":2,"tag":"PerMinuteWaitingCharge"}', 8);

-- AMBULANCE_VENTILATOR
insert into atlas_driver_offer_bpp.fare_policy
(id, night_shift_start, night_shift_end, created_at, updated_at, min_allowed_trip_distance, max_allowed_trip_distance, service_charge, govt_charges, fare_policy_type, description, per_minute_ride_extra_time_charge, congestion_charge_multiplier, parking_charge, currency, service_charge_amount, congestion_charge, toll_charges)
values (atlas_driver_offer_bpp.uuid_generate_v4(), '22:00:00', '05:00:00', now(), now(), 0, 100000, null, null, 'Ambulance', 'Ambulance Fare Policy Kolkata (Ambulance Ventilator)', null, null, null, 'INR', null, null, null);

insert into atlas_driver_offer_bpp.fare_policy_ambulance_details_slab(fare_policy_id, vehicle_age, base_fare, per_km_rate, currency, night_shift_charge, waiting_charge, free_waiting_time)
values
((select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance Ventilator)'), 0, 0, 67.4, 'INR', '{"contents": 1.15, "tag": "ProgressiveNightShiftCharge"}', '{"contents":2,"tag":"PerMinuteWaitingCharge"}', 8),
((select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance Ventilator)'), 60, 0, 41.7, 'INR', '{"contents": 1.15, "tag": "ProgressiveNightShiftCharge"}', '{"contents":2,"tag":"PerMinuteWaitingCharge"}', 8),
((select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance Ventilator)'), 120, 0, 39.4, 'INR', '{"contents": 1.15, "tag": "ProgressiveNightShiftCharge"}', '{"contents":2,"tag":"PerMinuteWaitingCharge"}', 8);

--- Fare products ---
INSERT INTO atlas_driver_offer_bpp.fare_product (area,enabled,fare_policy_id,id,merchant_id,merchant_operating_city_id,time_bounds,trip_category,vehicle_variant)
SELECT 'Default', true, (select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance Eco)'), atlas_driver_offer_bpp.uuid_generate_v4(), m.merchant_id,m.id,'Unbounded','Ambulance_OneWayOnDemandDynamicOffer','AMBULANCE_TAXI' FROM atlas_driver_offer_bpp.merchant_operating_city as m where m.city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.fare_product (area,enabled,fare_policy_id,id,merchant_id,merchant_operating_city_id,time_bounds,trip_category,vehicle_variant)
SELECT 'Default', true, (select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance Eco Oxy)'), atlas_driver_offer_bpp.uuid_generate_v4(), m.merchant_id,m.id,'Unbounded','Ambulance_OneWayOnDemandDynamicOffer','AMBULANCE_TAXI_OXY' FROM atlas_driver_offer_bpp.merchant_operating_city as m where m.city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.fare_product (area,enabled,fare_policy_id,id,merchant_id,merchant_operating_city_id,time_bounds,trip_category,vehicle_variant)
SELECT 'Default', true, (select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance AC)'), atlas_driver_offer_bpp.uuid_generate_v4(), m.merchant_id,m.id,'Unbounded','Ambulance_OneWayOnDemandDynamicOffer','AMBULANCE_AC' FROM atlas_driver_offer_bpp.merchant_operating_city as m where m.city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.fare_product (area,enabled,fare_policy_id,id,merchant_id,merchant_operating_city_id,time_bounds,trip_category,vehicle_variant)
SELECT 'Default', true, (select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance AC Oxy)'), atlas_driver_offer_bpp.uuid_generate_v4(), m.merchant_id,m.id,'Unbounded','Ambulance_OneWayOnDemandDynamicOffer','AMBULANCE_AC_OXY' FROM atlas_driver_offer_bpp.merchant_operating_city as m where m.city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.fare_product (area,enabled,fare_policy_id,id,merchant_id,merchant_operating_city_id,time_bounds,trip_category,vehicle_variant)
SELECT 'Default', true, (select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance Ventilator)'), atlas_driver_offer_bpp.uuid_generate_v4(), m.merchant_id,m.id,'Unbounded','Ambulance_OneWayOnDemandDynamicOffer','AMBULANCE_VENTILATOR' FROM atlas_driver_offer_bpp.merchant_operating_city as m where m.city = 'Kolkata';

alter table atlas_driver_offer_bpp.quote_special_zone add column min_estimated_fare double precision;
alter table atlas_driver_offer_bpp.quote_special_zone add column max_estimated_fare double precision;


-- ONLY LOCAL --

INSERT INTO atlas_driver_offer_bpp.fare_product (area,enabled,fare_policy_id,id,merchant_id,merchant_operating_city_id,time_bounds,trip_category,vehicle_variant)
VALUES ('Default', true, (select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance Eco)'), atlas_driver_offer_bpp.uuid_generate_v4(), 'favorit0-0000-0000-0000-00000favorit','favorit0-0000-0000-0000-00000000city','Unbounded','Ambulance_OneWayOnDemandDynamicOffer','AMBULANCE_TAXI');

INSERT INTO atlas_driver_offer_bpp.fare_product (area,enabled,fare_policy_id,id,merchant_id,merchant_operating_city_id,time_bounds,trip_category,vehicle_variant)
VALUES ('Default', true, (select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance Eco Oxy)'), atlas_driver_offer_bpp.uuid_generate_v4(), 'favorit0-0000-0000-0000-00000favorit','favorit0-0000-0000-0000-00000000city','Unbounded','Ambulance_OneWayOnDemandDynamicOffer','AMBULANCE_TAXI_OXY');

INSERT INTO atlas_driver_offer_bpp.fare_product (area,enabled,fare_policy_id,id,merchant_id,merchant_operating_city_id,time_bounds,trip_category,vehicle_variant)
VALUES ('Default', true, (select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance AC)'), atlas_driver_offer_bpp.uuid_generate_v4(), 'favorit0-0000-0000-0000-00000favorit','favorit0-0000-0000-0000-00000000city','Unbounded','Ambulance_OneWayOnDemandDynamicOffer','AMBULANCE_AC');

INSERT INTO atlas_driver_offer_bpp.fare_product (area,enabled,fare_policy_id,id,merchant_id,merchant_operating_city_id,time_bounds,trip_category,vehicle_variant)
VALUES ('Default', true, (select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance AC Oxy)'), atlas_driver_offer_bpp.uuid_generate_v4(), 'favorit0-0000-0000-0000-00000favorit','favorit0-0000-0000-0000-00000000city','Unbounded','Ambulance_OneWayOnDemandDynamicOffer','AMBULANCE_AC_OXY');

INSERT INTO atlas_driver_offer_bpp.fare_product (area,enabled,fare_policy_id,id,merchant_id,merchant_operating_city_id,time_bounds,trip_category,vehicle_variant)
VALUES ('Default', true, (select id from atlas_driver_offer_bpp.fare_policy where description = 'Ambulance Fare Policy Kolkata (Ambulance Ventilator)'), atlas_driver_offer_bpp.uuid_generate_v4(), 'favorit0-0000-0000-0000-00000favorit','favorit0-0000-0000-0000-00000000city','Unbounded','Ambulance_OneWayOnDemandDynamicOffer','AMBULANCE_VENTILATOR');

INSERT INTO atlas_driver_offer_bpp.driver_pool_config (
    actual_distance_threshold,
    created_at,
    distance_based_batch_split,
    driver_batch_size,
    driver_position_info_expiry,
    driver_quote_limit,
    driver_request_count_limit,
    driver_to_destination_distance_threshold,
    driver_to_destination_duration,
    id,
    max_driver_quotes_required,
    max_number_of_batches,
    max_parallel_search_requests,
    max_radius_of_search,
    merchant_id,
    merchant_operating_city_id,
    min_radius_of_search,
    pool_sorting_type,
    radius_shrink_value_for_drivers_on_ride,
    radius_step_size,
    schedule_try_times,
    single_batch_process_time,
    threshold_to_ignore_actual_distance_threshold,
    trip_category,
    trip_distance,
    updated_at,
    vehicle_variant,
    area,
    time_bounds
)
SELECT
    2000,
    now(),
    '{"BatchSplitByPickupDistance {batchSplitSize = 4, batchSplitDelay = 0 }","BatchSplitByPickupDistance { batchSplitSize = 4, batchSplitDelay = 3 }"}',
    12,
    null,
    2,
    2,
    300,
    10,
    atlas_driver_offer_bpp.uuid_generate_v4(),
    1,
    6,
    3,
    7000,
    m.merchant_id,
    m.id,
    4000,
    'Random',
    300,
    1000,
    '{1800,900,300}',
    25,
    null,
    'Ambulance_OneWayOnDemandDynamicOffer',
    100000,
    now(),
    null,
    'Default',
    'Unbounded'
FROM
    atlas_driver_offer_bpp.merchant_operating_city m
WHERE
    m.merchant_short_id = 'NAMMA_YATRI_PARTNER';


-- Corresponding query to be ran in master and prod
INSERT INTO atlas_driver_offer_bpp.beckn_config(
	buyer_finder_fee, collected_by, domain, gateway_url, id, payment_params_json, registry_url, settlement_type, settlement_window, static_terms_url, subscriber_id, subscriber_url, unique_key_id, vehicle_category, merchant_id, merchant_operating_city_id, created_at, updated_at, on_select_ttl_sec, on_search_ttl_sec, on_init_ttl_sec, on_confirm_ttl_sec, on_track_ttl_sec, on_status_ttl_sec, on_cancel_ttl_sec, on_update_ttl_sec)
	VALUES (null, 'BPP', 'MOBILITY', 'http://localhost:8015/v1', 'dd22a05d-29a3-42c8-9c8d-2de340f9b607', '{"bankAccNumber": "xyz@upi","bankCode": "xyz"}', 'http://localhost:8020', null, null, null, 'NAMMA_YATRI', 'http://localhost:8013/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 100, 'AMBULANCE', 'favorit0-0000-0000-0000-00000favorit', null, now(), now(), 120, 120, 120, 120, 120, 120, 120, 120);

-- LOCAL ONLY ----

insert into atlas_driver_offer_bpp.vehicle_service_tier (id, name, merchant_id, merchant_operating_city_id, seating_capacity, air_conditioned, driver_rating, vehicle_rating, luggage_capacity, short_description, long_description, allowed_vehicle_variant, default_for_vehicle_variant, service_tier_type, created_at, updated_at, auto_selected_vehicle_variant, oxygen, ventilator)
VALUES (
    atlas_driver_offer_bpp.uuid_generate_v4(),
    'Ambulance with Ventilator',
    'favorit0-0000-0000-0000-00000favorit',
    'favorit0-0000-0000-0000-00000000city',
    null,
    1,
    null,
    null,
    null,
    'Ambulance with ventilator',
    null,
    '{AMBULANCE_VENTILATOR}',
    '{AMBULANCE_VENTILATOR}',
    'AMBULANCE_VENTILATOR',
    now(),
    now(),
    '{AMBULANCE_VENTILATOR}',
    1,
    1),

    (atlas_driver_offer_bpp.uuid_generate_v4(),
    'Ambulance with AC and Oxygen',
   'favorit0-0000-0000-0000-00000favorit',
   'favorit0-0000-0000-0000-00000000city',
    null,
    1,
    null,
    null,
    null,
    'Ambulance with AC and O2',
    null,
    '{AMBULANCE_AC_OXY, AMBULANCE_VENTILATOR}',
    '{AMBULANCE_AC_OXY}',
    'AMBULANCE_AC_OXY',
    now(),
    now(),
    '{AMBULANCE_AC_OXY}',
    1,
    null),

    (atlas_driver_offer_bpp.uuid_generate_v4(),
    'Ambulance with AC',
   'favorit0-0000-0000-0000-00000favorit',
   'favorit0-0000-0000-0000-00000000city',
    null,
    1,
    null,
    null,
    null,
    'Ambulance',
    null,
    '{AMBULANCE_AC, AMBULANCE_AC_OXY, AMBULANCE_VENTILATOR}',
    '{AMBULANCE_AC}',
    'AMBULANCE_AC',
    now(),
    now(),
    '{AMBULANCE_AC}',
    null,
    null),

    (atlas_driver_offer_bpp.uuid_generate_v4(),
    'Ambulance Taxi with Oxygen',
   'favorit0-0000-0000-0000-00000favorit',
   'favorit0-0000-0000-0000-00000000city',
    null,
    null,
    null,
    null,
    null,
    'Ambulance Eco with O2',
    null,
    '{AMBULANCE_TAXI_OXY, AMBULANCE_AC, AMBULANCE_AC_OXY, AMBULANCE_VENTILATOR}',
    '{AMBULANCE_TAXI_OXY}',
    'AMBULANCE_TAXI_OXY',
    now(),
    now(),
    '{AMBULANCE_TAXI_OXY}',
    1,
    null),

    (atlas_driver_offer_bpp.uuid_generate_v4(),
    'Ambulance Eco',
   'favorit0-0000-0000-0000-00000favorit',
   'favorit0-0000-0000-0000-00000000city',
    null,
    null,
    null,
    null,
    null,
    'Ambulance Taxi',
    null,
    '{AMBULANCE_TAXI, AMBULANCE_TAXI_OXY, AMBULANCE_AC, AMBULANCE_AC_OXY, AMBULANCE_VENTILATOR}',
    '{AMBULANCE_TAXI}',
    'AMBULANCE_TAXI',
    now(),
    now(),
    '{AMBULANCE_TAXI}',
    null,
    null);
