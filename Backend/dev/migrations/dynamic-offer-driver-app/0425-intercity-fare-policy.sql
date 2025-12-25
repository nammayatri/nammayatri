CREATE TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details();

ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN fare_parameters_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN time_fare numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN distance_fare numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN pickup_charge numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN currency character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN extra_distance_fare numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN extra_time_fare numeric(30, 2) NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD PRIMARY KEY (fare_parameters_id);

-- fare_policy_inter_city_details
CREATE TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details();

ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN fare_policy_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN base_fare numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN per_hour_charge numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN per_km_rate_one_way numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN per_km_rate_round_trip numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN per_extra_km_rate numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN per_extra_min_rate numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN km_per_planned_extra_hour int NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN dead_km_fare numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN per_day_max_hour_allowance int NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN default_wait_time_at_destination int NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN currency character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN night_shift_charge json;

ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN toll_charges numeric(30, 2);

-- inter city fare policies  (ALREADY RAN IN MASTER, PRODUCTION RUN ONCE UI IS RELEASED)
-- Non-AC min
insert into atlas_driver_offer_bpp.fare_policy
(id, night_shift_start, night_shift_end, created_at, updated_at, min_allowed_trip_distance, max_allowed_trip_distance, service_charge, govt_charges, fare_policy_type, description, per_minute_ride_extra_time_charge, congestion_charge_multiplier, parking_charge, currency, service_charge_amount, congestion_charge, toll_charges)
values ('86a96e7a-10a4-40bf-b9cd-5eb6cc5f40ae', '22:00:00', '06:00:00', now(), now(), 0, 1000000, null, null, 'InterCity', 'Inter City Fare Policy Bangalore (Non-AC mini)', null, null, null, 'INR', null, null, null);

insert into atlas_driver_offer_bpp.fare_policy_inter_city_details(fare_policy_id, base_fare, per_hour_charge, per_km_rate_one_way, per_km_rate_round_trip, per_extra_km_rate, per_extra_min_rate, km_per_planned_extra_hour, dead_km_fare, per_day_max_hour_allowance, default_wait_time_at_destination, currency, night_shift_charge)
values ('86a96e7a-10a4-40bf-b9cd-5eb6cc5f40ae', 0, 60, 9, 10.8, 9.5, 2.2, 10, 25, 14, 60, 'INR', '{"contents": 200, "tag": "ConstantNightShiftCharge"}');

-- AC mini
insert into atlas_driver_offer_bpp.fare_policy
(id, night_shift_start, night_shift_end, created_at, updated_at, min_allowed_trip_distance, max_allowed_trip_distance, service_charge, govt_charges, fare_policy_type, description, per_minute_ride_extra_time_charge, congestion_charge_multiplier, parking_charge, currency, service_charge_amount, congestion_charge, toll_charges)
values ('86a96e7a-10a4-40bf-b9cd-5eb6cc5f40af', '22:00:00', '06:00:00', now(), now(), 0, 1000000, null, null, 'InterCity', 'Inter City Fare Policy Bangalore (AC mini)', null, null, null, 'INR', null, null, null);

insert into atlas_driver_offer_bpp.fare_policy_inter_city_details(fare_policy_id, base_fare, per_hour_charge, per_km_rate_one_way, per_km_rate_round_trip, per_extra_km_rate, per_extra_min_rate, km_per_planned_extra_hour, dead_km_fare, per_day_max_hour_allowance, default_wait_time_at_destination, currency, night_shift_charge)
values ('86a96e7a-10a4-40bf-b9cd-5eb6cc5f40af', 0, 60, 10, 12, 10.5, 2.5, 10, 30, 14, 60, 'INR', '{"contents": 250, "tag": "ConstantNightShiftCharge"}');

-- Sedan
insert into atlas_driver_offer_bpp.fare_policy
(id, night_shift_start, night_shift_end, created_at, updated_at, min_allowed_trip_distance, max_allowed_trip_distance, service_charge, govt_charges, fare_policy_type, description, per_minute_ride_extra_time_charge, congestion_charge_multiplier, parking_charge, currency, service_charge_amount, congestion_charge, toll_charges)
values ('86a96e7a-10a4-40bf-b9cd-5eb6cc5f40ag', '22:00:00', '06:00:00', now(), now(), 0, 1000000, null, null, 'InterCity', 'Inter City Fare Policy Bangalore (Sedan)', null, null, null, 'INR', null, null, null);

insert into atlas_driver_offer_bpp.fare_policy_inter_city_details(fare_policy_id, base_fare, per_hour_charge, per_km_rate_one_way, per_km_rate_round_trip, per_extra_km_rate, per_extra_min_rate, km_per_planned_extra_hour, dead_km_fare, per_day_max_hour_allowance, default_wait_time_at_destination, currency, night_shift_charge)
values ('86a96e7a-10a4-40bf-b9cd-5eb6cc5f40ag', 0, 60, 11, 13.2, 11.5, 2.85, 10, 35, 14, 60, 'INR', '{"contents": 300, "tag": "ConstantNightShiftCharge"}');

-- SUV
insert into atlas_driver_offer_bpp.fare_policy
(id, night_shift_start, night_shift_end, created_at, updated_at, min_allowed_trip_distance, max_allowed_trip_distance, service_charge, govt_charges, fare_policy_type, description, per_minute_ride_extra_time_charge, congestion_charge_multiplier, parking_charge, currency, service_charge_amount, congestion_charge, toll_charges)
values ('86a96e7a-10a4-40bf-b9cd-5eb6cc5f40ah', '22:00:00', '06:00:00', now(), now(), 0, 1000000, null, null, 'InterCity', 'Inter City Fare Policy Bangalore (SUV)', null, null, null, 'INR', null, null, null);

insert into atlas_driver_offer_bpp.fare_policy_inter_city_details(fare_policy_id, base_fare, per_hour_charge, per_km_rate_one_way, per_km_rate_round_trip, per_extra_km_rate, per_extra_min_rate, km_per_planned_extra_hour, dead_km_fare, per_day_max_hour_allowance, default_wait_time_at_destination, currency, night_shift_charge)
values ('86a96e7a-10a4-40bf-b9cd-5eb6cc5f40ah', 0, 60, 15, 18, 15.5, 3.2, 10, 40, 14, 60, 'INR', '{"contents": 350, "tag": "ConstantNightShiftCharge"}');

--- Banglore city fare product (only for local and master) ---
INSERT INTO atlas_driver_offer_bpp.fare_product ( area,enabled,fare_policy_id,id,merchant_id,merchant_operating_city_id,time_bounds,trip_category,vehicle_variant)
SELECT 'Default', true, '86a96e7a-10a4-40bf-b9cd-5eb6cc5f40ae',atlas_driver_offer_bpp.uuid_generate_v4(),m.merchant_id,m.id,'Unbounded','InterCity_OneWayOnDemandStaticOffer','TAXI' FROM atlas_driver_offer_bpp.merchant_operating_city as m where m.city = 'Bangalore';

INSERT INTO atlas_driver_offer_bpp.fare_product ( area,enabled,fare_policy_id,id,merchant_id,merchant_operating_city_id,time_bounds,trip_category,vehicle_variant)
SELECT 'Default', true, '86a96e7a-10a4-40bf-b9cd-5eb6cc5f40af',atlas_driver_offer_bpp.uuid_generate_v4(),m.merchant_id,m.id,'Unbounded','InterCity_OneWayOnDemandStaticOffer','ECO' FROM atlas_driver_offer_bpp.merchant_operating_city as m where m.city = 'Bangalore';

INSERT INTO atlas_driver_offer_bpp.fare_product ( area,enabled,fare_policy_id,id,merchant_id,merchant_operating_city_id,time_bounds,trip_category,vehicle_variant)
SELECT 'Default', true, '86a96e7a-10a4-40bf-b9cd-5eb6cc5f40ag',atlas_driver_offer_bpp.uuid_generate_v4(),m.merchant_id,m.id,'Unbounded','InterCity_OneWayOnDemandStaticOffer','COMFY' FROM atlas_driver_offer_bpp.merchant_operating_city as m where m.city = 'Bangalore';

INSERT INTO atlas_driver_offer_bpp.fare_product ( area,enabled,fare_policy_id,id,merchant_id,merchant_operating_city_id,time_bounds,trip_category,vehicle_variant)
SELECT 'Default', true, '86a96e7a-10a4-40bf-b9cd-5eb6cc5f40ah',atlas_driver_offer_bpp.uuid_generate_v4(),m.merchant_id,m.id,'Unbounded','InterCity_OneWayOnDemandStaticOffer','SUV' FROM atlas_driver_offer_bpp.merchant_operating_city as m where m.city = 'Bangalore';


--- adding cross city fare policy for delhi to gurugram ---
INSERT INTO atlas_driver_offer_bpp.fare_product ( area,enabled,fare_policy_id,id,merchant_id,merchant_operating_city_id,time_bounds,trip_category,vehicle_variant)
SELECT area, true, fare_policy_id, md5(random()::text || clock_timestamp()::text)::uuid, merchant_id, merchant_operating_city_id, time_bounds, 'CrossCity_OneWayOnDemandDynamicOffer', vehicle_variant FROM atlas_driver_offer_bpp.fare_product where area = 'Default' and time_bounds = 'Unbounded' and trip_category = 'OneWay_OneWayOnDemandDynamicOffer' and enabled = true and merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Delhi');

INSERT INTO atlas_driver_offer_bpp.fare_product ( area,enabled,fare_policy_id,id,merchant_id,merchant_operating_city_id,time_bounds,trip_category,vehicle_variant)
SELECT area, true, fare_policy_id, md5(random()::text || clock_timestamp()::text)::uuid, merchant_id, merchant_operating_city_id, time_bounds, 'CrossCity_OneWayOnDemandDynamicOffer', vehicle_variant FROM atlas_driver_offer_bpp.fare_product where area = 'Default' and time_bounds = 'Unbounded' and trip_category = 'OneWay_OneWayOnDemandDynamicOffer' and enabled = true and merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Gurugram');

INSERT INTO atlas_driver_offer_bpp.fare_product ( area,enabled,fare_policy_id,id,merchant_id,merchant_operating_city_id,time_bounds,trip_category,vehicle_variant)
SELECT area, true, fare_policy_id, md5(random()::text || clock_timestamp()::text)::uuid, merchant_id, merchant_operating_city_id, time_bounds, 'CrossCity_OneWayOnDemandDynamicOffer', vehicle_variant FROM atlas_driver_offer_bpp.fare_product where area = 'Default' and time_bounds = 'Unbounded' and trip_category = 'OneWay_OneWayOnDemandDynamicOffer' and enabled = true and merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Noida');


update atlas_driver_offer_bpp.transporter_config as t
set cross_travel_cities = '{Delhi, Noida}' FROM atlas_driver_offer_bpp.merchant_operating_city as m where m.city = 'Gurugram' and t.merchant_operating_city_id = m.id;

update atlas_driver_offer_bpp.transporter_config as t
set cross_travel_cities = '{Gurugram, Noida}' FROM atlas_driver_offer_bpp.merchant_operating_city as m where m.city = 'Delhi' and t.merchant_operating_city_id = m.id;

update atlas_driver_offer_bpp.transporter_config as t
set cross_travel_cities = '{Gurugram, Delhi}' FROM atlas_driver_offer_bpp.merchant_operating_city as m where m.city = 'Noida' and t.merchant_operating_city_id = m.id;
