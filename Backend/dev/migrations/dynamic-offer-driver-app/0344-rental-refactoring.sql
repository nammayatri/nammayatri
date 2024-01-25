-- RUN THIS BEFORE --
CREATE OR REPLACE FUNCTION drop_not_null_if_exists(target_schema_name TEXT, target_table_name TEXT, target_column_name TEXT)
RETURNS void AS $$
BEGIN
    IF EXISTS (
        SELECT 1
        FROM information_schema.columns
        WHERE table_schema = target_schema_name
        AND table_name = target_table_name
        AND column_name = target_column_name
        AND is_nullable = 'NO'
    ) THEN
        EXECUTE format('ALTER TABLE %I.%I ALTER COLUMN %I DROP NOT NULL', target_schema_name, target_table_name, target_column_name);
    END IF;
END;
$$ LANGUAGE plpgsql;

-- BOOKING TABLE --
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN trip_category text;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN stop_location_id character varying(36);
-- atlas_driver_offer_bpp.drop_not_null_if_exists('atlas_driver_offer_bpp', 'booking', 'estimated_distance');
-- atlas_driver_offer_bpp.drop_not_null_if_exists('atlas_driver_offer_bpp', 'booking', 'estimated_duration');

-- DRIVER INFORMATION --
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN can_switch_to_rental boolean;

-- DRIVER QUOTE --
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN trip_category text;
-- atlas_driver_offer_bpp.drop_not_null_if_exists('atlas_driver_offer_bpp', 'driver_quote', 'distance');

-- ESTIMATE --
ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN trip_category text;
ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN estimated_distance integer;
ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN fare_params_id character varying(36);
ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN fare_policy_id character varying(36);
ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN updated_at TIMESTAMP WITH TIME ZONE;

-- FARE PARAMETERS --
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN updated_at TIMESTAMP WITH TIME ZONE;

-- FARE PARAMETERS RENTAL DETAILS --
CREATE TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ();
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN fare_parameters_id character varying(36);
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN time_based_fare numeric(30, 2);
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN dist_based_fare numeric(30, 2);

-- FARE POLICY RENTAL DETAILS --
CREATE TABLE atlas_driver_offer_bpp.fare_policy_rental_details ();
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN fare_policy_id character varying(36);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN base_fare numeric(30, 2);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN per_hour_charge numeric(30, 2);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN planned_per_km_rate numeric(30, 2);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN included_km_per_hour int NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN per_extra_km_rate numeric(30, 2);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN night_shift_charge json;

-- FARE POLICY RENTAL DETAILS DISTANCE BUFFERS --
CREATE TABLE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers ();
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers ADD COLUMN fare_policy_id character varying(36);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers ADD COLUMN ride_duration integer;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers ADD COLUMN buffer_kms integer;

-- FARE PRODUCT --
ALTER TABLE atlas_driver_offer_bpp.fare_product ADD COLUMN trip_category text;
UPDATE atlas_driver_offer_bpp.fare_product
SET trip_category = CASE
                        WHEN flow = 'NORMAL' THEN 'OneWay_OneWayOnDemandDynamicOffer'
                        WHEN flow = 'RIDE_OTP' THEN 'OneWay_OneWayRideOtp'
                        ELSE trip_category
                    END;
ALTER TABLE atlas_driver_offer_bpp.fare_product ALTER COLUMN trip_category SET NOT NULL;

-- TRANSPORTER CONFIG --
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN can_switch_to_rental boolean default true;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN consider_drivers_for_search boolean default true;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN schedule_ride_buffer_time integer default 600;

-- QUOTE SPECIAL ZONE --
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN trip_category text;
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN fare_policy_id character varying(36);
-- atlas_driver_offer_bpp.drop_not_null_if_exists("atlas_driver_offer_bpp", "quote_special_zone", "distance");
-- atlas_driver_offer_bpp.drop_not_null_if_exists("atlas_driver_offer_bpp", "quote_special_zone", "estimated_finish_time");

-- SEARCH REQUEST --
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN message_id character varying(36);
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN start_time TIMESTAMP WITH TIME ZONE;
-- atlas_driver_offer_bpp.drop_not_null_if_exists("atlas_driver_offer_bpp", "search_request", "estimated_distance");
-- atlas_driver_offer_bpp.drop_not_null_if_exists("atlas_driver_offer_bpp", "search_request", "estimated_duration");

-- SEARCH TRY --
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN trip_category text;

-- FARE POLICY --
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN max_additional_kms_limit integer;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN total_additional_kms_limit integer;

-- INSERTIONS ---

INSERT INTO atlas_driver_offer_bpp.fare_policy_rental_details (fare_policy_id, base_fare, per_hour_charge, per_extra_min_rate, per_extra_km_rate, included_km_per_hr, planned_per_km_rate, night_shift_charge) VALUES
('71b52524-e773-03dc-5853-290132ce6fd5', 0, 180, 5, 18, 10, 15, '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json),
('51b42524-e113-03dc-5453-290032ce6fd5', 0, 220, 6, 22, 10, 18, '{"contents":250,"tag":"ConstantNightShiftCharge"}' :: json);

INSERT INTO atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers (fare_policy_id, ride_duration, buffer_kms) VALUES
('71b52524-e773-03dc-5853-290132ce6fd5', 0, 4),
('71b52524-e773-03dc-5853-290132ce6fd5', 5, 8),
('51b42524-e113-03dc-5453-290032ce6fd5', 0, 4),
('51b42524-e113-03dc-5453-290032ce6fd5', 5, 8);

INSERT INTO atlas_driver_offer_bpp.fare_policy (id, night_shift_start, night_shift_end, created_at, updated_at, min_allowed_trip_distance, max_allowed_trip_distance, service_charge, govt_charges, fare_policy_type, max_additional_kms_limit, total_additional_kms_limit, description) VALUES ('71b52524-e773-03dc-5853-290132ce6fd5', '22:00:00', '06:00:00', now(), now(), 0, 100000, null, null, 'Rental', 50, 120, 'Yatri Sathi SEDAN Rental');
INSERT INTO atlas_driver_offer_bpp.fare_policy (id, night_shift_start, night_shift_end, created_at, updated_at, min_allowed_trip_distance, max_allowed_trip_distance, service_charge, govt_charges, fare_policy_type, max_additional_kms_limit, total_additional_kms_limit, description) VALUES ('51b42524-e113-03dc-5453-290032ce6fd5', '22:00:00', '06:00:00', now(), now(), 0, 100000, null, null, 'Rental', 50, 120, 'Yatri Sathi SUV Rental');

INSERT INTO atlas_driver_offer_bpp.fare_product (id, merchant_id, fare_policy_id, vehicle_variant, area, flow, merchant_operating_city_id) VALUES ('294abc7f-9cc9-e3t3-3c8b-7721c6f1809f', 'favorit0-0000-0000-0000-00000favorit', '71b52524-e773-03dc-5853-290132ce6fd5', 'SEDAN', 'Default', 'RENTAL',(SELECT id from atlas_driver_offer_bpp.merchant_operating_city where merchant_id = 'favorit0-0000-0000-0000-00000favorit'));
INSERT INTO atlas_driver_offer_bpp.fare_product (id, merchant_id, fare_policy_id, vehicle_variant, area, flow, merchant_operating_city_id) VALUES ('394abc7f-9cc9-e3t3-3c8b-7721c6f1809f', 'favorit0-0000-0000-0000-00000favorit', '51b42524-e113-03dc-5453-290032ce6fd5', 'SUV', 'Default', 'RENTAL',(SELECT id from atlas_driver_offer_bpp.merchant_operating_city where merchant_id = 'favorit0-0000-0000-0000-00000favorit'));

-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ --
-- @ WARNING: DO NOT ENTER BEFORE FULL RELEASE - DROP QUERY ZONE @ --
-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ --
ALTER TABLE atlas_driver_offer_bpp.estimate DROP COLUMN estimate_breakup_list;
ALTER TABLE atlas_driver_offer_bpp.estimate DROP COLUMN night_shift_charge;
ALTER TABLE atlas_driver_offer_bpp.estimate DROP COLUMN night_shift_multiplier;
ALTER TABLE atlas_driver_offer_bpp.estimate DROP COLUMN night_shift_start;
ALTER TABLE atlas_driver_offer_bpp.estimate DROP COLUMN night_shift_end;
ALTER TABLE atlas_driver_offer_bpp.estimate DROP COLUMN waiting_charge_per_min;
ALTER TABLE atlas_driver_offer_bpp.estimate DROP COLUMN waiting_or_pickup_charges;

ALTER TABLE atlas_driver_offer_bpp.fare_product DROP COLUMN flow;
