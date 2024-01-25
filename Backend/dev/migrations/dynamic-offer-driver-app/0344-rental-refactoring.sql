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
drop_not_null_if_exists('atlas_driver_offer_bpp', 'booking', 'estimated_distance');
drop_not_null_if_exists('atlas_driver_offer_bpp', 'booking', 'estimated_duration');

-- DRIVER INFORMATION --
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN can_switch_to_rental;

-- DRIVER QUOTE --
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN trip_category text;
drop_not_null_if_exists('atlas_driver_offer_bpp', 'driver_quote', 'distance');

-- ESTIMATE --
ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN trip_category text;
ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN estimated_distance integer;
ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN fare_params_id character varying(36);
ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN fare_policy_id character varying(36);
ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN updated_at TIMESTAMP WITH TIME ZONE;

-- FARE PARAMETERS --
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN updated_at TIMESTAMP WITH TIME ZONE;

-- FARE PARAMETERS RENTAL DETAILS --
CREATE TABLE atlas_driver_offer_bppfare_parameters_rental_details ();
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN fare_parameters_id character varying(36);
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN time_based_fare numeric(30, 2);
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN extra_dist_fare numeric(30, 2);

-- FARE POLICY RENTAL DETAILS --
CREATE TABLE atlas_driver_offer_bpp.fare_policy_rental_details ();
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN fare_policy_id character varying(36);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN base_fare numeric(30, 2);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN per_hour_charge numeric(30, 2);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN per_hour_free_kms integer;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN per_extra_km_rate numeric(30, 2);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN night_shift_charge json;

-- FARE POLICY RENTAL DETAILS DISTANCE BUFFERS --
CREATE TABLE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers ();
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers ADD COLUMN fare_policy_id character varying(36);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers ADD COLUMN ride_duration integer;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers ADD COLUMN buffer_kms integer;

-- FARE PRODUCT --
ALTER TABLE atlas_driver_offer_bpp.fare_product ADD COLUMN trip_category text;
UPDATE fare_product
SET trip_category = CASE
                        WHEN flow_type = 'NORMAL' THEN 'OneWay_OneWayOnDemandDynamicOffer'
                        WHEN flow_type = 'RIDE_OTP' THEN 'OneWay_OneWayRideOtp'
                        ELSE trip_category
                    END;
ALTER TABLE atlas_driver_offer_bpp.fare_product ALTER COLUMN trip_category SET NOT NULL;

-- TRANSPORTER CONFIG --
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN can_switch_to_rental default true;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN consider_drivers_for_search default true;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN schedule_ride_buffer_time default 600;

-- QUOTE SPECIAL ZONE --
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN trip_category text;
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN fare_policy_id character varying(36);
drop_not_null_if_exists("atlas_driver_offer_bpp", "quote_special_zone", "distance");
drop_not_null_if_exists("atlas_driver_offer_bpp", "quote_special_zone", "estimated_finish_time");

-- SEARCH REQUEST --
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN message_id character varying(36);
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN start_time TIMESTAMP WITH TIME ZONE;
drop_not_null_if_exists("atlas_driver_offer_bpp", "search_request", "estimated_distance");
drop_not_null_if_exists("atlas_driver_offer_bpp", "search_request", "estimated_duration");

-- SEARCH TRY --
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN trip_category text;

-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ --
-- @ WARNING: DO NOT ENTER BEFORE FULL RELEASE - DROP QUERY ZONE @ --
-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ --
ALTER TABLE atlas_driver_offer_bpp.estimate DROP COLUMN estimate_breakup_list;
ALTER TABLE atlas_driver_offer_bpp.estimate DROP COLUMN night_shift_charge;
ALTER TABLE atlas_driver_offer_bpp.estimate DROP COLUMN old_night_shift_charge;
ALTER TABLE atlas_driver_offer_bpp.estimate DROP COLUMN night_shift_start;
ALTER TABLE atlas_driver_offer_bpp.estimate DROP COLUMN night_shift_end;
ALTER TABLE atlas_driver_offer_bpp.estimate DROP COLUMN waiting_charge_per_min;
ALTER TABLE atlas_driver_offer_bpp.estimate DROP COLUMN waiting_or_pickup_charges;

ALTER TABLE atlas_driver_offer_bpp.fare_product DROP COLUMN flow_type;
