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
-- atlas_driver_offer_bpp.drop_not_null_if_exists('atlas_driver_offer_bpp', 'booking', 'estimated_distance');
-- atlas_driver_offer_bpp.drop_not_null_if_exists('atlas_driver_offer_bpp', 'booking', 'estimated_duration');

-- DRIVER QUOTE --
-- atlas_driver_offer_bpp.drop_not_null_if_exists('atlas_driver_offer_bpp', 'driver_quote', 'distance');

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
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN included_km_per_hr int NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN per_extra_min_rate numeric(30, 2);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN per_extra_km_rate numeric(30, 2);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN night_shift_charge json;

-- FARE POLICY RENTAL DETAILS DISTANCE BUFFERS --
CREATE TABLE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers ();
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers ADD COLUMN fare_policy_id character varying(36);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers ADD COLUMN ride_duration integer;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers ADD COLUMN buffer_kms integer;

-- FARE PRODUCT --

ALTER TABLE atlas_driver_offer_bpp.fare_product ALTER COLUMN trip_category SET NOT NULL;

-- QUOTE SPECIAL ZONE --
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN trip_category text;
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN fare_policy_id character varying(36);
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN is_scheduled boolean;
-- atlas_driver_offer_bpp.drop_not_null_if_exists("atlas_driver_offer_bpp", "quote_special_zone", "distance");
-- atlas_driver_offer_bpp.drop_not_null_if_exists("atlas_driver_offer_bpp", "quote_special_zone", "estimated_finish_time");

-- atlas_driver_offer_bpp.drop_not_null_if_exists("atlas_driver_offer_bpp", "search_request", "estimated_distance");
-- atlas_driver_offer_bpp.drop_not_null_if_exists("atlas_driver_offer_bpp", "search_request", "estimated_duration");

-- SEARCH TRY --
-- FARE POLICY --
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN max_additional_kms_limit integer;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN total_additional_kms_limit integer;

-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ --
-- @ WARNING: DO NOT ENTER BEFORE FULL RELEASE - DROP QUERY ZONE @ --
-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ --
