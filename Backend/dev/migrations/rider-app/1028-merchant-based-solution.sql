-- Dropping NOT NULL constraint from each column except short_id and id
ALTER TABLE atlas_app.merchant ALTER COLUMN aadhaar_key_expiry_time DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN aadhaar_verification_try_limit DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN arrived_pickup_threshold DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN bap_id DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN bap_unique_key_id DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN cipher_text DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN country DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN created_at DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN city DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN state DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN driver_distance_threshold_from_pickup DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN driver_offer_api_key DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN driver_offer_base_url DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN driver_offer_merchant_id DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN driver_on_the_way_notify_expiry DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN edit_pickup_distance_threshold DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN fake_otp_mobile_numbers DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN fallback_short_id DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN gateway_url DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN geo_hash_precision_value DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN destination_restriction DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN origin_restriction DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN is_avoid_toll DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN kapture_disposition DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN media_file_size_upper_limit DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN media_file_url_pattern DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN minimum_driver_rates_count DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN name DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN num_of_allowed_edit_pickup_location_attempts_threshold DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN public_media_file_url_pattern DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN registry_url DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN schedule_ride_buffer_time DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN signature_expiry DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN signing_public_key DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN subscriber_id DROP NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN updated_at DROP NOT NULL;


INSERT INTO atlas_app.merchant (id, short_id) VALUES
    ('da4e23a5-3ce6-4c37-8b9b-41377c3c1a51', 'YATRI'),
    ('da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'NAMMA_YATRI'),
    ('4b17bd06-ae7e-48e9-85bf-282fb310209c', 'NAMMA_YATRI2'); -- only for local testing


--ALTER TABLE atlas_app.ride_booking ADD COLUMN merchant_id character(36) NOT NULL REFERENCES atlas_app.merchant (id) DEFAULT 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51';
--

CREATE TABLE atlas_app.selected_quote (
id character(36) NOT NULL PRIMARY KEY,
fare_product_type text NOT NULL,
quote_id character(36) NOT NULL,
estimated_fare numeric(30,10) NOT NULL,
discount double precision,
estimated_total_fare numeric(30,2),
provider_id character varying(255) NOT NULL,
provider_url character varying(255) NOT NULL,
provider_name character varying(255) NOT NULL,
provider_mobile_number character varying(255) NOT NULL,
provider_completed_rides_count integer NOT NULL,
vehicle_variant character varying(60) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
trip_terms_id character(36),

driver_name character varying(255) NOT NULL,
distance_to_pickup double precision NOT NULL,
duration_to_pickup integer NOT NULL,
valid_till timestamp with time zone NOT NULL,
rating double precision,
bpp_quote_id character(36) NOT NULL
);
