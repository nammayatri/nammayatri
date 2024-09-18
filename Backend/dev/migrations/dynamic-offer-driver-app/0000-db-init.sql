CREATE TABLE atlas_driver_offer_bpp.driver_location (
driver_id character(36) NOT NULL,
lat double precision NOT NULL,
lon double precision NOT NULL,
point public.geography(Point,4326) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  driver_location_pkey PRIMARY KEY (driver_id)
,CONSTRAINT  driver_location_driver_id_fkey FOREIGN KEY (driver_id) REFERENCES atlas_driver_offer_bpp.person(id)
);
ALTER TABLE atlas_driver_offer_bpp.driver_location OWNER TO atlas_driver_offer_bpp_user;

-- fare policy
CREATE TABLE atlas_driver_offer_bpp.fare_policy (
id character(36) NOT NULL,
organization_id character (36) NOT NULL,
base_fare double precision,
night_shift_start time without time zone,
night_shift_end time without time zone,
night_shift_rate double precision,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
ALTER TABLE atlas_driver_offer_bpp.fare_policy OWNER TO atlas_driver_offer_bpp_user;

CREATE TABLE atlas_driver_offer_bpp.fare_policy_per_extra_km_rate (
id character(36) NOT NULL,
organization_id character(36) NOT NULL,
distance_range_start double precision NOT NULL,
fare double precision NOT NULL
,CONSTRAINT  fare_policy_extra_km_rate_unique_extra_distance_range_start UNIQUE (organization_id, distance_range_start)
,CONSTRAINT  fare_policy_per_extra_km_rate_pkey PRIMARY KEY (id)
);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_per_extra_km_rate OWNER TO atlas_driver_offer_bpp_user;



-- NOT FOR LOCAL HENCE COMMENTED --
-- FOR MASTER AND PROD ------------
-- CREATE TABLE atlas_driver_offer_bpp.merchant_push_notification_new (
--     body text NOT NULL,
--     fcm_notification_type text NOT NULL,
--     fcm_sub_category text,
--     id character varying(36) NOT NULL,
--     key text NOT NULL,
--     language text NOT NULL,
--     merchant_id character varying(36) NOT NULL,
--     merchant_operating_city_id character varying(36) NOT NULL,
--     title text NOT NULL,
--     trip_category text,
--     created_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
--     updated_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
--     PRIMARY KEY (id)
-- );

-- INSERT INTO atlas_driver_offer_bpp.merchant_push_notification_new
-- (
--     body,
--     fcm_notification_type,
--     id,
--     key,
--     language,
--     merchant_id,
--     merchant_operating_city_id,
--     title,
--     created_at,
--     updated_at
-- )
-- SELECT
--     body,
--     fcm_notification_type,
--     atlas_driver_offer_bpp.uuid_generate_v4() as id,
--     key,
--     language,
--     merchant_id,
--     merchant_operating_city_id,
--     title,
--     created_at,
--     updated_at
-- FROM atlas_driver_offer_bpp.merchant_push_notification;

-- ALTER TABLE atlas_driver_offer_bpp.merchant_push_notification RENAME TO merchant_push_notification_backup;
-- ALTER TABLE atlas_driver_offer_bpp.merchant_push_notification_new RENAME TO merchant_push_notification;

-- ALTER TABLE atlas_driver_offer_bpp.merchant_push_notification_backup
-- RENAME CONSTRAINT merchant_push_notification_pkey TO merchant_push_notification_pkey_bak;

-- ALTER TABLE atlas_driver_offer_bpp.merchant_push_notification
-- RENAME CONSTRAINT merchant_push_notification_new_pkey TO merchant_push_notification_pkey;



-- [NOTE:] PLEASE RUN THE ABOVE COMMENTED QUERIES BEFORE THESE UNIQUE updates --
-- Ensure uniqueness when both trip_category and fcm_sub_category are not null
CREATE UNIQUE INDEX unique_combination_dne_not_null
ON atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type,
    key,
    merchant_id,
    merchant_operating_city_id,
    language,
    trip_category,
    fcm_sub_category
)
WHERE trip_category IS NOT NULL AND fcm_sub_category IS NOT NULL;

-- Ensure uniqueness when trip_category is null and fcm_sub_category is not null
CREATE UNIQUE INDEX unique_combination_trip_null_fcm_sub_not_null
ON atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type,
    key,
    merchant_id,
    merchant_operating_city_id,
    language,
    fcm_sub_category
)
WHERE trip_category IS NULL AND fcm_sub_category IS NOT NULL;

-- Ensure uniqueness when trip_category is not null and fcm_sub_category is null
CREATE UNIQUE INDEX unique_combination_trip_not_null_fcm_sub_null
ON atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type,
    key,
    merchant_id,
    merchant_operating_city_id,
    language,
    trip_category
)
WHERE trip_category IS NOT NULL AND fcm_sub_category IS NULL;

-- Ensure uniqueness when both trip_category and fcm_sub_category are null
CREATE UNIQUE INDEX unique_combination_trip_null_fcm_sub_null
ON atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type,
    key,
    merchant_id,
    merchant_operating_city_id,
    language
)
WHERE trip_category IS NULL AND fcm_sub_category IS NULL;
