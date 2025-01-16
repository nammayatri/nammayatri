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

-- In master and prod drop existing pkeys and make id pkey
-- Please drop all existing pkeys
ALTER TABLE atlas_driver_offer_bpp.merchant_push_notification DROP CONSTRAINT merchant_push_notification_pkey;
ALTER TABLE atlas_driver_offer_bpp.merchant_push_notification ADD PRIMARY KEY ( id);


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
