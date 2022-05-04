CREATE TABLE atlas_transporter.quote_bak_1021 AS TABLE atlas_transporter.quote;
CREATE TABLE atlas_transporter.ride_booking_bak_1021 AS TABLE atlas_transporter.ride_booking;
CREATE TABLE atlas_transporter.fare_policy_discount_bak_1021 AS TABLE atlas_transporter.fare_policy_discount;
CREATE TABLE atlas_transporter.search_request_bak_1021 AS TABLE atlas_transporter.search_request;

CREATE TABLE atlas_transporter.fare_product (
    id character(36) NOT NULL PRIMARY KEY,
    organization_id character(36) NOT NULL REFERENCES atlas_transporter.organization (id),
    enabled boolean NOT NULL,
    type character varying(255) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

INSERT INTO atlas_transporter.fare_product (id, organization_id, enabled, type, created_at, updated_at)
    SELECT
        md5(random()::text || clock_timestamp()::text)::uuid,
        T1.id,
        true,
        'ONE_WAY',
        now (),
		now ()
    FROM atlas_transporter.organization AS T1;

CREATE TABLE atlas_transporter.rental_fare_policy (
    id character(36) NOT NULL PRIMARY KEY,
    organization_id character(36) NOT NULL REFERENCES atlas_transporter.organization (id),
    vehicle_variant character varying(255) NOT NULL,
    base_fare double precision NOT NULL,
    base_distance double precision NOT NULL,
    base_duration_hr integer NOT NULL,
    extra_km_fare double precision NOT NULL,
    extra_minute_fare double precision NOT NULL,
    driver_allowance_for_day double precision
);

ALTER TABLE atlas_transporter.fare_policy_discount ADD COLUMN fare_product_type character varying(255) NOT NULL DEFAULT 'ONE_WAY';

CREATE TABLE atlas_transporter.one_way_quote (
    id character(36) NOT NULL PRIMARY KEY,
    quote_id character(36) NOT NULL REFERENCES atlas_transporter.quote (id),
    distance double precision NOT NULL,
    distance_to_nearest_driver double precision NOT NULL
);

INSERT INTO atlas_transporter.one_way_quote (id, quote_id, distance, distance_to_nearest_driver)
    SELECT
        md5(random()::text || clock_timestamp()::text)::uuid,
        T1.id,
        T1.distance,
        T1.distance_to_nearest_driver
    FROM atlas_transporter.quote AS T1;

ALTER TABLE atlas_transporter.quote DROP COLUMN distance;
ALTER TABLE atlas_transporter.quote DROP COLUMN distance_to_nearest_driver;

ALTER TABLE atlas_transporter.ride_booking ADD COLUMN fare_product_type character varying(255) NOT NULL DEFAULT 'ONE_WAY';
ALTER TABLE atlas_transporter.ride_booking RENAME COLUMN distance TO estimated_distance;
ALTER TABLE atlas_transporter.ride_booking ALTER COLUMN estimated_distance DROP NOT NULL;

ALTER TABLE atlas_transporter.search_request ALTER COLUMN to_location_id DROP NOT NULL;
