CREATE TABLE atlas_transporter.quote_bak_1022 AS TABLE atlas_transporter.quote;
CREATE TABLE atlas_transporter.ride_booking_bak_1022 AS TABLE atlas_transporter.ride_booking;
CREATE TABLE atlas_transporter.fare_policy_discount_bak_1022 AS TABLE atlas_transporter.fare_policy_discount;
CREATE TABLE atlas_transporter.search_request_bak_1022 AS TABLE atlas_transporter.search_request;
CREATE TABLE atlas_transporter.ride_bak_1022 AS TABLE atlas_transporter.ride;

CREATE TABLE atlas_transporter.fare_product (
    id character(36) NOT NULL PRIMARY KEY,
    organization_id character(36) NOT NULL REFERENCES atlas_transporter.organization (id),
    type character varying(255) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    UNIQUE (organization_id, type)
);

INSERT INTO atlas_transporter.fare_product (id, organization_id, type, created_at)
    SELECT
        md5(random()::text || clock_timestamp()::text)::uuid,
        T1.id,
        'ONE_WAY',
        now ()
    FROM atlas_transporter.organization AS T1 WHERE T1.type = 'PROVIDER' AND T1.domain = 'MOBILITY';

CREATE TABLE atlas_transporter.rental_fare_policy (
    id character(36) NOT NULL PRIMARY KEY,
    organization_id character(36) NOT NULL REFERENCES atlas_transporter.organization (id),
    vehicle_variant character varying(255) NOT NULL,
    base_fare double precision NOT NULL,
    base_distance double precision NOT NULL,
    base_duration_hr integer NOT NULL,
    extra_km_fare double precision NOT NULL,
    extra_minute_fare double precision NOT NULL,
    driver_allowance_for_day double precision,
    deleted boolean NOT NULL
);

ALTER TABLE atlas_transporter.fare_policy_discount ADD COLUMN fare_product_type character varying(255) NOT NULL DEFAULT 'ONE_WAY';

CREATE TABLE atlas_transporter.one_way_quote (
    quote_id character(36) NOT NULL PRIMARY KEY REFERENCES atlas_transporter.quote (id),
    distance double precision NOT NULL,
    distance_to_nearest_driver double precision NOT NULL
);

INSERT INTO atlas_transporter.one_way_quote (quote_id, distance, distance_to_nearest_driver)
    SELECT
        T1.id,
        T1.distance,
        T1.distance_to_nearest_driver
    FROM atlas_transporter.quote AS T1;

CREATE TABLE atlas_transporter.rental_quote (
    quote_id character(36) NOT NULL PRIMARY KEY REFERENCES atlas_transporter.quote (id),
    rental_fare_policy_id character(36) NOT NULL REFERENCES atlas_transporter.rental_fare_policy (id)
);

CREATE TABLE atlas_transporter.rental_ride_booking (
    ride_booking_id character(36) NOT NULL PRIMARY KEY REFERENCES atlas_transporter.ride_booking (id),
    rental_fare_policy_id character(36) NOT NULL REFERENCES atlas_transporter.rental_fare_policy (id)
);

ALTER TABLE atlas_transporter.quote DROP COLUMN distance;
ALTER TABLE atlas_transporter.quote DROP COLUMN distance_to_nearest_driver;
ALTER TABLE atlas_transporter.quote ADD COLUMN fare_product_type character varying(255) NOT NULL DEFAULT 'ONE_WAY';

ALTER TABLE atlas_transporter.ride_booking RENAME COLUMN distance TO estimated_distance;
ALTER TABLE atlas_transporter.ride_booking ALTER COLUMN estimated_distance DROP NOT NULL;
ALTER TABLE atlas_transporter.ride_booking ALTER COLUMN to_location_id DROP NOT NULL;

ALTER TABLE atlas_transporter.ride ADD COLUMN trip_start_time timestamp with time zone;
ALTER TABLE atlas_transporter.ride ADD COLUMN trip_end_time timestamp with time zone;

ALTER TABLE atlas_transporter.search_request ALTER COLUMN to_location_id DROP NOT NULL;

-- for testing purpose
INSERT INTO atlas_transporter.rental_fare_policy (id, organization_id, vehicle_variant, base_fare, base_distance, base_duration_hr, extra_km_fare, extra_minute_fare, driver_allowance_for_day, deleted) VALUES
    ('1391f193-561e-4f2e-910b-bbe3dbd257be', 'e1f37274-f0aa-4bb3-93a0-2476349487b7', 'SUV', '19.0', '100.0', '3', '0.3', '0.29', '5', false),
    ('1391f193-561e-4f2e-910b-bbe3dbd257br', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'SUV', '19.0', '100.0', '3', '0.3', '0.29', '5', false);
