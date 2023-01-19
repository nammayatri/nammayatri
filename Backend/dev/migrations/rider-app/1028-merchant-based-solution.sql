CREATE TABLE atlas_app.merchant (
    id character(36) NOT NULL PRIMARY KEY,
    short_id character varying(255) NOT NULL,
    UNIQUE (short_id)
);

INSERT INTO atlas_app.merchant (id, short_id) VALUES
    ('da4e23a5-3ce6-4c37-8b9b-41377c3c1a51', 'YATRI'),
    ('da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'NAMMA_YATRI');


ALTER TABLE atlas_app.person ADD COLUMN merchant_id character(36) NOT NULL REFERENCES atlas_app.merchant (id) DEFAULT 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51';

ALTER TABLE atlas_app.ride_booking ADD COLUMN merchant_id character(36) NOT NULL REFERENCES atlas_app.merchant (id) DEFAULT 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51';
ALTER TABLE atlas_app.search_request ADD COLUMN merchant_id character(36) NOT NULL REFERENCES atlas_app.merchant (id) DEFAULT 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51';

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
