CREATE TABLE atlas_app.estimate (
    id character(36) NOT NULL PRIMARY KEY,
    request_id character (36) NOT NULL REFERENCES atlas_app.search_request (id),
    estimated_fare numeric(30,10) NOT NULL,
    discount double precision,
    estimated_total_fare numeric(30,2),
    provider_id character varying(255) NOT NULL,
    provider_url character varying(255) NOT NULL,
    provider_name character varying(255) NOT NULL,
    provider_mobile_number character varying(255) NOT NULL,
    provider_completed_rides_count integer NOT NULL,
    vehicle_variant character varying(60) NOT NULL,
    trip_terms_id character(36) REFERENCES atlas_app.trip_terms (id),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE TABLE atlas_app.driver_offer (
    id character(36) NOT NULL PRIMARY KEY,
    estimate_id character(36) NOT NULL REFERENCES atlas_app.estimate (id),
    driver_name character varying(255) NOT NULL,
    distance_to_pickup double precision NOT NULL,
    duration_to_pickup integer NOT NULL,
    valid_till timestamp with time zone NOT NULL,
    rating double precision,
    bpp_quote_id character(36) NOT NULL
);

-- TODO do we need to migrate old data?
DROP TABLE atlas_app.selected_quote;

ALTER TABLE atlas_app.quote ADD COLUMN driver_offer_id character(36) REFERENCES atlas_app.driver_offer (id);
