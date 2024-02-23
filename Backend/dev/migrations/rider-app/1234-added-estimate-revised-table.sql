CREATE TABLE atlas_app.estimate_revised (
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
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    drivers_location text[],
    waiting_charge_per_min double precision, -- to see
    status character varying(255) NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    device text,
    estimated_duration integer,
    estimated_distance integer,
    bpp_estimate_revised_id character(36) DEFAULT 'UNKNOWN' NOT NULL,
    special_location_tag text,
    merchant_id character(36) REFERENCES atlas_app.merchant (id),
    item_id text NOT NULL DEFAULT '',
    merchant_operating_city_id character(36) REFERENCES atlas_app.merchant_operating_city (id),
    min_total_fare numeric(30,2),
    max_total_fare numeric(30,2),
    night_shift_multiplier numeric(10,2),
    night_shift_start time without time zone,
    night_shift_end time without time zone,
    night_shift_charge integer
);

UPDATE atlas_app.estimate_revised
    SET min_total_fare = estimated_total_fare,
        max_total_fare = estimated_total_fare;

ALTER TABLE atlas_app.estimate_revised
  ALTER COLUMN min_total_fare SET NOT NULL;

ALTER TABLE atlas_app.estimate_revised
  ALTER COLUMN max_total_fare SET NOT NULL;

ALTER TABLE atlas_app.estimate_revised ALTER COLUMN bpp_estimate_revised_id DROP DEFAULT;