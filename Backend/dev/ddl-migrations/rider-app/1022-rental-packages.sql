CREATE TABLE atlas_app.quote_bak_1022 AS TABLE atlas_app.quote;
--CREATE TABLE atlas_app.ride_booking_bak_1022 AS TABLE atlas_app.ride_booking;
CREATE TABLE atlas_app.search_request_bak_1022 AS TABLE atlas_app.search_request;

CREATE TABLE atlas_app.rental_quote (
    quote_id character(36) NOT NULL PRIMARY KEY REFERENCES atlas_app.quote (id),
    base_distance double precision NOT NULL,
    base_duration_hr integer NOT NULL
);

CREATE TABLE atlas_app.quote_terms (
    id character(36) NOT NULL PRIMARY KEY,
    quote_id character(36) NOT NULL REFERENCES atlas_app.quote (id),
    description character varying(1000) NOT NULL
);

ALTER TABLE atlas_app.quote ALTER COLUMN distance_to_nearest_driver DROP NOT NULL;

-- ALTER TABLE atlas_app.ride_booking ALTER COLUMN distance DROP NOT NULL;
-- ALTER TABLE atlas_app.ride_booking ALTER COLUMN to_location_id DROP NOT NULL;