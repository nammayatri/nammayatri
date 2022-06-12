CREATE TABLE atlas_app.rental_quote_bak_1023 AS TABLE atlas_app.rental_quote;
CREATE TABLE atlas_app.quote_terms_bak_1023 AS TABLE atlas_app.quote_terms;

-- do we need migrate data also?
CREATE TABLE atlas_app.rental_slab (
    id character(36) NOT NULL PRIMARY KEY,
    base_distance double precision NOT NULL,
    base_duration_hr integer NOT NULL
);

DROP TABLE atlas_app.rental_quote;

-- do we need migrate data also?
CREATE TABLE atlas_app.trip_terms (
    id character(36) NOT NULL PRIMARY KEY,
    descriptions text NOT NULL
);

DROP TABLE atlas_app.quote_terms;

CREATE TABLE atlas_app.fare_breakup (
    id character(36) NOT NULL PRIMARY KEY,
    ride_booking_id character(36) NOT NULL REFERENCES atlas_app.ride_booking (id),
    description text NOT NULL,
    amount double precision NOT NULL
);

-- do we need migrate data also?
ALTER TABLE atlas_app.quote ADD COLUMN fare_product_type varchar(255) NOT NULL DEFAULT 'ONE_WAY';
ALTER TABLE atlas_app.quote ADD COLUMN trip_terms_id character(36) REFERENCES atlas_app.trip_terms (id);
ALTER TABLE atlas_app.quote ADD COLUMN rental_slab_id character(36) REFERENCES atlas_app.rental_slab (id);

-- do we need migrate data also?
ALTER TABLE atlas_app.ride_booking ADD COLUMN fare_product_type varchar(255) NOT NULL DEFAULT 'ONE_WAY';
ALTER TABLE atlas_app.ride_booking ADD COLUMN trip_terms_id character(36) REFERENCES atlas_app.trip_terms (id);
ALTER TABLE atlas_app.ride_booking ADD COLUMN rental_slab_id character(36) REFERENCES atlas_app.rental_slab (id);
