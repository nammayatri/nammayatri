CREATE TABLE atlas_app.rental_quote_bak_1027 AS TABLE atlas_app.rental_quote;

CREATE TABLE atlas_app.quote_terms_bak_1027 AS TABLE atlas_app.quote_terms;

CREATE TABLE atlas_app.rental_slab (
    id character(36) NOT NULL PRIMARY KEY,
    base_distance integer NOT NULL,
    base_duration integer NOT NULL
);

ALTER TABLE
    atlas_app.quote
ADD
    COLUMN rental_slab_id character(36);

UPDATE
    atlas_app.quote
SET
    rental_slab_id = atlas_app.uuid_generate_v4()
FROM
    atlas_app.rental_quote AS T1
WHERE
    id = T1.quote_id;

INSERT INTO
    atlas_app.rental_slab(
        SELECT
            T2.rental_slab_id,
            T1.base_distance,
            T1.base_duration_hr
        FROM
            atlas_app.rental_quote AS T1,
            atlas_app.quote AS T2
        WHERE
            T1.quote_id = T2.id
    );

ALTER TABLE
    atlas_app.quote
ADD
    CONSTRAINT quote_rental_slab_id_fkey FOREIGN KEY (rental_slab_id) REFERENCES atlas_app.rental_slab (id);

DROP TABLE atlas_app.rental_quote;

UPDATE
    atlas_app.quote AS T1
SET
    trip_terms_id = atlas_app.uuid_generate_v4()
FROM
    atlas_app.quote_terms AS T2
WHERE
    T1.id = T2.quote_id;

UPDATE
    atlas_app.trip_terms
SET
    id = T2.trip_terms_id,
    descriptions = T1.description
FROM
    atlas_app.quote_terms AS T1,
    atlas_app.quote AS T2
WHERE
    T1.quote_id = T2.id;

DROP TABLE atlas_app.quote_terms;

ALTER TABLE
    atlas_app.quote
ALTER COLUMN
    fare_product_type DROP DEFAULT;

UPDATE
    atlas_app.quote
SET
    fare_product_type = 'RENTAL'
WHERE
    distance_to_nearest_driver IS NULL;

-- ALTER TABLE
--     atlas_app.ride_booking
-- ADD
--     COLUMN fare_product_type varchar(255) NOT NULL DEFAULT 'ONE_WAY';

-- ALTER TABLE
--     atlas_app.ride_booking
-- ALTER COLUMN
--     fare_product_type DROP DEFAULT;

UPDATE
    atlas_app.booking
SET
    fare_product_type = 'RENTAL'
WHERE
    to_location_id IS NULL;

INSERT INTO
    atlas_app.trip_terms (id, descriptions)
VALUES
    (
        '000dummy-trip-term-0000-000000000000',
        'UNKNOWN'
    );

INSERT INTO
    atlas_app.rental_slab (id, base_distance, base_duration)
VALUES
    (
        '000dummy-rent-slab-0000-000000000000',
        0,
        0
    );

-- ALTER TABLE
--     atlas_app.ride_booking
-- ADD
--     COLUMN trip_terms_id character(36) REFERENCES atlas_app.trip_terms (id),
-- ADD
--     COLUMN rental_slab_id character(36) REFERENCES atlas_app.rental_slab (id);

-- ALTER TABLE
--     atlas_app.ride_booking
-- ALTER COLUMN
--     trip_terms_id DROP DEFAULT,
-- ALTER COLUMN
--     rental_slab_id DROP DEFAULT;

-- COMMENT NEXT QUERY WHEN RUN ON MASTER
-- UPDATE
--     atlas_app.ride_booking AS T1
-- SET
--     trip_terms_id = T3.trip_terms_id,
--     rental_slab_id = T3.rental_slab_id
-- FROM
--     atlas_app.ride_booking_bak_1026 AS T2,
--     atlas_app.quote AS T3
-- WHERE
--     T3.id = T2.quote_id
--     AND T2.id = T1.id
--     AND T1.fare_product_type = 'RENTAL';