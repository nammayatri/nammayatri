DELETE FROM atlas_transporter.location WHERE id IN (SELECT location_id FROM atlas_transporter.organization); 
ALTER TABLE atlas_transporter.organization DROP COLUMN location_id;

CREATE TABLE atlas_transporter.driver_location (
    driver_id character(36) PRIMARY KEY NOT NULL REFERENCES atlas_transporter.person (id) ON DELETE CASCADE,
    lat double precision,
    long double precision,
    point public.geography(POINT,4326),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

ALTER TABLE atlas_transporter.driver_location OWNER TO atlas;

INSERT INTO atlas_transporter.driver_location (
    SELECT T2.id, T1.lat, T1.long, T1.point, T1.created_at, T1.updated_at FROM atlas_transporter.location AS T1
    INNER JOIN atlas_transporter.person AS T2
    ON T1.id = T2.location_id);
DELETE FROM atlas_transporter.location WHERE id IN (SELECT location_id FROM atlas_transporter.person);
DELETE FROM atlas_transporter.driver_location WHERE lat IS NULL OR long IS NULL OR point IS NULL;

ALTER TABLE atlas_transporter.driver_location ALTER COLUMN lat SET NOT NULL;
ALTER TABLE atlas_transporter.driver_location ALTER COLUMN long SET NOT NULL;
ALTER TABLE atlas_transporter.driver_location ALTER COLUMN point SET NOT NULL;

------------------------------------------------------------------------------------------------------

ALTER TABLE atlas_transporter.person DROP COLUMN location_id;
ALTER TABLE atlas_transporter.location DROP COLUMN ward;
ALTER TABLE atlas_transporter.location DROP COLUMN point;
ALTER TABLE atlas_transporter.location DROP COLUMN location_type;
ALTER TABLE atlas_transporter.location DROP COLUMN bound;

ALTER TABLE atlas_transporter.location RENAME TO search_request_location;

DELETE FROM atlas_transporter.search_request_location WHERE lat IS NULL OR long IS NULL;

ALTER TABLE atlas_transporter.search_request_location ALTER COLUMN lat SET NOT NULL;
ALTER TABLE atlas_transporter.search_request_location ALTER COLUMN long SET NOT NULL;