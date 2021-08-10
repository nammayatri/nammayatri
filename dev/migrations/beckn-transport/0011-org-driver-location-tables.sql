CREATE TABLE atlas_transporter.organization_location (
    org_id character(36) PRIMARY KEY NOT NULL REFERENCES atlas_transporter.organization (id) ON DELETE CASCADE,
    lat double precision,
    long double precision,
    district character varying(255),
    city character varying(255),
    state character varying(255),
    country character varying(255),
    pincode character varying(255),
    address character varying(255),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

ALTER TABLE atlas_transporter.organization_location OWNER TO atlas;

CREATE INDEX idx_16442_city ON atlas_transporter.organization_location USING btree (city);

CREATE INDEX idx_16442_state ON atlas_transporter.organization_location USING btree (state);

INSERT INTO atlas_transporter.organization_location (
    SELECT T2.id, T1.lat, T1.long, T1.district, T1.city, T1.state, T1.country, T1.pincode, T1.address, T1.created_at, T1.updated_at 
    FROM atlas_transporter.location AS T1
    INNER JOIN atlas_transporter.organization AS T2
    ON T1.id = T2.location_id);
DELETE FROM atlas_transporter.location WHERE id IN (SELECT location_id FROM atlas_transporter.organization); 

----------------------------------------------------------------------------------------------------

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
DELETE FROM atlas_transporter.driver_location WHERE lat IS NULL OR long IS NULL;

UPDATE atlas_transporter.driver_location AS T1 
	SET point = public.ST_SetSRID(public.ST_Point(T1.long, T1.lat), 4326)
    WHERE point IS NULL;

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

--------------------------------------------------------------------------------------------
-- Creating locations if there was none for some reason
--------------------------------------------------------------------------------------------

INSERT INTO atlas_transporter.organization_location (
    SELECT T1.id, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, now (), now ()
    FROM atlas_transporter.organization AS T1
    WHERE NOT EXISTS (SELECT 1 FROM atlas_transporter.organization_location AS T2 WHERE T1.id = T2.org_id)
);