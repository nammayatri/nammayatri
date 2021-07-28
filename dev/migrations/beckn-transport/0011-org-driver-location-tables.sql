CREATE TABLE atlas_transporter.organization_location (
    id character(36) NOT NULL,
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

ALTER TABLE ONLY atlas_transporter.organization_location
    ADD CONSTRAINT idx_16442_primary PRIMARY KEY (id);

CREATE INDEX idx_16442_city ON atlas_transporter.organization_location USING btree (city);

CREATE INDEX idx_16442_state ON atlas_transporter.organization_location USING btree (state);

INSERT INTO atlas_transporter.organization_location (SELECT id, lat, long, district, city, state, country, pincode, address, created_at, updated_at
    FROM atlas_transporter.location WHERE id IN (SELECT location_id FROM atlas_transporter.organization));
DELETE FROM atlas_transporter.location WHERE id IN (SELECT location_id FROM atlas_transporter.organization); 

----------------------------------------------------------------------------------------------------

CREATE TABLE atlas_transporter.driver_location (
    id character(36) NOT NULL,
    lat double precision,
    long double precision,
    point public.geography(POINT,4326),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

ALTER TABLE atlas_transporter.driver_location OWNER TO atlas;

ALTER TABLE ONLY atlas_transporter.driver_location
    ADD CONSTRAINT idx_16543_primary PRIMARY KEY (id);

INSERT INTO atlas_transporter.driver_location (SELECT id, lat, long, point, created_at, updated_at FROM atlas_transporter.location WHERE id IN (SELECT location_id FROM atlas_transporter.person));
DELETE FROM atlas_transporter.location WHERE id IN (SELECT location_id FROM atlas_transporter.person);

------------------------------------------------------------------------------------------------------

ALTER TABLE atlas_transporter.location DROP COLUMN ward;
ALTER TABLE atlas_transporter.location DROP COLUMN point;
ALTER TABLE atlas_transporter.location DROP COLUMN location_type;
ALTER TABLE atlas_transporter.location DROP COLUMN bound;

ALTER TABLE atlas_transporter.location RENAME TO search_request_location;