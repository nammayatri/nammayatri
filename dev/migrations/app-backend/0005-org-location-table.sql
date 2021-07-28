CREATE TABLE atlas_app.organization_location (
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

ALTER TABLE atlas_app.organization_location OWNER TO atlas;

ALTER TABLE ONLY atlas_app.organization_location
    ADD CONSTRAINT idx_16435_primary PRIMARY KEY (id);

CREATE INDEX idx_16435_city ON atlas_app.organization_location USING btree (city);

CREATE INDEX idx_16435_state ON atlas_app.organization_location USING btree (state);

INSERT INTO atlas_app.organization_location (SELECT id, lat, long, district, city, state, country, pincode, address, created_at, updated_at
    FROM atlas_app.location WHERE id IN (SELECT location_id FROM atlas_app.organization));
DELETE FROM atlas_app.location WHERE id IN (SELECT location_id FROM atlas_app.organization); 

--------------------------------------------------------------------------------------------

ALTER TABLE atlas_app.location DROP COLUMN info;
ALTER TABLE atlas_app.location DROP COLUMN ward;
ALTER TABLE atlas_app.location DROP COLUMN point;
ALTER TABLE atlas_app.location DROP COLUMN location_type;
ALTER TABLE atlas_app.location DROP COLUMN bound;

ALTER TABLE atlas_app.location RENAME TO search_request_location;