CREATE TABLE atlas_app.organization_location (
    org_id character(36) PRIMARY KEY NOT NULL REFERENCES atlas_app.organization (id) ON DELETE CASCADE,
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

CREATE INDEX idx_16435_city ON atlas_app.organization_location USING btree (city);

CREATE INDEX idx_16435_state ON atlas_app.organization_location USING btree (state);

INSERT INTO atlas_app.organization_location (
    SELECT T2.id, T1.lat, T1.long, T1.district, T1.city, T1.state, T1.country, T1.pincode, T1.address, T1.created_at, T1.updated_at 
    FROM atlas_app.location AS T1
    INNER JOIN atlas_app.organization AS T2
    ON T1.id = T2.location_id);
DELETE FROM atlas_app.location WHERE id IN (SELECT location_id FROM atlas_app.organization); 

--------------------------------------------------------------------------------------------

ALTER TABLE atlas_app.person DROP COLUMN location_id;
ALTER TABLE atlas_app.location DROP COLUMN info;
ALTER TABLE atlas_app.location DROP COLUMN ward;
ALTER TABLE atlas_app.location DROP COLUMN point;
ALTER TABLE atlas_app.location DROP COLUMN location_type;
ALTER TABLE atlas_app.location DROP COLUMN bound;

ALTER TABLE atlas_app.location RENAME TO search_request_location;

DELETE FROM atlas_app.search_request_location WHERE lat IS NULL OR long IS NULL;

ALTER TABLE atlas_app.search_request_location ALTER COLUMN lat SET NOT NULL;
ALTER TABLE atlas_app.search_request_location ALTER COLUMN long SET NOT NULL;