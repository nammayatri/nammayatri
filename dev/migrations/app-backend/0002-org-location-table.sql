CREATE TABLE atlas_app.organization_location (
    id character(36) NOT NULL,
    location_type character varying(255),
    lat double precision,
    long double precision,
    point public.geography(POINT,4326),
    ward character varying(255),
    district character varying(255),
    city character varying(255),
    state character varying(255),
    country character varying(255),
    pincode character varying(255),
    address character varying(255),
    bound character varying(255),
    info text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

ALTER TABLE atlas_app.organization_location OWNER TO atlas;

ALTER TABLE ONLY atlas_app.organization_location
    ADD CONSTRAINT idx_16435_primary PRIMARY KEY (id);

CREATE INDEX idx_16435_city ON atlas_app.organization_location USING btree (city);

CREATE INDEX idx_16435_state ON atlas_app.organization_location USING btree (state);