CREATE TABLE atlas_app.saved_location (
    id character(36) NOT NULL,
    lat double precision NOT NULL,
    lon double precision NOT NULL,
    city character varying(255),
    state character varying(255),
    country character varying(255),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    street character varying(255),
    door character varying(255),
    building character varying(255),
    area_code character varying(255),
    area character varying(255),
    tag character varying(255) NOT NULL,
    rider_id character(36) NOT NULL,
    CONSTRAINT idx_primary_search PRIMARY KEY (id),
    CONSTRAINT rider_id_saved_location_fkey FOREIGN KEY (rider_id) REFERENCES atlas_app.person(id)
);

CREATE INDEX ON atlas_app.saved_location USING btree (rider_id);

CREATE INDEX ON atlas_app.saved_location USING btree (tag);