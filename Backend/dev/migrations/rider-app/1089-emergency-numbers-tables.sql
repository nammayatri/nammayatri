CREATE TABLE atlas_app.person_default_emergency_number (
    person_id character(36) NOT NULL REFERENCES atlas_app.person (id),
    name character varying(255) NOT NULL,
    mobile_country_code character varying(255) NOT NULL,
    mobile_number_encrypted character varying(255) NOT NULL,
    mobile_number_hash bytea NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY  (person_id, mobile_country_code, mobile_number_hash)
);