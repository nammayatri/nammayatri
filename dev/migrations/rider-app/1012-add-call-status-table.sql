CREATE TABLE atlas_app.call_status (
    id character(36) PRIMARY KEY NOT NULL,
    exotel_call_sid character varying(255) NOT NULL,
    ride_id character(36) REFERENCES atlas_app.ride (id) NOT NULL,
    status character varying(10) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);