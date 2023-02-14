CREATE TABLE atlas_driver_offer_bpp.call_status (
    id character(36) PRIMARY KEY NOT NULL,
    exotel_call_sid character varying(255) NOT NULL,
    ride_id character(36) REFERENCES atlas_driver_offer_bpp.ride (id) NOT NULL,
    recording_url character varying(255),
    status varchar (255) NOT NULL,
    conversation_duration int8,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);