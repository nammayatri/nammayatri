CREATE TABLE atlas_transporter.business_event (
    id character(36) PRIMARY KEY NOT NULL,
    driver_id character(36),
    event_type varchar (255) NOT NULL,
    time_stamp timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    ride_booking_id character(36),
    when_pool_was_computed varchar (255),
    vehicle_variant varchar (255),
    distance double precision,
    duration double precision,
    ride_id varchar (255)
);