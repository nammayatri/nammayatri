CREATE TABLE atlas_transporter.fare_breakup (
    id character(36) NOT NULL PRIMARY KEY,
    ride_booking_id character(36) NOT NULL REFERENCES atlas_transporter.ride_booking (id),
    description text NOT NULL,
    amount double precision NOT NULL
);
