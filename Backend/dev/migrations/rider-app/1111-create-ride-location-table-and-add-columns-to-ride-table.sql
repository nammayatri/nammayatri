-- before the release
CREATE TABLE atlas_app.trip_location as SELECT * FROM atlas_app.booking_location;
ALTER TABLE atlas_app.trip_location ADD CONSTRAINT trip_location_pkey PRIMARY KEY (id);

-- before the release
ALTER TABLE atlas_app.ride ADD COLUMN from_location_id character(36);
ALTER TABLE atlas_app.ride ADD COLUMN to_location_id character(36);

-- before the release
UPDATE atlas_app.ride
SET from_location_id = atlas_app.booking.from_location_id
FROM atlas_app.booking
WHERE atlas_app.ride.booking_id = atlas_app.booking.id;

-- before the release
UPDATE atlas_app.ride
SET to_location_id = atlas_app.booking.to_location_id
FROM atlas_app.booking
WHERE atlas_app.ride.booking_id = atlas_app.booking.id;

-- before the release
ALTER TABLE atlas_app.booking
    DROP CONSTRAINT booking_from_location_id_fkey,
    ADD CONSTRAINT booking_from_location_id_fkey FOREIGN KEY (from_location_id) REFERENCES atlas_app.trip_location (id),
    DROP CONSTRAINT ride_booking_to_location_id_fkey,
    ADD CONSTRAINT ride_booking_to_location_id_fkey FOREIGN KEY (to_location_id) REFERENCES atlas_app.trip_location (id);
