
ALTER TABLE atlas_app.ride_booking RENAME TO booking;

ALTER TABLE atlas_app.fare_breakup RENAME COLUMN ride_booking_id TO booking_id;

