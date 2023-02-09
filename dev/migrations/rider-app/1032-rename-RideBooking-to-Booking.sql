
ALTER TABLE atlas_app.ride_booking RENAME TO booking;
ALTER TABLE atlas_app.ride_booking_cancellation_reason RENAME TO booking_cancellation_reason;

ALTER TABLE atlas_app.booking_cancellation_reason RENAME COLUMN ride_booking_id TO booking_id;
ALTER TABLE atlas_app.fare_breakup RENAME COLUMN ride_booking_id TO booking_id;

