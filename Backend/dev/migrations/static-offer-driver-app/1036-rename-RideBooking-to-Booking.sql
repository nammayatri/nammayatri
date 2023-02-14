
ALTER TABLE atlas_transporter.ride_booking RENAME TO booking;
ALTER TABLE atlas_transporter.ride_booking_cancellation_reason RENAME TO booking_cancellation_reason;
ALTER TABLE atlas_transporter.rental_ride_booking RENAME TO rental_booking;

ALTER TABLE atlas_transporter.allocation_event RENAME COLUMN ride_booking_id TO booking_id;
ALTER TABLE atlas_transporter.booking_cancellation_reason RENAME COLUMN ride_booking_id TO booking_id;
ALTER TABLE atlas_transporter.business_event RENAME COLUMN ride_booking_id TO booking_id;
ALTER TABLE atlas_transporter.discount_transaction RENAME COLUMN ride_booking_id TO booking_id;
ALTER TABLE atlas_transporter.notification_status RENAME COLUMN ride_booking_id TO booking_id;
ALTER TABLE atlas_transporter.rental_booking RENAME COLUMN ride_booking_id TO booking_id;
ALTER TABLE atlas_transporter.ride_request RENAME COLUMN ride_booking_id TO booking_id;
ALTER TABLE atlas_transporter.fare_breakup RENAME COLUMN ride_booking_id TO booking_id;
