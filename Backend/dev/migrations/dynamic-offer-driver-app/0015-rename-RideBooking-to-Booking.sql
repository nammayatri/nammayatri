
ALTER TABLE atlas_driver_offer_bpp.ride_booking RENAME TO booking;

ALTER TABLE atlas_driver_offer_bpp.business_event RENAME COLUMN ride_booking_id TO booking_id;
