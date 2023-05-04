-- before release
CREATE TABLE atlas_driver_offer_bpp.trip_location as SELECT * FROM atlas_driver_offer_bpp.booking_location;
ALTER TABLE atlas_driver_offer_bpp.trip_location ADD CONSTRAINT trip_location_pkey PRIMARY KEY (id);

-- before release
ALTER TABLE atlas_driver_offer_bpp.booking DROP CONSTRAINT IF EXISTS ride_booking_from_location_id_fkey;
ALTER TABLE atlas_driver_offer_bpp.booking ADD CONSTRAINT ride_booking_from_location_id_fkey FOREIGN KEY (from_location_id) REFERENCES atlas_driver_offer_bpp.trip_location (id);
ALTER TABLE atlas_driver_offer_bpp.booking DROP CONSTRAINT ride_booking_to_location_id_fkey;
ALTER TABLE atlas_driver_offer_bpp.booking ADD CONSTRAINT ride_booking_to_location_id_fkey FOREIGN KEY (to_location_id) REFERENCES atlas_driver_offer_bpp.trip_location (id);

-- before release
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN from_location_id character (36);
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN to_location_id character (36);

-- before release
UPDATE atlas_driver_offer_bpp.ride
SET from_location_id = atlas_driver_offer_bpp.booking.from_location_id
FROM atlas_driver_offer_bpp.booking
WHERE atlas_driver_offer_bpp.ride.booking_id = atlas_driver_offer_bpp.booking.id;

-- before release
UPDATE atlas_driver_offer_bpp.ride
SET to_location_id = atlas_driver_offer_bpp.booking.to_location_id
FROM atlas_driver_offer_bpp.booking
WHERE atlas_driver_offer_bpp.ride.booking_id = atlas_driver_offer_bpp.booking.id;

