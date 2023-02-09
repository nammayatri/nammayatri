ALTER TABLE
   atlas_app.booking DROP CONSTRAINT ride_booking_from_location_id_fkey,
ADD
   CONSTRAINT booking_from_location_id_fkey FOREIGN KEY (from_location_id) REFERENCES atlas_app.booking_location(id)