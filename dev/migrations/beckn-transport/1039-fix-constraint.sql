ALTER TABLE
   atlas_transporter.booking DROP CONSTRAINT ride_booking_from_location_id_fkey,
ADD
   CONSTRAINT booking_from_location_id_fkey FOREIGN KEY (from_location_id) REFERENCES atlas_transporter.booking_location(id);
