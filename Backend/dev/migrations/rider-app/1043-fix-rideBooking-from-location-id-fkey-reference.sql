ALTER TABLE
   atlas_app.booking
ADD
   CONSTRAINT booking_from_location_id_fkey FOREIGN KEY (from_location_id) REFERENCES atlas_app.booking_location(id)