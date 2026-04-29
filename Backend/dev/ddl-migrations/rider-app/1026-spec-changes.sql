CREATE TABLE atlas_app.quote_bak_1026 AS TABLE atlas_app.quote;
--CREATE TABLE atlas_app.ride_booking_bak_1026 AS TABLE atlas_app.ride_booking;
CREATE TABLE atlas_app.search_request_location_1026 AS TABLE atlas_app.search_request_location;

-- ALTER TABLE
--    atlas_app.booking DROP CONSTRAINT ride_booking_from_location_id_fkey,
-- ADD
--    CONSTRAINT ride_booking_from_location_id_fkey FOREIGN KEY (to_location_id) REFERENCES atlas_app.booking_location(id),
--    DROP CONSTRAINT ride_booking_to_location_id_fkey,
-- ADD
--    CONSTRAINT ride_booking_to_location_id_fkey FOREIGN KEY (to_location_id) REFERENCES atlas_app.booking_location(id);

ALTER TABLE
   atlas_app.rental_quote
ALTER COLUMN
   base_distance TYPE integer USING (base_distance :: integer);
