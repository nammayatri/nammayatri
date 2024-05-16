CREATE TABLE atlas_app.quote_bak_1026 AS TABLE atlas_app.quote;
--CREATE TABLE atlas_app.ride_booking_bak_1026 AS TABLE atlas_app.ride_booking;
CREATE TABLE atlas_app.search_request_location_1026 AS TABLE atlas_app.search_request_location;

INSERT INTO
   atlas_app.booking_location (
      SELECT
         (
            T1.id,
            T1.lat,
            T1.lon,
            T1.city,
            T1.state,
            T1.country,
            T1.street,
            T1.door,
            T1.building,
            T1.area_code,
            T1.area,
            T1.created_at,
            T1.updated_at
         )
      FROM
         atlas_app.search_request_location AS T1
         RIGHT JOIN atlas_app.booking AS T2 ON (
            T2.from_location_id = T1.id
            OR T2.to_location_id = T1.id
         )
   );

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
