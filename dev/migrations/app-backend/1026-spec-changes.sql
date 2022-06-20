CREATE TABLE atlas_app.booking_location (
   id CHARACTER(36) PRIMARY KEY NOT NULL,
   lat DOUBLE PRECISION NOT NULL,
   lon DOUBLE PRECISION NOT NULL,
   city CHARACTER VARYING(255),
   state CHARACTER VARYING(255),
   country CHARACTER VARYING(255),
   street CHARACTER VARYING(255),
   door CHARACTER VARYING(255),
   building CHARACTER VARYING(255),
   area_code CHARACTER VARYING(255),
   area CHARACTER VARYING(255),
   created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
   updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL
);

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
         RIGHT JOIN atlas_app.ride_booking AS T2 ON (
            T2.from_location_id = T1.id
            OR T2.to_location_id = T1.id
         )
   );
ALTER TABLE
   atlas_app.search_request
ADD
   CONSTRAINT search_request_from_location_id_fkey FOREIGN KEY (from_location_id) REFERENCES atlas_app.search_request_location(id),
ADD
   CONSTRAINT search_request_to_location_id_fkey FOREIGN KEY (to_location_id) REFERENCES atlas_app.search_request_location(id);

ALTER TABLE
   atlas_app.ride_booking DROP CONSTRAINT ride_booking_from_location_id_fkey,
ADD
   CONSTRAINT ride_booking_from_location_id_fkey FOREIGN KEY (to_location_id) REFERENCES atlas_app.booking_location(id),
   DROP CONSTRAINT ride_booking_to_location_id_fkey,
ADD
   CONSTRAINT ride_booking_to_location_id_fkey FOREIGN KEY (to_location_id) REFERENCES atlas_app.booking_location(id);

ALTER TABLE
   atlas_app.search_request_location DROP COLUMN city,
   DROP COLUMN state,
   DROP COLUMN country,
   DROP COLUMN street,
   DROP COLUMN door,
   DROP COLUMN building,
   DROP COLUMN area_code,
   DROP COLUMN area;

ALTER TABLE
   atlas_app.rental_quote
ALTER COLUMN
   base_distance TYPE integer USING (base_distance :: integer);

ALTER TABLE
   atlas_app.ride_booking DROP COLUMN request_id,
   DROP COLUMN quote_id;
ALTER TABLE
   atlas_app.quote DROP COLUMN bpp_quote_id;