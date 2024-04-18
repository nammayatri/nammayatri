CREATE TABLE atlas_app.booking_update_request ();

ALTER TABLE atlas_app.booking_update_request ADD COLUMN booking_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.booking_update_request ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.booking_update_request ADD COLUMN current_point_lat double precision ;
ALTER TABLE atlas_app.booking_update_request ADD COLUMN current_point_lon double precision ;
ALTER TABLE atlas_app.booking_update_request ADD COLUMN estimated_fare double precision ;
ALTER TABLE atlas_app.booking_update_request ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.booking_update_request ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.booking_update_request ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.booking_update_request ADD COLUMN old_estimated_fare double precision NOT NULL;
ALTER TABLE atlas_app.booking_update_request ADD COLUMN old_total_distance integer NOT NULL;
ALTER TABLE atlas_app.booking_update_request ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.booking_update_request ADD COLUMN total_distance integer ;
ALTER TABLE atlas_app.booking_update_request ADD COLUMN travelled_distance integer ;
ALTER TABLE atlas_app.booking_update_request ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.booking_update_request ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.booking_update_request ALTER COLUMN old_estimated_fare TYPE text;
ALTER TABLE atlas_app.booking_update_request ADD COLUMN old_estimated_distance integer ;
ALTER TABLE atlas_app.booking_update_request ADD COLUMN estimated_distance integer ;
ALTER TABLE atlas_app.booking_update_request DROP COLUMN old_total_distance;


------- SQL updates -------

ALTER TABLE atlas_app.booking_update_request ALTER COLUMN old_estimated_fare TYPE double precision;