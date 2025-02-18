CREATE TABLE atlas_app.route_details ();

ALTER TABLE atlas_app.route_details ADD COLUMN agency_gtfs_id text ;
ALTER TABLE atlas_app.route_details ADD COLUMN agency_name text ;
ALTER TABLE atlas_app.route_details ADD COLUMN end_location_lat double precision ;
ALTER TABLE atlas_app.route_details ADD COLUMN end_location_lon double precision ;
ALTER TABLE atlas_app.route_details ADD COLUMN frequency integer ;
ALTER TABLE atlas_app.route_details ADD COLUMN from_arrival_time timestamp with time zone ;
ALTER TABLE atlas_app.route_details ADD COLUMN from_departure_time timestamp with time zone ;
ALTER TABLE atlas_app.route_details ADD COLUMN from_stop_code text ;
ALTER TABLE atlas_app.route_details ADD COLUMN from_stop_gtfs_id text ;
ALTER TABLE atlas_app.route_details ADD COLUMN from_stop_name text ;
ALTER TABLE atlas_app.route_details ADD COLUMN from_stop_platform_code text ;
ALTER TABLE atlas_app.route_details ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route_details ADD COLUMN journey_leg_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route_details ADD COLUMN route_color_code text ;
ALTER TABLE atlas_app.route_details ADD COLUMN route_color_name text ;
ALTER TABLE atlas_app.route_details ADD COLUMN route_gtfs_id text ;
ALTER TABLE atlas_app.route_details ADD COLUMN route_long_name text ;
ALTER TABLE atlas_app.route_details ADD COLUMN route_short_name text ;
ALTER TABLE atlas_app.route_details ADD COLUMN start_location_lat double precision ;
ALTER TABLE atlas_app.route_details ADD COLUMN start_location_lon double precision ;
ALTER TABLE atlas_app.route_details ADD COLUMN to_arrival_time timestamp with time zone ;
ALTER TABLE atlas_app.route_details ADD COLUMN to_departure_time timestamp with time zone ;
ALTER TABLE atlas_app.route_details ADD COLUMN to_stop_code text ;
ALTER TABLE atlas_app.route_details ADD COLUMN to_stop_gtfs_id text ;
ALTER TABLE atlas_app.route_details ADD COLUMN to_stop_name text ;
ALTER TABLE atlas_app.route_details ADD COLUMN to_stop_platform_code text ;
ALTER TABLE atlas_app.route_details ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.route_details ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.route_details ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.route_details ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.route_details ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.route_details ADD COLUMN sub_leg_order integer ;