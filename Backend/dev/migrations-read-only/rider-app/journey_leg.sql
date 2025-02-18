CREATE TABLE atlas_app.journey_leg ();

ALTER TABLE atlas_app.journey_leg ADD COLUMN agency_gtfs_id text ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN agency_name text ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN distance double precision ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN distance_unit character varying(255) ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN duration integer ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN end_location_lat double precision NOT NULL;
ALTER TABLE atlas_app.journey_leg ADD COLUMN end_location_lon double precision NOT NULL;
ALTER TABLE atlas_app.journey_leg ADD COLUMN estimated_max_fare double precision ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN estimated_min_fare double precision ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN from_arrival_time timestamp with time zone ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN from_departure_time timestamp with time zone ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN from_stop_code text ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN from_stop_gtfs_id text ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN from_stop_name text ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.journey_leg ADD COLUMN is_deleted boolean ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN journey_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.journey_leg ADD COLUMN leg_id text ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN mode text NOT NULL;
ALTER TABLE atlas_app.journey_leg ADD COLUMN frequency integer ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN route_color_code text ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN route_color_name text ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN route_gtfs_id text ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN route_long_name text ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN route_short_name text ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN sequence_number integer NOT NULL;
ALTER TABLE atlas_app.journey_leg ADD COLUMN start_location_lat double precision NOT NULL;
ALTER TABLE atlas_app.journey_leg ADD COLUMN start_location_lon double precision NOT NULL;
ALTER TABLE atlas_app.journey_leg ADD COLUMN to_arrival_time timestamp with time zone ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN to_departure_time timestamp with time zone ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN to_stop_code text ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN to_stop_gtfs_id text ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN to_stop_name text ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.journey_leg ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.journey_leg ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.journey_leg ADD COLUMN to_stop_platform_code text ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN from_stop_platform_code text ;




------- SQL updates -------

ALTER TABLE atlas_app.journey_leg ADD COLUMN is_skipped boolean ;