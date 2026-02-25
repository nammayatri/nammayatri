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
ALTER TABLE atlas_app.journey_leg ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.journey_leg ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.journey_leg ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.journey_leg ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.journey_leg ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.journey_leg ADD COLUMN to_stop_platform_code text ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN from_stop_platform_code text ;




------- SQL updates -------

ALTER TABLE atlas_app.journey_leg ADD COLUMN is_skipped boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.journey_leg ADD COLUMN service_types text[] ;


------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.journey_leg ADD COLUMN final_boarded_bus_number text ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN changed_buses_in_sequence text[] ;


------- SQL updates -------

ALTER TABLE atlas_app.journey_leg ADD COLUMN exit json ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN entrance json ;




------- SQL updates -------

ALTER TABLE atlas_app.journey_leg ADD COLUMN status text ;





------- SQL updates -------

ALTER TABLE atlas_app.journey_leg ADD COLUMN straight_line_exit json ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN straight_line_entrance json ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN osm_exit json ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN osm_entrance json ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.journey_leg ADD COLUMN route_group_id text ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.journey_leg ADD COLUMN rider_id character varying(36) ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.journey_leg ADD COLUMN group_code text ;


------- SQL updates -------




------- SQL updates -------



------- SQL updates -------
ALTER TABLE atlas_app.journey_leg ALTER COLUMN sequence_number DROP NOT NULL;
ALTER TABLE atlas_app.journey_leg ALTER COLUMN journey_id DROP NOT NULL;



------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.journey_leg ADD COLUMN leg_pricing_id text ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.journey_leg ADD COLUMN multimodal_search_request_id text ;


------- SQL updates -------



------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.journey_leg ADD COLUMN final_boarded_bus_number_updated_by_user boolean ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.journey_leg ADD COLUMN final_boarded_waybill_id text ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN final_boarded_schedule_no text ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN final_boarded_depot_no text ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN final_boarded_bus_number_source text ;


------- SQL updates -------

ALTER TABLE atlas_app.journey_leg ADD COLUMN final_boarded_bus_service_tier_type text ;


------- SQL updates -------

ALTER TABLE atlas_app.journey_leg ADD COLUMN user_booked_bus_service_tier_type text ;


------- SQL updates -------

ALTER TABLE atlas_app.journey_leg ADD COLUMN bus_location_data json ;


------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.journey_leg ALTER COLUMN service_types TYPE text;
ALTER TABLE atlas_app.journey_leg ADD COLUMN bus_driver_id text ;
ALTER TABLE atlas_app.journey_leg ADD COLUMN bus_conductor_id text ;


------- SQL updates -------

ALTER TABLE atlas_app.journey_leg ADD COLUMN provider_route_id text ;


------- SQL updates -------

ALTER TABLE atlas_app.journey_leg ADD COLUMN user_preferred_service_tier text ;