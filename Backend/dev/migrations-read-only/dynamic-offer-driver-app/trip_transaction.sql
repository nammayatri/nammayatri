CREATE TABLE atlas_driver_offer_bpp.trip_transaction ();

ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN allow_ending_mid_route boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN deviation_count integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN end_location_lat double precision ;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN end_location_lon double precision ;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN end_stop_code text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN fleet_owner_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN is_currently_deviated boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN round_route_code text ;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN route_code text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN start_location_lat double precision ;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN start_location_lon double precision ;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN started_near_stop_code text ;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN trip_code text ;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN trip_end_time timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN trip_start_time timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN vehicle_number text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN vehicle_service_tier_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN end_ride_approval_request_id character varying(36) ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN trip_termination_source text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN driver_name text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN fleet_badge_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN trip_start_source text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN conductor_fleet_badge_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN conductor_name text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN vip_name text ;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN duty_type text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN start_address text ;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN end_address text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN scheduled_trip_time timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN trip_type text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN pilot_source text ;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN pilot_destination text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN pilot_source_lon double precision ;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN pilot_source_lat double precision ;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN pilot_destination_lon double precision ;
ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN pilot_destination_lat double precision ;


------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.trip_transaction ADD COLUMN trip_estimated_route_details json ;


------- SQL updates -------

