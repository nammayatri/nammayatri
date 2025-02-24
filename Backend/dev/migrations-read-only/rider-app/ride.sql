CREATE TABLE atlas_app.ride ();

ALTER TABLE atlas_app.ride ADD COLUMN allowed_edit_location_attempts int ;
ALTER TABLE atlas_app.ride ADD COLUMN booking_id character(36) NOT NULL;
ALTER TABLE atlas_app.ride ADD COLUMN bpp_ride_id character(36) NOT NULL;
ALTER TABLE atlas_app.ride ADD COLUMN chargeable_distance numeric(30,2) ;
ALTER TABLE atlas_app.ride ADD COLUMN client_id character varying(36) ;
ALTER TABLE atlas_app.ride ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ride ADD COLUMN driver_arrival_time timestamp with time zone ;
ALTER TABLE atlas_app.ride ADD COLUMN driver_image text ;
ALTER TABLE atlas_app.ride ADD COLUMN driver_mobile_country_code text ;
ALTER TABLE atlas_app.ride ADD COLUMN driver_mobile_number character varying(255) NOT NULL;
ALTER TABLE atlas_app.ride ADD COLUMN driver_name character varying(255) NOT NULL;
ALTER TABLE atlas_app.ride ADD COLUMN driver_rating numeric(10,2) ;
ALTER TABLE atlas_app.ride ADD COLUMN driver_registered_at timestamp with time zone ;
ALTER TABLE atlas_app.ride ADD COLUMN end_odometer_reading double precision ;
ALTER TABLE atlas_app.ride ADD COLUMN end_otp text ;
ALTER TABLE atlas_app.ride ADD COLUMN fare numeric(30,2) ;

ALTER TABLE atlas_app.ride ADD COLUMN id character(36) NOT NULL;
ALTER TABLE atlas_app.ride ADD COLUMN is_free_ride boolean ;
ALTER TABLE atlas_app.ride ADD COLUMN merchant_id character(36) ;
ALTER TABLE atlas_app.ride ADD COLUMN merchant_operating_city_id character(36) ;
ALTER TABLE atlas_app.ride ADD COLUMN otp character(4) NOT NULL;
ALTER TABLE atlas_app.ride ADD COLUMN ride_end_time timestamp with time zone ;
ALTER TABLE atlas_app.ride ADD COLUMN ride_rating bigint ;
ALTER TABLE atlas_app.ride ADD COLUMN ride_start_time timestamp with time zone ;
ALTER TABLE atlas_app.ride ADD COLUMN safety_check_status boolean ;
ALTER TABLE atlas_app.ride ADD COLUMN short_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ride ADD COLUMN start_odometer_reading double precision ;
ALTER TABLE atlas_app.ride ADD COLUMN status character varying(255) NOT NULL;

ALTER TABLE atlas_app.ride ADD COLUMN total_fare numeric(30,2) ;
ALTER TABLE atlas_app.ride ADD COLUMN tracking_url character varying(255) ;
ALTER TABLE atlas_app.ride ADD COLUMN traveled_distance numeric(30,2) ;
ALTER TABLE atlas_app.ride ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ride ADD COLUMN vehicle_color character varying(255) NOT NULL;
ALTER TABLE atlas_app.ride ADD COLUMN vehicle_model character varying(255) NOT NULL;
ALTER TABLE atlas_app.ride ADD COLUMN vehicle_number character varying(255) NOT NULL;
ALTER TABLE atlas_app.ride ADD COLUMN vehicle_variant character varying(60) NOT NULL;
ALTER TABLE atlas_app.ride ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.ride ALTER COLUMN vehicle_color DROP NOT NULL;
ALTER TABLE atlas_app.ride ADD COLUMN currency character varying(255) ;


------- SQL updates -------

ALTER TABLE atlas_app.ride ADD COLUMN vehicle_service_tier_type text ;


------- SQL updates -------

ALTER TABLE atlas_app.ride ADD COLUMN traveled_distance_value double precision ;
ALTER TABLE atlas_app.ride ADD COLUMN distance_unit character varying(255) ;
ALTER TABLE atlas_app.ride ADD COLUMN chargeable_distance_value double precision ;


------- SQL updates -------

ALTER TABLE atlas_app.ride ADD COLUMN client_sdk_version text ;
ALTER TABLE atlas_app.ride ADD COLUMN client_os_version text ;
ALTER TABLE atlas_app.ride ADD COLUMN client_os_type text ;
ALTER TABLE atlas_app.ride ADD COLUMN client_config_version text ;
ALTER TABLE atlas_app.ride ADD COLUMN client_bundle_version text ;
ALTER TABLE atlas_app.ride ADD COLUMN backend_config_version text ;
ALTER TABLE atlas_app.ride ADD COLUMN backend_app_version text ;


------- SQL updates -------

ALTER TABLE atlas_app.ride ADD COLUMN show_drivers_previous_ride_drop_loc boolean ;
ALTER TABLE atlas_app.ride ADD COLUMN drivers_previous_ride_drop_lon double precision ;
ALTER TABLE atlas_app.ride ADD COLUMN drivers_previous_ride_drop_lat double precision ;


------- SQL updates -------

ALTER TABLE atlas_app.ride ADD COLUMN toll_confidence text ;


------- SQL updates -------

ALTER TABLE atlas_app.ride ADD COLUMN payment_done boolean ;
ALTER TABLE atlas_app.ride ADD COLUMN driver_account_id text ;

------- SQL updates -------

ALTER TABLE atlas_app.ride ADD COLUMN online_payment boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.ride ADD COLUMN allowed_edit_pickup_location_attempts int ;

------- SQL updates -------

ALTER TABLE atlas_app.ride ADD COLUMN vehicle_age int;


------- SQL updates -------

ALTER TABLE atlas_app.ride ADD COLUMN driver_number_hash bytea ;
ALTER TABLE atlas_app.ride ADD COLUMN driver_number_encrypted text ;
ALTER TABLE atlas_app.ride ADD COLUMN client_model_name text ;
ALTER TABLE atlas_app.ride ADD COLUMN client_manufacturer text ;



------- SQL updates -------

ALTER TABLE atlas_app.ride ADD COLUMN cancellation_fee_if_cancelled double precision ;



------- SQL updates -------

ALTER TABLE atlas_app.ride ADD COLUMN driver_alternate_number_hash bytea ;
ALTER TABLE atlas_app.ride ADD COLUMN driver_alternate_number_encrypted text ;


------- SQL updates -------

ALTER TABLE atlas_app.ride ADD COLUMN is_already_fav boolean ;
ALTER TABLE atlas_app.ride ADD COLUMN fav_count integer ;


------- SQL updates -------

ALTER TABLE atlas_app.ride ALTER COLUMN is_already_fav SET NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_app.ride ALTER COLUMN is_already_fav DROP NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_app.ride ADD COLUMN mobile_number_hash text NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_app.ride DROP COLUMN mobile_number_hash;


------- SQL updates -------

ALTER TABLE atlas_app.ride ADD COLUMN safety_journey_status text ;


------- SQL updates -------

ALTER TABLE atlas_app.ride ADD COLUMN destination_reached_at timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_app.ride ADD COLUMN estimated_end_time_range_start timestamp with time zone ;
ALTER TABLE atlas_app.ride ADD COLUMN estimated_end_time_range_end timestamp with time zone ;

ALTER TABLE atlas_app.ride ADD COLUMN tip_amount numeric(30,2) ;

------- SQL updates -------

ALTER TABLE atlas_app.ride ADD COLUMN payment_status text ;




------- SQL updates -------

ALTER TABLE atlas_app.ride ADD COLUMN has_stops boolean ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.ride ADD COLUMN was_ride_safe boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.ride ADD COLUMN feedback_skipped boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.ride ADD COLUMN pickup_route_call_count integer ;


------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.ride ADD COLUMN talked_with_driver boolean ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

