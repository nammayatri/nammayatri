CREATE TABLE atlas_driver_offer_bpp.ride ();

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN booking_id character(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN chargeable_distance integer ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN client_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN distance_calculation_failed boolean ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN driver_arrival_time timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN driver_deviated_from_route boolean ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN driver_go_home_request_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN driver_id character(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN enable_frequent_location_updates boolean ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN end_odometer_reading_file_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN end_odometer_reading_value double precision ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN end_otp text ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN fare integer ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN fare_parameters_id character(36) ;

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN id character(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN is_free_ride boolean ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN merchant_id character(36) ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN merchant_operating_city_id character(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN number_of_deviation boolean ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN number_of_osrm_snap_to_road_calls integer ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN number_of_snap_to_road_calls integer ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN otp character(4) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN pickup_drop_outside_of_threshold boolean ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN safety_alert_triggered boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN short_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN start_odometer_reading_file_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN start_odometer_reading_value double precision ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN status character varying(255) NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN tracking_url character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN traveled_distance double precision NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN trip_end_lat double precision ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN trip_end_lon double precision ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN trip_end_time timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN trip_start_lat double precision ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN trip_start_lon double precision ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN trip_start_time timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN ui_distance_calculation_with_accuracy integer ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN ui_distance_calculation_without_accuracy integer ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.ride ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN toll_charges double precision ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ALTER COLUMN merchant_operating_city_id DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN vehicle_service_tier_seating_capacity integer ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN vehicle_service_tier_air_conditioned double precision ;

------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN ride_ended_by text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN client_sdk_version text ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN client_os_version text ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN client_os_type text ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN client_config_version text ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN client_bundle_version text ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN backend_config_version text ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN backend_app_version text ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN traveled_distance_value text ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN distance_unit text ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN chargeable_distance_value text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ALTER COLUMN traveled_distance_value SET DEFAULT 0.0;