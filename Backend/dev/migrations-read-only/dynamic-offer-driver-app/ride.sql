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

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN toll_names text[] ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN number_of_self_tuned integer ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN estimated_toll_names text[] ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN estimated_toll_charges double precision ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN driver_deviated_to_toll_route boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN fare_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN currency character varying(255) ;




------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN toll_confidence text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN trip_category text ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN previous_ride_trip_end_lon double precision ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN previous_ride_trip_end_lat double precision ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN is_advance_booking boolean  default false;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN vehicle_variant text ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN vehicle_service_tier_name text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN distance_unit character varying(255) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN is_air_conditioned boolean ;

------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN online_payment boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN enable_otp_less_ride boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN client_model_name text ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN client_manufacturer text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN cancellation_fee_if_cancelled double precision ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN previous_ride_trip_end_time timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN tip_amount double precision;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN passed_through_destination boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN delivery_file_ids text[] ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN destination_reached_at timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN estimated_end_time_range_start timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN estimated_end_time_range_end timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN ride_tags text[] ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN has_stops boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN is_pickup_or_destination_edited boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN is_driver_special_loc_warrior boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride ALTER COLUMN client_id TYPE text;