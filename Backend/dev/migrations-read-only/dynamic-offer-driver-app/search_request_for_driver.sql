CREATE TABLE atlas_driver_offer_bpp.search_request_for_driver ();

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN acceptance_ratio real ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN actual_distance_to_pickup bigint NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN air_conditioned boolean ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN backend_app_version text ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN backend_config_version text ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN base_fare integer ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN base_fare_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN batch_number integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN cancellation_ratio real ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN client_bundle_version text ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN client_config_version text ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN client_os_type text ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN client_os_version text ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN client_sdk_version text ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN customer_cancellation_dues double precision ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN driver_available_time real ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN driver_default_step_fee integer ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN driver_default_step_fee_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN driver_max_extra_fee double precision ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN driver_max_extra_fee_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN driver_min_extra_fee double precision ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN driver_min_extra_fee_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN driver_speed double precision ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN driver_step_fee integer ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN driver_step_fee_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN duration_to_pickup bigint NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN estimate_id character varying (36) ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN go_home_request_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN is_part_of_intelligent_pool boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN keep_hidden_for_seconds integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN lat double precision ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN lon double precision ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN mode text ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN parallel_search_request_count smallint ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN pickup_zone boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN request_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN response character varying (255) ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN ride_frequency_score double precision ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN ride_request_popup_delay_duration integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN search_request_valid_till timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN search_try_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN start_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN status character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN straight_line_distance_to_pickup bigint NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN vehicle_service_tier text ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN vehicle_service_tier_name text ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN vehicle_variant character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN is_forward_request boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN distance_unit character varying(255) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN notification_source text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN total_rides integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN vehicle_age int ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN client_model_name text ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN client_manufacturer text ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN previous_drop_geo_hash text ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN updated_at timestamp with time zone  default CURRENT_TIMESTAMP;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN responded_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN rendered_at timestamp with time zone ;

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN from_loc_geohash text ;





------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN driver_tags json ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN customer_tags json ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN pooling_logic_version integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN pooling_config_version integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN upgrade_cab_request boolean ;

------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN is_favourite boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN middle_stop_count integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN last_ride_id character varying(36) ;


------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN parcel_type text ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN parcel_quantity integer ;




------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN driver_tag_score json ;