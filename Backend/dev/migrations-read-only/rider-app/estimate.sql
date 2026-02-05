CREATE TABLE atlas_app.estimate ();

ALTER TABLE atlas_app.estimate ADD COLUMN backend_app_version text ;
ALTER TABLE atlas_app.estimate ADD COLUMN backend_config_version text ;
ALTER TABLE atlas_app.estimate ADD COLUMN bpp_estimate_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN client_bundle_version text ;
ALTER TABLE atlas_app.estimate ADD COLUMN client_config_version text ;
ALTER TABLE atlas_app.estimate ADD COLUMN client_os_type text ;
ALTER TABLE atlas_app.estimate ADD COLUMN client_os_version text ;
ALTER TABLE atlas_app.estimate ADD COLUMN client_sdk_version text ;
ALTER TABLE atlas_app.estimate ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.estimate ADD COLUMN device text ;
ALTER TABLE atlas_app.estimate ADD COLUMN discount double precision ;
ALTER TABLE atlas_app.estimate ADD COLUMN drivers_location text[] NOT NULL;

ALTER TABLE atlas_app.estimate ADD COLUMN distance_unit character varying(255) ;
ALTER TABLE atlas_app.estimate ADD COLUMN estimated_distance integer ;
ALTER TABLE atlas_app.estimate ADD COLUMN estimated_distance_value double precision ;
ALTER TABLE atlas_app.estimate ADD COLUMN estimated_duration integer ;
ALTER TABLE atlas_app.estimate ADD COLUMN estimated_fare numeric(30,10) NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN estimated_total_fare numeric(30,2) NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN item_id text NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.estimate ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.estimate ADD COLUMN night_shift_charge integer ;
ALTER TABLE atlas_app.estimate ADD COLUMN night_shift_charge_amount double precision ;
ALTER TABLE atlas_app.estimate ADD COLUMN night_shift_end time without time zone ;
ALTER TABLE atlas_app.estimate ADD COLUMN night_shift_start time without time zone ;
ALTER TABLE atlas_app.estimate ADD COLUMN old_night_shift_charge text ;
ALTER TABLE atlas_app.estimate ADD COLUMN provider_completed_rides_count integer NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN provider_id character varying(255) NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN provider_mobile_number character varying(255) NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN provider_name character varying(255) NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN provider_url character varying(255) NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN request_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN service_tier_name text ;
ALTER TABLE atlas_app.estimate ADD COLUMN service_tier_short_desc text ;
ALTER TABLE atlas_app.estimate ADD COLUMN special_location_tag text ;
ALTER TABLE atlas_app.estimate ADD COLUMN status character varying(255) NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_app.estimate ADD COLUMN max_total_fare numeric(30,2) NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN min_total_fare numeric(30,2) NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN trip_terms_id character varying(36) ;
ALTER TABLE atlas_app.estimate ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.estimate ADD COLUMN valid_till timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN vehicle_variant character varying(255) NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN waiting_charge_per_min double precision ;
ALTER TABLE atlas_app.estimate ADD COLUMN waiting_charge_per_min_amount double precision ;
ALTER TABLE atlas_app.estimate ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN toll_names text[] ;
ALTER TABLE atlas_app.estimate ADD COLUMN toll_charges double precision ;
ALTER TABLE atlas_app.estimate ADD COLUMN is_customer_preffered_search_route boolean ;
ALTER TABLE atlas_app.estimate ADD COLUMN is_blocked_route boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN estimated_pickup_duration integer ;


------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN vehicle_service_tier_seating_capacity integer ;
ALTER TABLE atlas_app.estimate ADD COLUMN vehicle_service_tier_air_conditioned double precision ;


------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN special_location_name text ;


------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN is_air_conditioned boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN client_model_name text ;
ALTER TABLE atlas_app.estimate ADD COLUMN client_manufacturer text ;


------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN fare_product_type text ;


------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN trip_category text ;


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




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN estimated_static_duration integer ;


------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN vehicle_icon_url text ;


------- SQL updates -------

ALTER TABLE atlas_app.estimate ALTER COLUMN vehicle_icon_url TYPE character varying(255);


------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN smart_tip_suggestion double precision ;


------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN smart_tip_reason text ;


------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN tip_options integer[] ;


------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN is_insured boolean ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN insured_amount text ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN is_multimodal_search boolean ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN boost_search_pre_selection_service_tier_config text[] ;


------- SQL updates -------
ALTER TABLE atlas_app.estimate ADD COLUMN vehicle_category text ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN estimate_tags text[] ;


------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN qar double precision ;


------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN business_discount_percentage double precision ;
ALTER TABLE atlas_app.estimate ADD COLUMN business_discount double precision ;



------- SQL updates -------

-- ALTER TABLE atlas_app.estimate ALTER COLUMN boost_search_pre_selection_service_tier_config TYPE text;


------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN personal_discount_percentage double precision ;
ALTER TABLE atlas_app.estimate ADD COLUMN personal_discount double precision ;


------- SQL updates -------

