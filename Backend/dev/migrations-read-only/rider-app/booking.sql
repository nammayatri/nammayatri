CREATE TABLE atlas_app.booking ();

ALTER TABLE atlas_app.booking ADD COLUMN distance numeric(30,2) ;
ALTER TABLE atlas_app.booking ADD COLUMN fare_product_type character varying(255) NOT NULL default 'ONE_WAY';
ALTER TABLE atlas_app.booking ADD COLUMN otp_code character(4) ;
ALTER TABLE atlas_app.booking ADD COLUMN stop_location_id character varying(36) ;
ALTER TABLE atlas_app.booking ADD COLUMN to_location_id character(36) ;
ALTER TABLE atlas_app.booking ADD COLUMN bpp_booking_id character varying(36) ;
ALTER TABLE atlas_app.booking ADD COLUMN client_id character varying(36) ;
ALTER TABLE atlas_app.booking ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.booking ADD COLUMN discount numeric(30,2) ;
ALTER TABLE atlas_app.booking ADD COLUMN distance_unit character varying(255) ;
ALTER TABLE atlas_app.booking ADD COLUMN distance_value double precision ;
ALTER TABLE atlas_app.booking ADD COLUMN estimated_distance double precision ;
ALTER TABLE atlas_app.booking ADD COLUMN estimated_distance_value double precision ;
ALTER TABLE atlas_app.booking ADD COLUMN estimated_duration integer ;
ALTER TABLE atlas_app.booking ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_app.booking ADD COLUMN estimated_fare numeric(30,2) NOT NULL;
ALTER TABLE atlas_app.booking ADD COLUMN estimated_total_fare numeric(30,2) NOT NULL;
ALTER TABLE atlas_app.booking ADD COLUMN from_location_id character(36) ;
ALTER TABLE atlas_app.booking ADD COLUMN fulfillment_id text ;
ALTER TABLE atlas_app.booking ADD COLUMN id character(36) NOT NULL;

ALTER TABLE atlas_app.booking ADD COLUMN is_scheduled boolean ;
ALTER TABLE atlas_app.booking ADD COLUMN item_id text NOT NULL default '';
ALTER TABLE atlas_app.booking ADD COLUMN merchant_id character(36) NOT NULL default 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51';
ALTER TABLE atlas_app.booking ADD COLUMN merchant_operating_city_id character(36) ;
ALTER TABLE atlas_app.booking ADD COLUMN payment_method_id character(36) ;
ALTER TABLE atlas_app.booking ADD COLUMN payment_status text ;
ALTER TABLE atlas_app.booking ADD COLUMN payment_url text ;
ALTER TABLE atlas_app.booking ADD COLUMN primary_exophone character varying(255) NOT NULL;
ALTER TABLE atlas_app.booking ADD COLUMN provider_id character varying(255) NOT NULL;
ALTER TABLE atlas_app.booking ADD COLUMN provider_url character varying(255) NOT NULL;
ALTER TABLE atlas_app.booking ADD COLUMN quote_id character(36) ;
ALTER TABLE atlas_app.booking ADD COLUMN rider_id character(36) NOT NULL;
ALTER TABLE atlas_app.booking ADD COLUMN service_tier_name text ;
ALTER TABLE atlas_app.booking ADD COLUMN service_tier_short_desc text ;
ALTER TABLE atlas_app.booking ADD COLUMN special_location_tag text ;
ALTER TABLE atlas_app.booking ADD COLUMN start_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.booking ADD COLUMN status character varying(255) NOT NULL;
ALTER TABLE atlas_app.booking ADD COLUMN rider_transaction_id text NOT NULL;
ALTER TABLE atlas_app.booking ADD COLUMN trip_terms_id character(36) ;
ALTER TABLE atlas_app.booking ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.booking ADD COLUMN vehicle_variant character varying(60) NOT NULL;
ALTER TABLE atlas_app.booking ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.booking ADD COLUMN client_sdk_version text ;
ALTER TABLE atlas_app.booking ADD COLUMN client_os_version text ;
ALTER TABLE atlas_app.booking ADD COLUMN client_os_type text ;
ALTER TABLE atlas_app.booking ADD COLUMN client_config_version text ;
ALTER TABLE atlas_app.booking ADD COLUMN client_bundle_version text ;
ALTER TABLE atlas_app.booking ADD COLUMN backend_config_version text ;
ALTER TABLE atlas_app.booking ADD COLUMN backend_app_version text ;


------- SQL updates -------

ALTER TABLE atlas_app.booking ADD COLUMN round_trip boolean ;
ALTER TABLE atlas_app.booking ADD COLUMN return_time timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_app.booking ADD COLUMN vehicle_service_tier_seating_capacity integer ;
ALTER TABLE atlas_app.booking ADD COLUMN vehicle_service_tier_air_conditioned double precision ;


------- SQL updates -------

ALTER TABLE atlas_app.booking ADD COLUMN special_location_name text ;


------- SQL updates -------

ALTER TABLE atlas_app.booking ADD COLUMN estimate_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_app.booking ADD COLUMN is_air_conditioned boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.booking ALTER COLUMN payment_method_id TYPE text;


------- SQL updates -------

ALTER TABLE atlas_app.booking ADD COLUMN estimated_application_fee numeric(30,2) ;


------- SQL updates -------


--- Drop columns section begins. Please be careful while running ---
ALTER TABLE atlas_app.booking DROP COLUMN estimate_id;
--- Drop columns section ends ---



------- SQL updates -------

ALTER TABLE atlas_app.booking ADD COLUMN is_booking_updated boolean  default false;


------- SQL updates -------

ALTER TABLE atlas_app.booking ADD COLUMN client_model_name text ;
ALTER TABLE atlas_app.booking ADD COLUMN client_manufacturer text ;


------- SQL updates -------

ALTER TABLE atlas_app.booking ADD COLUMN is_dashboard_request boolean ;


------- SQL updates -------


ALTER TABLE atlas_app.booking ADD COLUMN trip_category text ;


------- SQL updates -------

ALTER TABLE atlas_app.booking ADD COLUMN initiated_by text ;




------- SQL updates -------

ALTER TABLE atlas_app.booking ADD COLUMN estimated_static_duration integer ;


------- SQL updates -------




------- SQL updates -------

