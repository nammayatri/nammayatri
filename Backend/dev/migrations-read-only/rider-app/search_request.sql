CREATE TABLE atlas_app.search_request ();

ALTER TABLE atlas_app.search_request ADD COLUMN auto_assign_enabled boolean ;
ALTER TABLE atlas_app.search_request ADD COLUMN auto_assign_enabled_v2 boolean ;
ALTER TABLE atlas_app.search_request ADD COLUMN available_payment_methods character(36) [] NOT NULL;
ALTER TABLE atlas_app.search_request ADD COLUMN bundle_version text ;
ALTER TABLE atlas_app.search_request ADD COLUMN client_id character varying(36) ;
ALTER TABLE atlas_app.search_request ADD COLUMN client_version text ;
ALTER TABLE atlas_app.search_request ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.search_request ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_app.search_request ADD COLUMN customer_extra_fee integer ;
ALTER TABLE atlas_app.search_request ADD COLUMN customer_extra_fee_amount double precision ;
ALTER TABLE atlas_app.search_request ADD COLUMN device text ;
ALTER TABLE atlas_app.search_request ADD COLUMN disability_tag character(255) ;
ALTER TABLE atlas_app.search_request ADD COLUMN distance numeric(30,2) ;
ALTER TABLE atlas_app.search_request ADD COLUMN estimated_ride_duration integer ;
ALTER TABLE atlas_app.search_request ADD COLUMN from_location_id character varying(36) ;
ALTER TABLE atlas_app.search_request ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.search_request ADD COLUMN language character varying(255) ;
ALTER TABLE atlas_app.search_request ADD COLUMN max_distance double precision ;
ALTER TABLE atlas_app.search_request ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.search_request ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.search_request ADD COLUMN rider_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.search_request ADD COLUMN rider_preferred_option text NOT NULL;
ALTER TABLE atlas_app.search_request ADD COLUMN selected_payment_method_id character varying(36) ;
ALTER TABLE atlas_app.search_request ADD COLUMN start_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.search_request ADD COLUMN to_location_id character varying(36) ;
ALTER TABLE atlas_app.search_request ADD COLUMN valid_till timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.search_request ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.search_request ALTER COLUMN rider_preferred_option DROP NOT NULL;
ALTER TABLE atlas_app.search_request ALTER COLUMN merchant_operating_city_id DROP NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_app.search_request ADD COLUMN max_distance_value double precision ;
ALTER TABLE atlas_app.search_request ADD COLUMN distance_value double precision ;
ALTER TABLE atlas_app.search_request ADD COLUMN distance_unit character varying(255) ;


------- SQL updates -------

ALTER TABLE atlas_app.search_request ADD COLUMN client_sdk_version text ;
ALTER TABLE atlas_app.search_request ADD COLUMN client_os_version text ;
ALTER TABLE atlas_app.search_request ADD COLUMN client_os_type text ;
ALTER TABLE atlas_app.search_request ADD COLUMN client_config_version text ;
ALTER TABLE atlas_app.search_request ADD COLUMN client_bundle_version text ;
ALTER TABLE atlas_app.search_request ADD COLUMN backend_config_version text ;
ALTER TABLE atlas_app.search_request ADD COLUMN backend_app_version text ;
ALTER TABLE atlas_app.search_request DROP COLUMN client_version;
ALTER TABLE atlas_app.search_request DROP COLUMN bundle_version;


------- SQL updates -------

ALTER TABLE atlas_app.search_request ADD COLUMN return_time timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_app.search_request ADD COLUMN round_trip boolean ;
ALTER TABLE atlas_app.search_request ADD COLUMN is_advance_booking_enabled boolean ;



------- SQL updates -------

ALTER TABLE atlas_app.search_request ALTER COLUMN selected_payment_method_id TYPE text;

--- Drop columns section begins. Please be careful while running ---
ALTER TABLE atlas_app.search_request DROP COLUMN available_payment_methods;
--- Drop columns section ends ---
ALTER TABLE atlas_app.search_request ADD COLUMN total_rides_count integer ;



------- SQL updates -------

ALTER TABLE atlas_app.search_request ADD COLUMN client_model_name text ;
ALTER TABLE atlas_app.search_request ADD COLUMN client_manufacturer text ;


------- SQL updates -------

ALTER TABLE atlas_app.search_request ADD COLUMN is_dashboard_request boolean ;
ALTER TABLE atlas_app.search_request ADD COLUMN available_payment_methods text[] NOT NULL;



------- SQL updates -------

ALTER TABLE atlas_app.search_request ADD COLUMN estimated_ride_static_duration integer ;


------- SQL updates -------

ALTER TABLE atlas_app.search_request ADD COLUMN place_name_source text ;
ALTER TABLE atlas_app.search_request ADD COLUMN initiated_by text ;


------- SQL updates -------

ALTER TABLE atlas_app.search_request ADD COLUMN stops text ;


------- SQL updates -------

ALTER TABLE atlas_app.search_request ADD COLUMN has_stops boolean ;
ALTER TABLE atlas_app.search_request ADD COLUMN skip_mode boolean ;
ALTER TABLE atlas_app.search_request ADD COLUMN skip_booking boolean ;
ALTER TABLE atlas_app.search_request ADD COLUMN journey_leg_order integer ;
ALTER TABLE atlas_app.search_request ADD COLUMN journey_id text ;
ALTER TABLE atlas_app.search_request ADD COLUMN convenience_cost integer ;
ALTER TABLE atlas_app.search_request ADD COLUMN agency text ;





------- SQL updates -------

ALTER TABLE atlas_app.search_request ADD COLUMN pricing_id text ;



------- SQL updates -------

ALTER TABLE atlas_app.search_request ADD COLUMN driver_identifier_value text ;
ALTER TABLE atlas_app.search_request ADD COLUMN driver_identifier_type text ;


------- SQL updates -------

ALTER TABLE atlas_app.search_request ADD COLUMN client_react_native_version text ;


------- SQL updates -------

ALTER TABLE atlas_app.search_request ADD COLUMN has_multimodal_search boolean  default false;


------- SQL updates -------

ALTER TABLE atlas_app.search_request ADD COLUMN is_deleted boolean ;





------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.search_request ADD COLUMN config_in_experiment_versions json ;


------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.search_request ADD COLUMN is_meter_ride_search boolean ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.search_request ADD COLUMN recent_location_id character varying(36) ;
ALTER TABLE atlas_app.search_request ADD COLUMN route_code text ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------


ALTER TABLE atlas_app.search_request ADD COLUMN origin_stop_code text ;
ALTER TABLE atlas_app.search_request ADD COLUMN destination_stop_code text ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.search_request ADD COLUMN all_journeys_loaded boolean ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.search_request ADD COLUMN vehicle_category text ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.search_request ADD COLUMN on_search_failed boolean ;