CREATE TABLE atlas_app.quote ();

ALTER TABLE atlas_app.quote ADD COLUMN backend_app_version text ;
ALTER TABLE atlas_app.quote ADD COLUMN backend_config_version text ;
ALTER TABLE atlas_app.quote ADD COLUMN client_bundle_version text ;
ALTER TABLE atlas_app.quote ADD COLUMN client_config_version text ;
ALTER TABLE atlas_app.quote ADD COLUMN client_os_type text ;
ALTER TABLE atlas_app.quote ADD COLUMN client_os_version text ;
ALTER TABLE atlas_app.quote ADD COLUMN client_sdk_version text ;
ALTER TABLE atlas_app.quote ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.quote ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_app.quote ADD COLUMN discount numeric(30,2) ;
ALTER TABLE atlas_app.quote ADD COLUMN distance_to_nearest_driver double precision ;
ALTER TABLE atlas_app.quote ADD COLUMN distance_to_nearest_driver_value double precision ;
ALTER TABLE atlas_app.quote ADD COLUMN distance_unit character varying(255) ;
ALTER TABLE atlas_app.quote ADD COLUMN driver_offer_id character(36) ;
ALTER TABLE atlas_app.quote ADD COLUMN fare_product_type character varying(255) NOT NULL default 'ONE_WAY';
ALTER TABLE atlas_app.quote ADD COLUMN rental_details_id text ;
ALTER TABLE atlas_app.quote ADD COLUMN special_zone_quote_id character(36) ;
ALTER TABLE atlas_app.quote ADD COLUMN estimated_fare numeric(30,10) NOT NULL;
ALTER TABLE atlas_app.quote ADD COLUMN estimated_total_fare numeric(30,2) NOT NULL;
ALTER TABLE atlas_app.quote ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.quote ADD COLUMN item_id text NOT NULL default '';
ALTER TABLE atlas_app.quote ADD COLUMN merchant_id character varying(36) NOT NULL default 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51';
ALTER TABLE atlas_app.quote ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.quote ADD COLUMN provider_id character varying(255) NOT NULL;
ALTER TABLE atlas_app.quote ADD COLUMN provider_url character varying(255) NOT NULL;

ALTER TABLE atlas_app.quote ADD COLUMN request_id character varying(255) NOT NULL;
ALTER TABLE atlas_app.quote ADD COLUMN service_tier_name text ;
ALTER TABLE atlas_app.quote ADD COLUMN service_tier_short_desc text ;
ALTER TABLE atlas_app.quote ADD COLUMN special_location_tag text ;
ALTER TABLE atlas_app.quote ADD COLUMN trip_terms_id character(36) ;
ALTER TABLE atlas_app.quote ADD COLUMN updated_at timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.quote ADD COLUMN valid_till timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.quote ADD COLUMN vehicle_variant text NOT NULL;
ALTER TABLE atlas_app.quote ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.quote ADD COLUMN is_customer_preffered_search_route boolean ;
ALTER TABLE atlas_app.quote ADD COLUMN is_blocked_route boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.quote ADD COLUMN toll_names text[] ;
ALTER TABLE atlas_app.quote ADD COLUMN toll_charges double precision ;


------- SQL updates -------

ALTER TABLE atlas_app.quote ADD COLUMN estimated_pickup_duration integer ;


------- SQL updates -------

ALTER TABLE atlas_app.quote ADD COLUMN vehicle_service_tier_seating_capacity integer ;
ALTER TABLE atlas_app.quote ADD COLUMN vehicle_service_tier_air_conditioned double precision ;


------- SQL updates -------

ALTER TABLE atlas_app.quote ADD COLUMN special_location_name text ;


------- SQL updates -------

ALTER TABLE atlas_app.quote ADD COLUMN is_air_conditioned boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.quote ADD COLUMN client_model_name text ;
ALTER TABLE atlas_app.quote ADD COLUMN client_manufacturer text ;


------- SQL updates -------

ALTER TABLE atlas_app.quote ADD COLUMN vehicle_model text ;