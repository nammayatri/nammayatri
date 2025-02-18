CREATE TABLE atlas_driver_offer_bpp.driver_quote ();

ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN backend_app_version text ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN backend_config_version text ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN client_bundle_version text ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN client_config_version text ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN client_os_type text ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN client_os_version text ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN client_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN client_sdk_version text ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN distance integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN distance_to_pickup bigint NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN driver_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN driver_rating double precision ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN duration_to_pickup bigint NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN estimate_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN estimated_fare integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN estimated_fare_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN fare_parameters_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN go_home_request_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN provider_id character varying(36) NOT NULL default '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f';
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN request_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN search_request_for_driver_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN search_try_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN special_location_tag text ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN status character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN trip_category text ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN valid_till timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN vehicle_service_tier text ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN vehicle_variant character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN vehicle_service_tier_name text ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN merchant_operating_city_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN distance_unit character varying(255) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN client_model_name text ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN client_manufacturer text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_quote ALTER COLUMN client_id TYPE text;