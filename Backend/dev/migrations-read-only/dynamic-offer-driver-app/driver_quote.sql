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
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN distance integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN distance_to_pickup integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN driver_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN driver_rating text ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN duration_to_pickup integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN estimate_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN estimated_fare integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN go_home_request_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN provider_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN request_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN search_request_for_driver_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN search_try_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN special_location_tag text ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN trip_category text ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN valid_till timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN vehicle_variant text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN vehicle_service_tier text ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN fare_params_id character varying(36) NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN fare_params text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote DROP COLUMN fare_params_id;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN fare_parameters_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote DROP COLUMN fare_params;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_quote ALTER COLUMN estimated_fare TYPE double precision;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN currency character varying(255) ;