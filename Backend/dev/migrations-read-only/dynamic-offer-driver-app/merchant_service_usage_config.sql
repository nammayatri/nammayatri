CREATE TABLE atlas_driver_offer_bpp.merchant_service_usage_config ();

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN aadhaar_verification_service character varying(30) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN auto_complete character varying(30) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN driver_background_verification_service character varying(30) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN face_verification_service character varying(30) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN get_distances character varying(30) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN get_distances_for_cancel_ride text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN get_estimated_pickup_distances text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN get_exophone character varying(255) NOT NULL default 'Exotel';
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN get_pickup_routes text NOT NULL default 'Google';
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN get_place_details character varying(30) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN get_place_name character varying(30) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN get_routes character varying(30) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN get_trip_routes text NOT NULL default 'Google';
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN initiate_call character varying(30) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN issue_ticket_service character varying(30) NOT NULL default 'Kapture';
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN rectify_distant_points_failure text NOT NULL default 'OSRM';
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN send_search_request_to_driver text[] NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN sms_providers_priority_list text[] NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN snap_to_road character varying(30) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN snap_to_road_providers_list text[] NOT NULL default '{"OSRM", "Google"}';
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN verification_providers_priority_list text[] NOT NULL default '{"Idfy"}';
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN verification_service character varying(30) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN whatsapp_providers_priority_list text[] NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD PRIMARY KEY ( merchant_operating_city_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN retry_bank_account_link text NOT NULL default 'Stripe';
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN get_bank_account text NOT NULL default 'Stripe';
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN create_bank_account text NOT NULL default 'Stripe';



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN background_verification text NOT NULL default 'Checkr';



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN sdk_verification_service text NOT NULL default 'HyperVerge';


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN get_distances_for_scheduled_rides text  default 'OSRM';


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ALTER COLUMN get_distances_for_scheduled_rides SET NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN llm_chat_completion character varying(30)  default 'AzureOpenAI';


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN pan_verification_service character varying(30)  default 'HyperVerge';


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN gst_verification_service character varying(30) ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN dashboard_pan_verification_service character varying(30) ;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN dashboard_gst_verification_service character varying(30) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN create_transfer text  default 'Stripe';