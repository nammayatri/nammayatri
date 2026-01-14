CREATE TABLE atlas_app.merchant_service_usage_config ();

ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN aadhaar_verification_service character varying(30) NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN auto_complete character varying(30) NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN enable_dashboard_sms boolean NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN get_distances character varying(30) NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN get_distances_for_cancel_ride text NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN get_exophone character varying (255) NOT NULL default 'Exotel';
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN get_pickup_routes text NOT NULL default 'Google';
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN get_place_details character varying(30) NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN get_place_name character varying(30) NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN get_routes character varying(30) NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN get_trip_routes text NOT NULL default 'Google';
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN initiate_call character varying(30) NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN issue_ticket_service character varying(30) NOT NULL default 'Kapture';
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN notify_person text NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN sms_providers_priority_list text[] NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN snap_to_road character varying(30) NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN use_fraud_detection boolean NOT NULL default False;
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN whatsapp_providers_priority_list text[] NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ADD PRIMARY KEY ( merchant_operating_city_id);


------- SQL updates -------

ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN update_payment_method_in_intent text NOT NULL default 'Stripe';
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN update_amount_in_payment_intent text NOT NULL default 'Stripe';
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN get_card_list text NOT NULL default 'Stripe';
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN create_setup_intent text NOT NULL default 'Stripe';
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN create_payment_intent text NOT NULL default 'Stripe';
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN create_payment_customer text NOT NULL default 'Stripe';
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN create_ephemeral_keys text NOT NULL default 'Stripe';
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN capture_payment_intent text NOT NULL default 'Stripe';


------- SQL updates -------

ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN delete_card text NOT NULL default 'Stripe';


------- SQL updates -------

ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN get_distances_for_scheduled_rides text  default 'OSRM';


------- SQL updates -------

ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN get_distances_for_scheduled_rides SET NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN cancel_payment_intent text  default 'Stripe';


------- SQL updates -------

ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN get_frfs_autocomplete_distances text  default 'OSRM';


------- SQL updates -------

ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN get_multi_modal_service text  default 'OTPTransit';


------- SQL updates -------

ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN get_first_pickup_route text ;


------- SQL updates -------

ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN get_multimodal_walk_distance text  default 'OSRM';


------- SQL updates -------

ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN insurance_service character varying(30)  default 'Acko';



------- SQL updates -------

ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN create_refunds text  default 'Stripe';
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN get_refunds text  default 'Stripe';