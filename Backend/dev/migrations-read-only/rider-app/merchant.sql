CREATE TABLE atlas_app.merchant ();

ALTER TABLE atlas_app.merchant ADD COLUMN aadhaar_key_expiry_time integer NOT NULL;
ALTER TABLE atlas_app.merchant ADD COLUMN aadhaar_verification_try_limit integer NOT NULL;
ALTER TABLE atlas_app.merchant ADD COLUMN arrived_pickup_threshold double precision  default 50;
ALTER TABLE atlas_app.merchant ADD COLUMN bap_id text NOT NULL;
ALTER TABLE atlas_app.merchant ADD COLUMN bap_unique_key_id text NOT NULL;
ALTER TABLE atlas_app.merchant ADD COLUMN cipher_text text  default 'TXlTZWNyZXRLZXkxMjM0NQo=';
ALTER TABLE atlas_app.merchant ADD COLUMN country text NOT NULL default 'India';
ALTER TABLE atlas_app.merchant ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.merchant ADD COLUMN city text NOT NULL default 'Kochi';
ALTER TABLE atlas_app.merchant ADD COLUMN state text NOT NULL;
ALTER TABLE atlas_app.merchant ADD COLUMN driver_distance_threshold_from_pickup double precision NOT NULL default 100;
ALTER TABLE atlas_app.merchant ADD COLUMN driver_offer_api_key varchar(128) NOT NULL;
ALTER TABLE atlas_app.merchant ADD COLUMN driver_offer_base_url text NOT NULL;
ALTER TABLE atlas_app.merchant ADD COLUMN driver_offer_merchant_id varchar(255) NOT NULL;
ALTER TABLE atlas_app.merchant ADD COLUMN driver_on_the_way_notify_expiry integer  default 3600;
ALTER TABLE atlas_app.merchant ADD COLUMN edit_pickup_distance_threshold double precision NOT NULL default 100;
ALTER TABLE atlas_app.merchant ADD COLUMN fake_otp_mobile_numbers text[] NOT NULL;
ALTER TABLE atlas_app.merchant ADD COLUMN fallback_short_id text NOT NULL;
ALTER TABLE atlas_app.merchant ADD COLUMN gateway_url text NOT NULL;
ALTER TABLE atlas_app.merchant ADD COLUMN geo_hash_precision_value INT NOT NULL default 9;
ALTER TABLE atlas_app.merchant ADD COLUMN destination_restriction text[] NOT NULL;
ALTER TABLE atlas_app.merchant ADD COLUMN origin_restriction text[] NOT NULL;
ALTER TABLE atlas_app.merchant ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.merchant ADD COLUMN is_avoid_toll boolean NOT NULL default true;
ALTER TABLE atlas_app.merchant ADD COLUMN kapture_disposition text NOT NULL;
ALTER TABLE atlas_app.merchant ADD COLUMN media_file_size_upper_limit int NOT NULL default 10000000;
ALTER TABLE atlas_app.merchant ADD COLUMN media_file_url_pattern text NOT NULL default 'http://localhost:8013/v2/<DOMAIN>/media?filePath=<FILE_PATH>';
ALTER TABLE atlas_app.merchant ADD COLUMN minimum_driver_rates_count int NOT NULL;
ALTER TABLE atlas_app.merchant ADD COLUMN name character varying(255) NOT NULL;
ALTER TABLE atlas_app.merchant ADD COLUMN num_of_allowed_edit_pickup_location_attempts_threshold int NOT NULL default 2;
ALTER TABLE atlas_app.merchant ADD COLUMN public_media_file_url_pattern text NOT NULL default 'http://localhost:8013/v2/<DOMAIN>/media?filePath=<FILE_PATH>';
ALTER TABLE atlas_app.merchant ADD COLUMN registry_url text NOT NULL;
ALTER TABLE atlas_app.merchant ADD COLUMN schedule_ride_buffer_time integer NOT NULL default 1800;
ALTER TABLE atlas_app.merchant ADD COLUMN short_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.merchant ADD COLUMN signature_expiry int NOT NULL;
ALTER TABLE atlas_app.merchant ADD COLUMN signing_public_key text NOT NULL;
ALTER TABLE atlas_app.merchant ADD COLUMN subscriber_id character(36) NOT NULL;
ALTER TABLE atlas_app.merchant ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.merchant ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.merchant ADD COLUMN edit_pickup_distance_threshold_value double precision ;
ALTER TABLE atlas_app.merchant ADD COLUMN driver_distance_threshold_from_pickup_value double precision ;
ALTER TABLE atlas_app.merchant ADD COLUMN distanc_unit character varying(255) ;
ALTER TABLE atlas_app.merchant ADD COLUMN arrived_pickup_threshold_value double precision ;


------- SQL updates -------

ALTER TABLE atlas_app.merchant ADD COLUMN distance_unit character varying(255) ;
ALTER TABLE atlas_app.merchant DROP COLUMN distanc_unit;


------- SQL updates -------

ALTER TABLE atlas_app.merchant ADD COLUMN fake_otp_emails text[] NOT NULL default '{}';


------- SQL updates -------

ALTER TABLE atlas_app.merchant ADD COLUMN arriving_pickup_threshold double precision NOT NULL default 100;


------- SQL updates -------

ALTER TABLE atlas_app.merchant ADD COLUMN online_payment boolean NOT NULL default false;


------- SQL updates -------

ALTER TABLE atlas_app.merchant ADD COLUMN num_of_allowed_edit_location_attempts_threshold int NOT NULL default 3;


------- SQL updates -------

ALTER TABLE atlas_app.merchant ADD COLUMN gateway_and_registry_priority_list text[]  default '{"NY"}';



------- SQL updates -------

ALTER TABLE atlas_app.merchant ADD COLUMN enable_for_multiple_search_issue boolean  default true;


------- SQL updates -------

ALTER TABLE atlas_app.merchant ADD COLUMN stuck_ride_auto_cancellation_buffer integer;


------- SQL updates -------

ALTER TABLE atlas_app.merchant ADD COLUMN signing_private_key text ;


------- SQL updates -------

