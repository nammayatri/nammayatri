CREATE TABLE atlas_driver_offer_bpp.person ();

ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN alternate_mobile_number_encrypted character varying (255) ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN alternate_mobile_number_hash bytea ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN backend_app_version text ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN backend_config_version text ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN client_bundle_version text ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN client_config_version text ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN client_os_type text ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN client_os_version text ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN client_sdk_version text ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN description character varying (255) ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN device_token character varying (255) ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN driver_tag text[] ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN email character varying (255) ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN face_image_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN first_name character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN gender character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN hometown character varying (255) ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN identifier character varying (255) ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN identifier_type character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN is_new boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN language character varying (255) ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN languages_spoken text[]  default '{}';
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN last_name character varying (255) ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN merchant_id character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN middle_name character varying (255) ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN mobile_country_code character varying (255) ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN mobile_number_encrypted character varying (255) ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN mobile_number_hash bytea ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN onboarded_from_dashboard boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN password_hash bytea ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN rating double precision ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN registration_lat double precision ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN registration_lon double precision ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN role character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN total_earned_coins integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN unencrypted_alternate_mobile_number text ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN unencrypted_mobile_number text ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN use_fake_otp text ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN used_coins integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN whatsapp_notification_enroll_status character varying (255) ;
ALTER TABLE atlas_driver_offer_bpp.person ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN total_ratings integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN total_rating_score integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN is_valid_rating boolean NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.person DROP COLUMN rating;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.person ALTER COLUMN total_ratings DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person ALTER COLUMN total_rating_score DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person ALTER COLUMN is_valid_rating DROP NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.person DROP COLUMN total_ratings;
ALTER TABLE atlas_driver_offer_bpp.person DROP COLUMN total_rating_score;
ALTER TABLE atlas_driver_offer_bpp.person DROP COLUMN is_valid_rating;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN client_model_name text ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN client_manufacturer text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN client_id text ;


------- SQL updates -------

