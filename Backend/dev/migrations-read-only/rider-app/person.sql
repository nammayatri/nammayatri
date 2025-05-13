CREATE TABLE atlas_app.person ();

ALTER TABLE atlas_app.person ADD COLUMN aadhaar_verified boolean NOT NULL default false;
ALTER TABLE atlas_app.person ADD COLUMN backend_app_version text ;
ALTER TABLE atlas_app.person ADD COLUMN blocked boolean NOT NULL;
ALTER TABLE atlas_app.person ADD COLUMN blocked_at timestamp with time zone ;
ALTER TABLE atlas_app.person ADD COLUMN blocked_by_rule_id character varying(36) ;
ALTER TABLE atlas_app.person ADD COLUMN client_bundle_version text ;
ALTER TABLE atlas_app.person ADD COLUMN client_config_version text ;
ALTER TABLE atlas_app.person ADD COLUMN client_os_type text ;
ALTER TABLE atlas_app.person ADD COLUMN client_os_version text ;
ALTER TABLE atlas_app.person ADD COLUMN client_sdk_version text ;
ALTER TABLE atlas_app.person ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.person ADD COLUMN current_city text ;
ALTER TABLE atlas_app.person ADD COLUMN customer_referral_code text ;
ALTER TABLE atlas_app.person ADD COLUMN description character varying(255) ;
ALTER TABLE atlas_app.person ADD COLUMN device_token character varying(255) ;
ALTER TABLE atlas_app.person ADD COLUMN email_encrypted character varying(255) ;
ALTER TABLE atlas_app.person ADD COLUMN email_hash bytea ;
ALTER TABLE atlas_app.person ADD COLUMN enabled boolean NOT NULL default true;
ALTER TABLE atlas_app.person ADD COLUMN false_safety_alarm_count integer ;
ALTER TABLE atlas_app.person ADD COLUMN first_name character varying(255) ;
ALTER TABLE atlas_app.person ADD COLUMN follows_ride boolean NOT NULL default false;
ALTER TABLE atlas_app.person ADD COLUMN gender character varying(255) NOT NULL;
ALTER TABLE atlas_app.person ADD COLUMN has_completed_mock_safety_drill boolean  default false;
ALTER TABLE atlas_app.person ADD COLUMN has_completed_safety_setup boolean NOT NULL default false;
ALTER TABLE atlas_app.person ADD COLUMN has_disability boolean ;
ALTER TABLE atlas_app.person ADD COLUMN has_taken_valid_ride boolean NOT NULL;
ALTER TABLE atlas_app.person ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.person ADD COLUMN identifier character varying(255) ;
ALTER TABLE atlas_app.person ADD COLUMN identifier_type character varying(255) NOT NULL;
ALTER TABLE atlas_app.person ADD COLUMN is_new boolean NOT NULL;
ALTER TABLE atlas_app.person ADD COLUMN is_valid_rating boolean NOT NULL;
ALTER TABLE atlas_app.person ADD COLUMN language character varying(255) ;
ALTER TABLE atlas_app.person ADD COLUMN last_name character varying(255) ;
ALTER TABLE atlas_app.person ADD COLUMN merchant_id character varying(36) NOT NULL default 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51';
ALTER TABLE atlas_app.person ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.person ADD COLUMN middle_name character varying(255) ;
ALTER TABLE atlas_app.person ADD COLUMN mobile_country_code character varying(255) ;
ALTER TABLE atlas_app.person ADD COLUMN mobile_number_encrypted character varying(255) ;
ALTER TABLE atlas_app.person ADD COLUMN mobile_number_hash bytea ;
ALTER TABLE atlas_app.person ADD COLUMN night_safety_checks boolean NOT NULL default true;
ALTER TABLE atlas_app.person ADD COLUMN notification_token character varying(255) ;
ALTER TABLE atlas_app.person ADD COLUMN password_hash bytea ;

ALTER TABLE atlas_app.person ADD COLUMN referral_code character varying(15) ;
ALTER TABLE atlas_app.person ADD COLUMN referred_at timestamp with time zone ;
ALTER TABLE atlas_app.person ADD COLUMN referred_by_customer text ;
ALTER TABLE atlas_app.person ADD COLUMN registration_lat double precision ;
ALTER TABLE atlas_app.person ADD COLUMN registration_lon double precision ;
ALTER TABLE atlas_app.person ADD COLUMN role character varying(255) NOT NULL;
ALTER TABLE atlas_app.person ADD COLUMN safety_center_disabled_on_date timestamp with time zone ;
ALTER TABLE atlas_app.person ADD COLUMN share_emergency_contacts boolean NOT NULL default false;
ALTER TABLE atlas_app.person ADD COLUMN share_trip_with_emergency_contact_option text ;
ALTER TABLE atlas_app.person ADD COLUMN total_rating_score integer NOT NULL default 0;
ALTER TABLE atlas_app.person ADD COLUMN total_ratings integer NOT NULL default 0;
ALTER TABLE atlas_app.person ADD COLUMN unencrypted_mobile_number character varying(255) ;
ALTER TABLE atlas_app.person ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.person ADD COLUMN use_fake_otp text ;
ALTER TABLE atlas_app.person ADD COLUMN whatsapp_notification_enroll_status character varying(255) ;
ALTER TABLE atlas_app.person ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.person ADD COLUMN blocked_count integer ;


------- SQL updates -------

ALTER TABLE atlas_app.person ADD COLUMN registered_via_partner_org_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_app.person ADD COLUMN customer_payment_id text ;


------- SQL updates -------

ALTER TABLE atlas_app.person ADD COLUMN default_payment_method_id text ;
ALTER TABLE atlas_app.person ADD COLUMN total_rides_count integer ;



------- SQL updates -------

ALTER TABLE atlas_app.person ADD COLUMN enable_otp_less_ride boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.person ADD COLUMN device_id text ;


------- SQL updates -------

ALTER TABLE atlas_app.person ADD COLUMN client_model_name text ;
ALTER TABLE atlas_app.person ADD COLUMN client_manufacturer text ;


------- SQL updates -------

ALTER TABLE atlas_app.person ADD COLUMN inform_police_sos boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.person ADD COLUMN android_id text ;
ALTER TABLE atlas_app.person ADD COLUMN customer_namma_tags text[] ;



------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.person ADD COLUMN client_react_native_version text ;


------- SQL updates -------

ALTER TABLE atlas_app.person ADD COLUMN payout_vpa text ;





------- SQL updates -------

ALTER TABLE atlas_app.person ADD COLUMN frequent_loc_geohashes text[] ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.person ADD COLUMN verification_channel text ;
ALTER TABLE atlas_app.person ADD COLUMN profile_picture text ;
ALTER TABLE atlas_app.person ADD COLUMN date_of_birth timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_app.person ADD COLUMN juspay_customer_payment_id text ;


------- SQL updates -------

ALTER TABLE atlas_app.person ADD COLUMN live_activity_token text ;


------- SQL updates -------

ALTER TABLE atlas_app.person ADD COLUMN imei_number_hash bytea ;
ALTER TABLE atlas_app.person ADD COLUMN imei_number_encrypted character varying(255) ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.person ADD COLUMN blocked_until timestamp with time zone;
ALTER TABLE atlas_app.person ADD COLUMN auth_blocked boolean  default false;


------- SQL updates -------

ALTER TABLE atlas_app.person ADD COLUMN latest_lon double precision ;
ALTER TABLE atlas_app.person ADD COLUMN latest_lat double precision ;


------- SQL updates -------

ALTER TABLE atlas_app.person ALTER COLUMN blocked_until SET DEFAULT '';
ALTER TABLE atlas_app.person ADD COLUMN blocked_reason text ;
ALTER TABLE atlas_app.person ADD COLUMN block_source text ;


------- SQL updates -------

