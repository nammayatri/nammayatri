CREATE TABLE atlas_driver_offer_bpp.driver_information ();

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN aadhaar_verified boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN active boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN admin_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN auto_pay_status text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN available_upi_apps text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN block_expiry_time timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN block_state_modifier text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN blocked boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN blocked_reason text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN can_downgrade_to_hatchback boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN can_downgrade_to_sedan boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN can_downgrade_to_taxi boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN can_switch_to_rental boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN comp_aadhaar_image_path text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN driver_dob timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN enabled boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN enabled_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN last_enabled_on timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN mode text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN num_of_locks integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN on_ride boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN payer_vpa text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN payment_pending boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN referral_code text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN referred_by_driver_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN subscribed boolean NOT NULL default true;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN total_referred integer  default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN verified boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD PRIMARY KEY ( driver_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN air_condition_score double precision ;