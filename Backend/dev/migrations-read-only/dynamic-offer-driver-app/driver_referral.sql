CREATE TABLE atlas_driver_offer_bpp.driver_referral ();

ALTER TABLE atlas_driver_offer_bpp.driver_referral ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_referral ADD COLUMN linked_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_referral ADD COLUMN referral_code character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_referral ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_referral ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_referral ADD PRIMARY KEY ( referral_code, driver_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_referral DROP CONSTRAINT driver_referral_pkey;
ALTER TABLE atlas_driver_offer_bpp.driver_referral ADD PRIMARY KEY ( referral_code);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_referral ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_referral ADD COLUMN merchant_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_referral ADD COLUMN dynamic_referral_code_valid_till timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_referral ADD COLUMN dynamic_referral_code text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_referral ADD COLUMN role text DEFAULT 'DRIVER';