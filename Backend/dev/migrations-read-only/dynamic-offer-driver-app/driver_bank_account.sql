CREATE TABLE atlas_driver_offer_bpp.driver_bank_account ();

ALTER TABLE atlas_driver_offer_bpp.driver_bank_account ADD COLUMN account_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_bank_account ADD COLUMN charges_enabled boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_bank_account ADD COLUMN current_account_link text ;
ALTER TABLE atlas_driver_offer_bpp.driver_bank_account ADD COLUMN current_account_link_expiry timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_bank_account ADD COLUMN details_submitted boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_bank_account ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_bank_account ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_bank_account ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_bank_account ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_bank_account ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_bank_account ADD PRIMARY KEY ( driver_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_bank_account ADD COLUMN payment_mode text ;