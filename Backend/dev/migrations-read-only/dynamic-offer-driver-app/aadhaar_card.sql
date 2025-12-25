CREATE TABLE atlas_driver_offer_bpp.aadhaar_card ();

ALTER TABLE atlas_driver_offer_bpp.aadhaar_card ADD COLUMN aadhaar_back_image_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_card ADD COLUMN aadhaar_front_image_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_card ADD COLUMN address text ;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_card ADD COLUMN consent boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_card ADD COLUMN consent_timestamp timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_card ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_card ADD COLUMN date_of_birth text ;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_card ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_card ADD COLUMN masked_aadhaar_number text ;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_card ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_card ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_card ADD COLUMN name_on_card text ;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_card ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_card ADD COLUMN verification_status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_card ADD PRIMARY KEY ( driver_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.aadhaar_card ADD COLUMN driver_image_path text ;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_card ADD COLUMN driver_image text ;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_card ADD COLUMN driver_gender text ;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_card ADD COLUMN aadhaar_number_hash text ;