CREATE TABLE atlas_driver_offer_bpp.aadhaar_verification ();

ALTER TABLE atlas_driver_offer_bpp.aadhaar_verification ADD COLUMN aadhaar_number_hash text ;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_verification ADD COLUMN driver_dob text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_verification ADD COLUMN driver_gender text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_verification ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_verification ADD COLUMN driver_image text ;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_verification ADD COLUMN driver_image_path text ;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_verification ADD COLUMN driver_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_verification ADD COLUMN is_verified boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_verification ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_verification ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_verification ADD PRIMARY KEY ( driver_id);