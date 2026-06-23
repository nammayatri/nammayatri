CREATE TABLE atlas_driver_offer_bpp.driver_identity_info ();

ALTER TABLE atlas_driver_offer_bpp.driver_identity_info ADD COLUMN address text ;
ALTER TABLE atlas_driver_offer_bpp.driver_identity_info ADD COLUMN address_document_type text ;
ALTER TABLE atlas_driver_offer_bpp.driver_identity_info ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_identity_info ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_identity_info ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_identity_info ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_identity_info ADD COLUMN nominee_dob date ;
ALTER TABLE atlas_driver_offer_bpp.driver_identity_info ADD COLUMN nominee_name text ;
ALTER TABLE atlas_driver_offer_bpp.driver_identity_info ADD COLUMN nominee_relationship text ;
ALTER TABLE atlas_driver_offer_bpp.driver_identity_info ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_identity_info ADD PRIMARY KEY ( driver_id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_identity_info ADD COLUMN address_state text ;


------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_identity_info ADD COLUMN court_record json ;