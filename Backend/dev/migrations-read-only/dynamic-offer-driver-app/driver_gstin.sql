CREATE TABLE atlas_driver_offer_bpp.driver_gstin ();

ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD COLUMN address text ;
ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD COLUMN constitution_of_business text ;
ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD COLUMN date_of_liability timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD COLUMN document_image_id1 character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD COLUMN document_image_id2 character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD COLUMN driver_name text ;
ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD COLUMN gstin_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD COLUMN gstin_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD COLUMN is_provisional boolean ;
ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD COLUMN legal_name text ;
ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD COLUMN trade_name text ;
ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD COLUMN type_of_registration text ;
ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD COLUMN valid_from timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD COLUMN valid_upto timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD COLUMN verification_status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD COLUMN verified_by text ;
ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD COLUMN pan_number text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_gstin ADD COLUMN is_strictly_verified boolean ;