CREATE TABLE atlas_driver_offer_bpp.merchant_document ();

ALTER TABLE atlas_driver_offer_bpp.merchant_document ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.merchant_document ADD COLUMN document_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_document ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_document ADD COLUMN language text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_document ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_document ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.merchant_document ADD COLUMN role text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_document ADD COLUMN title text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_document ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.merchant_document ADD COLUMN url text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_document ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.merchant_document ADD COLUMN platform_type text ;