CREATE TABLE atlas_driver_offer_bpp.iffco_tokio_insurance ();

ALTER TABLE atlas_driver_offer_bpp.iffco_tokio_insurance ADD COLUMN certificate_number text ;
ALTER TABLE atlas_driver_offer_bpp.iffco_tokio_insurance ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.iffco_tokio_insurance ADD COLUMN declaration_id text ;
ALTER TABLE atlas_driver_offer_bpp.iffco_tokio_insurance ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.iffco_tokio_insurance ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.iffco_tokio_insurance ADD COLUMN iffco_status text ;
ALTER TABLE atlas_driver_offer_bpp.iffco_tokio_insurance ADD COLUMN insurance_status character varying(255) NOT NULL default 'PENDING';
ALTER TABLE atlas_driver_offer_bpp.iffco_tokio_insurance ADD COLUMN invoice_request_number text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.iffco_tokio_insurance ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.iffco_tokio_insurance ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.iffco_tokio_insurance ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.iffco_tokio_insurance ADD PRIMARY KEY ( id);
