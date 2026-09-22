CREATE TABLE atlas_driver_offer_bpp.bap_metadata ();

ALTER TABLE atlas_driver_offer_bpp.bap_metadata ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.bap_metadata ADD COLUMN logo_url text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.bap_metadata ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.bap_metadata ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.bap_metadata ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.bap_metadata ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.bap_metadata ALTER COLUMN logo_url DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.bap_metadata ADD COLUMN domain text ;


------- SQL updates -------

------------ DONT RUN THIS IN MASTER OR PROD --------------

ALTER TABLE atlas_driver_offer_bpp.bap_metadata ALTER COLUMN id TYPE character varying(255);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.bap_metadata ADD COLUMN static_terms_url text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.bap_metadata ADD COLUMN support_email text ;
ALTER TABLE atlas_driver_offer_bpp.bap_metadata ADD COLUMN support_phone text ;
ALTER TABLE atlas_driver_offer_bpp.bap_metadata ADD COLUMN support_url text ;
ALTER TABLE atlas_driver_offer_bpp.bap_metadata ADD COLUMN offline_contract boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.bap_metadata ALTER COLUMN domain SET DEFAULT 'MOBILITY';
ALTER TABLE atlas_driver_offer_bpp.bap_metadata ALTER COLUMN domain SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.bap_metadata ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.bap_metadata ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.bap_metadata ADD COLUMN enable_ondc_scheduled_ride_support boolean ;
ALTER TABLE atlas_driver_offer_bpp.bap_metadata DROP CONSTRAINT bap_metadata_pkey;
ALTER TABLE atlas_driver_offer_bpp.bap_metadata ADD PRIMARY KEY ( domain, id, merchant_id, merchant_operating_city_id);