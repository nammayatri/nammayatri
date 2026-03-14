CREATE TABLE atlas_driver_offer_bpp.knowledge_center ();

ALTER TABLE atlas_driver_offer_bpp.knowledge_center ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.knowledge_center ADD COLUMN file_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.knowledge_center ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.knowledge_center ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.knowledge_center ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.knowledge_center ADD COLUMN s3_path text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.knowledge_center ADD COLUMN sop_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.knowledge_center ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.knowledge_center ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.knowledge_center ADD COLUMN document_name text ;