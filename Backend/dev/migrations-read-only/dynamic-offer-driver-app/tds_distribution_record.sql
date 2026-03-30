CREATE TABLE atlas_driver_offer_bpp.tds_distribution_record ();

ALTER TABLE atlas_driver_offer_bpp.tds_distribution_record ADD COLUMN assessment_year character varying(20) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_record ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_record ADD COLUMN driver_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_record ADD COLUMN email_address character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_record ADD COLUMN file_name character varying(512) ;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_record ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_record ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_record ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_record ADD COLUMN quarter character varying(10) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_record ADD COLUMN retry_count integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_record ADD COLUMN s3_file_path character varying(512) ;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_record ADD COLUMN status character varying(30) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_record ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_record ADD PRIMARY KEY ( id);
