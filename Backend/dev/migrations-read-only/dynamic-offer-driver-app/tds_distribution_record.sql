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



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.tds_distribution_record ADD COLUMN financial_year character varying(10) ;


------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.tds_distribution_record ADD COLUMN latest_email_delivery_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_record ADD COLUMN last_attempt_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_record ADD COLUMN failure_reason character varying(30) ;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_record ADD COLUMN delivered_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_record ADD COLUMN batch_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_record ADD COLUMN attempt_count integer ;


------- SQL updates -------




------- SQL updates -------

