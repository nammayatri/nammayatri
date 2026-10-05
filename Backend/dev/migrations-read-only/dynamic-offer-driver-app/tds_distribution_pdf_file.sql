CREATE TABLE atlas_driver_offer_bpp.tds_distribution_pdf_file ();

ALTER TABLE atlas_driver_offer_bpp.tds_distribution_pdf_file ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_pdf_file ADD COLUMN file_name character varying(512) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_pdf_file ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_pdf_file ADD COLUMN s3_file_path character varying(512) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_pdf_file ADD COLUMN tds_distribution_record_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_pdf_file ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_pdf_file ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.tds_distribution_pdf_file ADD COLUMN validation_status character varying(30) ;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_pdf_file ADD COLUMN size_bytes integer ;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_pdf_file ADD COLUMN recipient_type character varying(30) ;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_pdf_file ADD COLUMN matched_person_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_pdf_file ADD COLUMN issue character varying(30) ;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_pdf_file ADD COLUMN batch_id character varying(36) ;