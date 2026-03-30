CREATE TABLE atlas_driver_offer_bpp.tds_distribution_pdf_file ();

ALTER TABLE atlas_driver_offer_bpp.tds_distribution_pdf_file ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_pdf_file ADD COLUMN file_name character varying(512) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_pdf_file ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_pdf_file ADD COLUMN s3_file_path character varying(512) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_pdf_file ADD COLUMN tds_distribution_record_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_pdf_file ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_pdf_file ADD PRIMARY KEY ( id);
