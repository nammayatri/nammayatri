CREATE TABLE atlas_driver_offer_bpp.tds_distribution_batch ();

ALTER TABLE atlas_driver_offer_bpp.tds_distribution_batch ADD COLUMN completed_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_batch ADD COLUMN confirmed_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_batch ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_batch ADD COLUMN financial_year character varying(10) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_batch ADD COLUMN folder_name character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_batch ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_batch ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_batch ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_batch ADD COLUMN quarter character varying(10) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_batch ADD COLUMN status character varying(30) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_batch ADD COLUMN total_files integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_batch ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_batch ADD COLUMN uploaded_by_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_batch ADD COLUMN uploaded_by_name character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_batch ADD COLUMN validated_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_batch ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.tds_distribution_batch ADD COLUMN confirmed_by_name character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.tds_distribution_batch ADD COLUMN confirmed_by_id character varying(36) ;