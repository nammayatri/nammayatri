CREATE TABLE atlas_driver_offer_bpp.media_file_document ();

ALTER TABLE atlas_driver_offer_bpp.media_file_document ADD COLUMN creator_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.media_file_document ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.media_file_document ADD COLUMN media_file_document_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.media_file_document ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.media_file_document ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.media_file_document ADD COLUMN rc_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.media_file_document ADD COLUMN s3_path text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.media_file_document ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.media_file_document ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.media_file_document ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.media_file_document ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.media_file_document ADD COLUMN file_hash text ;
ALTER TABLE atlas_driver_offer_bpp.media_file_document ADD COLUMN upload_link text ;