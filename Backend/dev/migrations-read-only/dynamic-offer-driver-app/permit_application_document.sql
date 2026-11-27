CREATE TABLE atlas_driver_offer_bpp.permit_application_document ();

ALTER TABLE atlas_driver_offer_bpp.permit_application_document ADD COLUMN application_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.permit_application_document ADD COLUMN content_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.permit_application_document ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.permit_application_document ADD COLUMN document_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.permit_application_document ADD COLUMN file_hash text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.permit_application_document ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.permit_application_document ADD COLUMN lifetime boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.permit_application_document ADD COLUMN s3_path text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.permit_application_document ADD COLUMN size_bytes integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.permit_application_document ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.permit_application_document ADD COLUMN valid_until date ;
ALTER TABLE atlas_driver_offer_bpp.permit_application_document ADD PRIMARY KEY ( id);
CREATE INDEX CONCURRENTLY permit_application_document_idx_application_id ON atlas_driver_offer_bpp.permit_application_document USING btree (application_id);
ALTER TABLE atlas_driver_offer_bpp.permit_application_document ADD CONSTRAINT permit_application_document_unique_idx_application_id_document_type UNIQUE (application_id, document_type);