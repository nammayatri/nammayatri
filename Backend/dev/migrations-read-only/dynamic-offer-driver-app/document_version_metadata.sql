CREATE TABLE atlas_driver_offer_bpp.document_version_metadata ();

ALTER TABLE atlas_driver_offer_bpp.document_version_metadata ADD COLUMN document_number_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_version_metadata ADD COLUMN document_number_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_version_metadata ADD COLUMN document_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_version_metadata ADD COLUMN document_version_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_version_metadata ADD COLUMN metadata json ;
ALTER TABLE atlas_driver_offer_bpp.document_version_metadata ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_version_metadata ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.document_version_metadata ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.document_version_metadata ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.document_version_metadata ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.document_version_metadata ADD PRIMARY KEY ( document_version_id);



------- SQL updates -------

CREATE INDEX CONCURRENTLY document_version_metadata_idx_person_id ON atlas_driver_offer_bpp.document_version_metadata USING btree (person_id);