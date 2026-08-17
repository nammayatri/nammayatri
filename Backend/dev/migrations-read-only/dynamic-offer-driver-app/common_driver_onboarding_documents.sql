CREATE TABLE atlas_driver_offer_bpp.common_driver_onboarding_documents ();

ALTER TABLE atlas_driver_offer_bpp.common_driver_onboarding_documents ADD COLUMN document_data text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.common_driver_onboarding_documents ADD COLUMN document_image_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.common_driver_onboarding_documents ADD COLUMN document_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.common_driver_onboarding_documents ADD COLUMN driver_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.common_driver_onboarding_documents ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.common_driver_onboarding_documents ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.common_driver_onboarding_documents ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.common_driver_onboarding_documents ADD COLUMN reject_reason text ;
ALTER TABLE atlas_driver_offer_bpp.common_driver_onboarding_documents ADD COLUMN verification_status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.common_driver_onboarding_documents ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.common_driver_onboarding_documents ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.common_driver_onboarding_documents ADD PRIMARY KEY ( id);

------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.common_driver_onboarding_documents ALTER COLUMN document_data TYPE text;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.common_driver_onboarding_documents ADD COLUMN rc_id character varying(36) ;


------- SQL updates -------

CREATE INDEX CONCURRENTLY common_driver_onboarding_documents_idx_document_image_id ON atlas_driver_offer_bpp.common_driver_onboarding_documents USING btree (document_image_id);
CREATE INDEX CONCURRENTLY common_driver_onboarding_documents_idx_driver_id ON atlas_driver_offer_bpp.common_driver_onboarding_documents USING btree (driver_id);
CREATE INDEX CONCURRENTLY common_driver_onboarding_documents_idx_rc_id ON atlas_driver_offer_bpp.common_driver_onboarding_documents USING btree (rc_id);