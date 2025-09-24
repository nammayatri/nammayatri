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
