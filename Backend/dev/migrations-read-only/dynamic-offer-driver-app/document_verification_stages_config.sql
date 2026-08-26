CREATE TABLE atlas_driver_offer_bpp.document_verification_stages_config ();

ALTER TABLE atlas_driver_offer_bpp.document_verification_stages_config ADD COLUMN applicable_to text NOT NULL default 'FLEET_AND_INDIVIDUAL';
ALTER TABLE atlas_driver_offer_bpp.document_verification_stages_config ADD COLUMN description text ;
ALTER TABLE atlas_driver_offer_bpp.document_verification_stages_config ADD COLUMN document_category text NOT NULL default 'Driver';
ALTER TABLE atlas_driver_offer_bpp.document_verification_stages_config ADD COLUMN document_onboarding_stage text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_verification_stages_config ADD COLUMN hint text ;
ALTER TABLE atlas_driver_offer_bpp.document_verification_stages_config ADD COLUMN is_hidden boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.document_verification_stages_config ADD COLUMN media_json json ;
ALTER TABLE atlas_driver_offer_bpp.document_verification_stages_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_verification_stages_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_verification_stages_config ADD COLUMN "order" integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.document_verification_stages_config ADD COLUMN stage_dependency text[] NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_verification_stages_config ADD COLUMN title text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_verification_stages_config ADD COLUMN vehicle_category text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_verification_stages_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.document_verification_stages_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.document_verification_stages_config ADD PRIMARY KEY ( applicable_to, document_category, document_onboarding_stage, merchant_operating_city_id, vehicle_category);
