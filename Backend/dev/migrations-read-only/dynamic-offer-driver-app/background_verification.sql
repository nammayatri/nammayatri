CREATE TABLE atlas_driver_offer_bpp.background_verification ();

ALTER TABLE atlas_driver_offer_bpp.background_verification ADD COLUMN candidate_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.background_verification ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.background_verification ADD COLUMN expires_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.background_verification ADD COLUMN invitation_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.background_verification ADD COLUMN invitation_status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.background_verification ADD COLUMN invitation_url text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.background_verification ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.background_verification ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.background_verification ADD COLUMN report_id text ;
ALTER TABLE atlas_driver_offer_bpp.background_verification ADD COLUMN report_status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.background_verification ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.background_verification ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.background_verification ADD PRIMARY KEY ( driver_id);