CREATE TABLE atlas_driver_offer_bpp.app_attest_key ();

ALTER TABLE atlas_driver_offer_bpp.app_attest_key ADD COLUMN bundle_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.app_attest_key ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.app_attest_key ADD COLUMN environment text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.app_attest_key ADD COLUMN key_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.app_attest_key ADD COLUMN last_used_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.app_attest_key ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.app_attest_key ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.app_attest_key ADD COLUMN public_key text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.app_attest_key ADD COLUMN receipt text ;
ALTER TABLE atlas_driver_offer_bpp.app_attest_key ADD COLUMN sign_count bigint NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.app_attest_key ADD COLUMN status text NOT NULL default 'ACTIVE';
ALTER TABLE atlas_driver_offer_bpp.app_attest_key ADD COLUMN team_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.app_attest_key ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.app_attest_key ADD COLUMN use_count bigint NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.app_attest_key ADD PRIMARY KEY ( key_id);
