CREATE TABLE atlas_driver_offer_bpp.fleet_referral ();

ALTER TABLE atlas_driver_offer_bpp.fleet_referral ADD COLUMN fleet_owner_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_referral ADD COLUMN linked_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_referral ADD COLUMN referral_code character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_referral ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_referral ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_referral ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_referral ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_referral ADD PRIMARY KEY ( referral_code);
