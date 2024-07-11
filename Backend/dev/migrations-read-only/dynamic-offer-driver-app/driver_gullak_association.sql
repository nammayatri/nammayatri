CREATE TABLE atlas_driver_offer_bpp.driver_gullak_association ();

ALTER TABLE atlas_driver_offer_bpp.driver_gullak_association ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_gullak_association ADD COLUMN gullak_token text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_gullak_association ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_gullak_association ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_gullak_association ADD COLUMN token_expiry timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_gullak_association ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_gullak_association ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_gullak_association ADD PRIMARY KEY ( driver_id);