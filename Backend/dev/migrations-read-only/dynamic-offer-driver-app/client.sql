CREATE TABLE atlas_driver_offer_bpp.client ();

ALTER TABLE atlas_driver_offer_bpp.client ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.client ADD COLUMN short_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.client ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.client ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.client ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.client ADD PRIMARY KEY ( id);