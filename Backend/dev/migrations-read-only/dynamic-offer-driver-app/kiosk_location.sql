CREATE TABLE atlas_driver_offer_bpp.kiosk_location ();

ALTER TABLE atlas_driver_offer_bpp.kiosk_location ADD COLUMN address text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.kiosk_location ADD COLUMN contact character varying (15) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.kiosk_location ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.kiosk_location ADD COLUMN landmark text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.kiosk_location ADD COLUMN latitude double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.kiosk_location ADD COLUMN longitude double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.kiosk_location ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.kiosk_location ADD PRIMARY KEY ( id);