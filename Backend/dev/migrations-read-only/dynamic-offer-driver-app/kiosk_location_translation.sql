CREATE TABLE atlas_driver_offer_bpp.kiosk_location_translation ();

ALTER TABLE atlas_driver_offer_bpp.kiosk_location_translation ADD COLUMN address text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.kiosk_location_translation ADD COLUMN kiosk_location_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.kiosk_location_translation ADD COLUMN landmark character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.kiosk_location_translation ADD COLUMN language text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.kiosk_location_translation ADD PRIMARY KEY ( kiosk_location_id);