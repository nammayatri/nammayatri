CREATE TABLE atlas_driver_offer_bpp.vehicle_info ();

ALTER TABLE atlas_driver_offer_bpp.vehicle_info ADD COLUMN answer text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_info ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_info ADD COLUMN question text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_info ADD COLUMN question_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_info ADD COLUMN rc_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_info ADD PRIMARY KEY ( id);
