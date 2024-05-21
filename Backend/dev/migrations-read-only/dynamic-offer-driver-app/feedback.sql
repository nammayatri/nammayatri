CREATE TABLE atlas_driver_offer_bpp.feedback ();

ALTER TABLE atlas_driver_offer_bpp.feedback ADD COLUMN badge character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.feedback ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.feedback ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.feedback ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.feedback ADD COLUMN ride_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.feedback ADD PRIMARY KEY ( id);