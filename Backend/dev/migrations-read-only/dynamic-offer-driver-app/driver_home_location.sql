CREATE TABLE atlas_driver_offer_bpp.driver_home_location ();

ALTER TABLE atlas_driver_offer_bpp.driver_home_location ADD COLUMN address text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_home_location ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_home_location ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_home_location ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_home_location ADD COLUMN lat double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_home_location ADD COLUMN lon double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_home_location ADD COLUMN tag text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_home_location ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_home_location ADD PRIMARY KEY ( id);