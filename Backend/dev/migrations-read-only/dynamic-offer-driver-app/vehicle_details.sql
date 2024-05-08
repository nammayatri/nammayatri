CREATE TABLE atlas_driver_offer_bpp.vehicle_details ();

ALTER TABLE atlas_driver_offer_bpp.vehicle_details ADD COLUMN ac_available boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_details ADD COLUMN capacity integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_details ADD COLUMN color text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_details ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_details ADD COLUMN make text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_details ADD COLUMN model text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_details ADD PRIMARY KEY ( id);




------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_details ALTER COLUMN capacity DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_details ALTER COLUMN ac_available DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_details ADD COLUMN vehicle_variant text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_details DROP COLUMN color;