CREATE TABLE atlas_driver_offer_bpp.vehicle_details ();

ALTER TABLE atlas_driver_offer_bpp.vehicle_details ADD COLUMN ac_available boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_details ADD COLUMN capacity integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_details ADD COLUMN color text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_details ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_details ADD COLUMN make text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_details ADD COLUMN model text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_details ADD PRIMARY KEY ( id);


------- SQL updates -------
ALTER TABLE atlas_driver_offer_bpp.vehicle_details DROP COLUMN capacity;
ALTER TABLE atlas_driver_offer_bpp.vehicle_details DROP COLUMN color;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_details ADD COLUMN variant text ;