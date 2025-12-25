CREATE TABLE atlas_driver_offer_bpp.cancellation_charges ();

ALTER TABLE atlas_driver_offer_bpp.cancellation_charges ADD COLUMN cancellation_charges integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_charges ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_charges ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_charges ADD COLUMN ride_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_charges ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.cancellation_charges ADD COLUMN currency character varying(255) ;