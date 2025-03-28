CREATE TABLE atlas_driver_offer_bpp.vehicle_info ();

ALTER TABLE atlas_driver_offer_bpp.vehicle_info ADD COLUMN answer text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_info ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_info ADD COLUMN question text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_info ADD COLUMN question_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_info ADD COLUMN rc_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_info ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_info DROP CONSTRAINT vehicle_info_pkey;
ALTER TABLE atlas_driver_offer_bpp.vehicle_info ADD PRIMARY KEY ( );

--- Now DSL don't allow dropping tables instead we will drop not null constraint if any .Please be careful while running ---
ALTER TABLE atlas_driver_offer_bpp.vehicle_info ALTER COLUMN id DROP NOT NULL;
--- Drop section ends. Please check before running ---



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_info ALTER COLUMN id SET DEFAULT '';
ALTER TABLE atlas_driver_offer_bpp.vehicle_info ALTER COLUMN id SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_info DROP CONSTRAINT vehicle_info_pkey;
ALTER TABLE atlas_driver_offer_bpp.vehicle_info ADD PRIMARY KEY ( id);