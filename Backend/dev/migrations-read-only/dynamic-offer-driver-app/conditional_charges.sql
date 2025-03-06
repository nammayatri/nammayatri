CREATE TABLE atlas_driver_offer_bpp.conditional_charges ();

ALTER TABLE atlas_driver_offer_bpp.conditional_charges ADD COLUMN cgst_percentage double precision ;
ALTER TABLE atlas_driver_offer_bpp.conditional_charges ADD COLUMN charge double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.conditional_charges ADD COLUMN charge_category text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.conditional_charges ADD COLUMN fare_policy_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.conditional_charges ADD COLUMN sgst_percentage double precision ;
ALTER TABLE atlas_driver_offer_bpp.conditional_charges ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.conditional_charges ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.conditional_charges ADD PRIMARY KEY ( charge_category, fare_policy_id);
