ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN scheduling_charge JSON;

ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN scheduling_charge double precision;
