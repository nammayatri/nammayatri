-- No need to run this migration, these are already there !! --
CREATE TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ();

ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD COLUMN cgst double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD COLUMN currency character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD COLUMN dist_based_fare double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD COLUMN fare_parameters_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD COLUMN platform_fee double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD COLUMN sgst double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD PRIMARY KEY ( fare_parameters_id);

-- No need to run migrations till here, these are already there !! --