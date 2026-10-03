-- No need to run this migration, these are already there !! --
CREATE TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details ();

ALTER TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details ADD COLUMN dead_km_fare integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details ADD COLUMN dead_km_fare_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details ADD COLUMN extra_km_fare integer ;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details ADD COLUMN extra_km_fare_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details ADD COLUMN fare_parameters_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details ADD COLUMN ride_duration_fare double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details ADD PRIMARY KEY ( fare_parameters_id);

-- No need to run migrations till here, these are already there !! --