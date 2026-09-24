-- No need to run this migration, these are already there !! --
CREATE TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ();

ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN dead_km_fare double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN dist_based_fare integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN dist_based_fare_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN distance_unit character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN extra_distance integer ;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN extra_duration integer ;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN fare_parameters_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN time_based_fare integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN time_based_fare_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD PRIMARY KEY ( fare_parameters_id);

-- No need to run migrations till here, these are already there !! --