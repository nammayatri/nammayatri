-- No need to run this migration, these are already there !! --
CREATE TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ();

ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN currency character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN distance_fare double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN extra_distance_fare double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN extra_time_fare double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN fare_parameters_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN pickup_charge double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN state_entry_permit_charges double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN time_fare double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD PRIMARY KEY ( fare_parameters_id);

-- No need to run migrations till here, these are already there !! --