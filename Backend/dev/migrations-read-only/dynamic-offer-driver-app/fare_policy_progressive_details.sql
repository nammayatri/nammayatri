-- No need to run this migration, these are already there !! --
CREATE TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ();

ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN base_distance integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN base_fare integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN base_fare_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN dead_km_fare integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN dead_km_fare_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN distance_unit character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN fare_policy_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN night_shift_charge text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN per_min_rate_duration_basis text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN pickup_charges_max integer ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN pickup_charges_max_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN pickup_charges_min integer ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN pickup_charges_min_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN free_wating_time text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN waiting_charge text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD PRIMARY KEY ( fare_policy_id);

-- No need to run migrations till here, these are already there !! --