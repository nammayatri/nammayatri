-- No need to run this migration, these are already there !! --
CREATE TABLE atlas_driver_offer_bpp.fare_policy_rental_details ();

ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN base_fare integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN base_fare_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN dead_km_fare double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN fare_policy_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN included_km_per_hr integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN max_additional_kms_limit integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN night_shift_charge text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN per_extra_km_rate integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN per_extra_km_rate_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN per_extra_min_rate integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN per_extra_min_rate_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN per_hour_charge integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN per_hour_charge_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN planned_per_km_rate integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN planned_per_km_rate_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN total_additional_kms_limit integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN free_waiting_time text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN waiting_charge text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD PRIMARY KEY ( fare_policy_id);

-- No need to run migrations till here, these are already there !! --