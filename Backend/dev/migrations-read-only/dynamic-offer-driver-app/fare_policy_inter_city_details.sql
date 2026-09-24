-- No need to run this migration, these are already there !! --
CREATE TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ();

ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN base_fare double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN currency character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN dead_km_fare double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN default_wait_time_at_destination text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN fare_policy_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN km_per_planned_extra_hour integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN night_shift_charge text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN per_day_max_allowance_in_mins text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN per_day_max_hour_allowance text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN per_extra_km_rate double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN per_extra_min_rate double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN per_hour_charge double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN per_km_rate_one_way double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN per_km_rate_round_trip double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN state_entry_permit_charges double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN free_wating_time text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN waiting_charge text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD PRIMARY KEY ( fare_policy_id);

-- No need to run migrations till here, these are already there !! --