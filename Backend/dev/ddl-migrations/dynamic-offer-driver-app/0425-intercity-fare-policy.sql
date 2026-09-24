-- NOTE: dont need to run these queries
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.fare_parameters_inter_city_details();

ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN IF NOT EXISTS fare_parameters_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN IF NOT EXISTS time_fare numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN IF NOT EXISTS distance_fare numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN IF NOT EXISTS pickup_charge numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN IF NOT EXISTS currency character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN IF NOT EXISTS extra_distance_fare numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN IF NOT EXISTS extra_time_fare numeric(30, 2) NOT NULL;
DO $$ BEGIN

ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD PRIMARY KEY (fare_parameters_id);
EXCEPTION WHEN OTHERS THEN NULL;
END $$;

-- fare_policy_inter_city_details
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.fare_policy_inter_city_details();

ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN IF NOT EXISTS fare_policy_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN IF NOT EXISTS base_fare numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN IF NOT EXISTS per_hour_charge numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN IF NOT EXISTS per_km_rate_one_way numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN IF NOT EXISTS per_km_rate_round_trip numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN IF NOT EXISTS per_extra_km_rate numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN IF NOT EXISTS per_extra_min_rate numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN IF NOT EXISTS km_per_planned_extra_hour int NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN IF NOT EXISTS dead_km_fare numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN IF NOT EXISTS per_day_max_hour_allowance int NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN IF NOT EXISTS default_wait_time_at_destination int NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN IF NOT EXISTS currency character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN IF NOT EXISTS night_shift_charge json;

ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN IF NOT EXISTS toll_charges numeric(30, 2);
-- NOTE: dont need to run these queries
