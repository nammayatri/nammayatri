CREATE TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details();

ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN fare_parameters_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN time_fare numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN distance_fare numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN pickup_charge numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN currency character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN extra_distance_fare numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD COLUMN extra_time_fare numeric(30, 2) NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details ADD PRIMARY KEY (fare_parameters_id);

-- fare_policy_inter_city_details
CREATE TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details();

ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN fare_policy_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN base_fare numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN per_hour_charge numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN per_km_rate_one_way numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN per_km_rate_round_trip numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN per_extra_km_rate numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN per_extra_min_rate numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN km_per_planned_extra_hour int NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN dead_km_fare numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN per_day_max_hour_allowance int NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN default_wait_time_at_destination int NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN currency character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN night_shift_charge json;

ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN toll_charges numeric(30, 2);