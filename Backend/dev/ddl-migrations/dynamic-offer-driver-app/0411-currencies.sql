-- NOTE: dont need to run these queries
--ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN IF NOT EXISTS currency character varying(255);

ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN IF NOT EXISTS currency character varying(255);
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN IF NOT EXISTS base_fare_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN IF NOT EXISTS driver_selected_fare_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN IF NOT EXISTS customer_extra_fee_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN IF NOT EXISTS waiting_charge_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN IF NOT EXISTS ride_extra_time_fare_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN IF NOT EXISTS night_shift_charge_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN IF NOT EXISTS service_charge_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN IF NOT EXISTS govt_charges_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN IF NOT EXISTS congestion_charge_amount double precision;

ALTER TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details ADD COLUMN IF NOT EXISTS currency character varying(255);
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details ADD COLUMN IF NOT EXISTS dead_km_fare_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details ADD COLUMN IF NOT EXISTS extra_km_fare_amount double precision;

ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN IF NOT EXISTS currency character varying(255);
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN IF NOT EXISTS time_based_fare_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN IF NOT EXISTS dist_based_fare_amount double precision;

ALTER TABLE atlas_driver_offer_bpp.fare_parameters_slab_details ADD COLUMN IF NOT EXISTS currency character varying(255);

ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN IF NOT EXISTS currency character varying(255);
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN IF NOT EXISTS service_charge_amount double precision;

ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ADD COLUMN IF NOT EXISTS min_fee_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ADD COLUMN IF NOT EXISTS max_fee_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ADD COLUMN IF NOT EXISTS step_fee_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ADD COLUMN IF NOT EXISTS default_step_fee_amount double precision;

ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN IF NOT EXISTS currency character varying(255);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN IF NOT EXISTS base_fare_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN IF NOT EXISTS dead_km_fare_amount double precision;

ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN IF NOT EXISTS currency character varying(255);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN IF NOT EXISTS base_fare_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN IF NOT EXISTS per_hour_charge_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN IF NOT EXISTS per_extra_min_rate_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN IF NOT EXISTS per_extra_km_rate_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN IF NOT EXISTS planned_per_km_rate_amount double precision;

ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN IF NOT EXISTS currency character varying(255);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN IF NOT EXISTS base_fare_amount double precision;


ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN currency character varying(255);
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN estimated_fare_amount double precision;-- NOTE: dont need to run these queries
-- NOTE: dont need to run these queries
