ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN currency character varying(255);

ALTER TABLE atlas_driver_offer_bpp.cancellation_charges ADD COLUMN currency character varying(255);

ALTER TABLE atlas_driver_offer_bpp.coin_purchase_history ADD COLUMN currency character varying(255);

ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN currency character varying(255);
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN total_earnings_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN govt_charges_amount double precision;

ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN currency character varying(255);
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN estimated_fare_amount double precision;

ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN currency character varying(255);
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN total_earnings_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN bonus_earned_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN earnings_missed_amount double precision;

ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN currency character varying(255);
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN base_fare_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN driver_selected_fare_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN customer_extra_fee_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN waiting_charge_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN ride_extra_time_fare_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN night_shift_charge_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN service_charge_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN govt_charges_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN congestion_charge_amount double precision;

ALTER TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details ADD COLUMN currency character varying(255);
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details ADD COLUMN dead_km_fare_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details ADD COLUMN extra_km_fare_amount double precision;

ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN currency character varying(255);
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN time_based_fare_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN dist_based_fare_amount double precision;

ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN currency character varying(255);
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN service_charge_amount double precision;

ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ADD COLUMN min_fee_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ADD COLUMN max_fee_amount double precision;

ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN currency character varying(255);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN base_fare_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN dead_km_fare_amount double precision;

ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN currency character varying(255);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN base_fare_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN per_hour_charge_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN per_extra_min_rate_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN per_extra_km_rate_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN planned_per_km_rate_amount double precision;

ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN currency character varying(255);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN base_fare_amount double precision;

ALTER TABLE atlas_driver_offer_bpp.mandate ADD COLUMN currency character varying(255);

ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ADD COLUMN currency character varying(255);
UPDATE atlas_driver_offer_bpp.merchant_operating_city SET currency = 'INR';
ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ALTER COLUMN currency SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN currency character varying(255);

ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN currency character varying(255);
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN estimated_fare_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN driver_min_fee_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN driver_max_fee_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN driver_pick_up_charge_amount double precision;

ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN currency character varying(255);

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN currency character varying(255);
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN driver_default_extra_fee_amount double precision;

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN currency character varying(255);
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN driver_min_extra_fee_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN driver_max_extra_fee_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN base_fare_amount double precision;

ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN currency character varying(255);
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN base_fare_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN customer_extra_fee_amount double precision;
