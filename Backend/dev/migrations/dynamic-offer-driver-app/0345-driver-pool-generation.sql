ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN stop_location_id character varying(36);

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN end_otp text;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN start_odometer_reading_value double precision;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN end_odometer_reading_value double precision;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN start_odometer_reading_file_id character varying(36);
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN end_odometer_reading_file_id character varying(36);

-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ --
-- @ WARNING: DO NOT RUN IN PROD (Just for Local Testing to work)@ --
-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ --
WITH DriverPoolConfig AS (
  select T1.merchant_id, T1.id, 'All', 'All', 5000, 7000, 500, 36000, 7000, 1, 2, 3, 5, 3, 3, 'Intelligent', 10, 0, 300, 300, 10, now(), now()
  FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
)
INSERT INTO atlas_driver_offer_bpp.driver_pool_config (merchant_id, merchant_operating_city_id, trip_category, vehicle_variant, min_radius_of_search, max_radius_of_search, radius_step_size, driver_position_info_expiry, actual_distance_threshold, max_driver_quotes_required, driver_quote_limit, driver_request_count_limit, driver_batch_size, max_number_of_batches, max_parallel_search_requests, pool_sorting_type, single_batch_process_time, trip_distance, radius_shrink_value_for_drivers_on_ride, driver_to_destination_distance_threshold, driver_to_destination_duration, created_at, updated_at)
	(SELECT * FROM DriverPoolConfig);