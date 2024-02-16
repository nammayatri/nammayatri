ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN num_rides_weightage INT DEFAULT 2;
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN max_num_rides INT DEFAULT 336;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN ride_frequency_score DOUBLE PRECISION;
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN cancellation_and_ride_frequency_ratio_window_option json NOT NULL DEFAULT '{"period":7, "periodType":"Days"}';
UPDATE atlas_driver_offer_bpp.driver_intelligent_pool_config SET cancellation_and_ride_frequency_ratio_window_option = cancellation_ratio_window_option;
-- NOTE:-
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config DROP COLUMN cancellation_ratio_window_option; -- Run After release is stable.