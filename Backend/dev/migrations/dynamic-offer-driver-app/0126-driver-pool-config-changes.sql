ALTER TABLE atlas_driver_offer_bpp.transporter_config
ADD COLUMN popup_delay_to_add_as_penalty int,
ADD COLUMN threshold_cancellation_score int,
ADD COLUMN min_rides_for_cancellation_score int;

UPDATE atlas_driver_offer_bpp.transporter_config
SET popup_delay_to_add_as_penalty = 5, threshold_cancellation_score = 40, min_rides_for_cancellation_score = 5;

ALTER TABLE atlas_driver_offer_bpp.transporter_config DROP COLUMN availability_time_weightage;
ALTER TABLE atlas_driver_offer_bpp.transporter_config DROP COLUMN acceptance_ratio_weightage;
ALTER TABLE atlas_driver_offer_bpp.transporter_config DROP COLUMN cancellation_ratio_weightage;
ALTER TABLE atlas_driver_offer_bpp.transporter_config DROP COLUMN availability_time_window_option;
ALTER TABLE atlas_driver_offer_bpp.transporter_config DROP COLUMN acceptance_ratio_window_option;
ALTER TABLE atlas_driver_offer_bpp.transporter_config DROP COLUMN cancellation_ratio_window_option;

CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.driver_intelligent_pool_config
(
    merchant_id character(36) PRIMARY KEY NOT NULL REFERENCES atlas_driver_offer_bpp.merchant (id),
    availability_time_weightage int NOT NULL,
    acceptance_ratio_weightage int NOT NULL,
    cancellation_ratio_weightage int NOT NULL,
    availability_time_window_option json NOT NULL,
    acceptance_ratio_window_option json NOT NULL,
    cancellation_ratio_window_option json NOT NULL,
    min_quotes_to_qualify_for_intelligent_pool int NOT NULL,
    min_quotes_to_qualify_for_intelligent_pool_window_option json NOT NULL,
    intelligent_pool_percentage int,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config OWNER TO atlas_driver_offer_bpp_user;

INSERT INTO atlas_driver_offer_bpp.driver_intelligent_pool_config(
	merchant_id, availability_time_weightage, acceptance_ratio_weightage, cancellation_ratio_weightage, availability_time_window_option, acceptance_ratio_window_option, cancellation_ratio_window_option, min_quotes_to_qualify_for_intelligent_pool, min_quotes_to_qualify_for_intelligent_pool_window_option, intelligent_pool_percentage, created_at, updated_at)
	VALUES ('favorit0-0000-0000-0000-00000favorit', 70, 40, -40, '{"period":7, "periodType":"Days"}', '{"period":7, "periodType":"Days"}', '{"period":7, "periodType":"Days"}', 5, '{"period":24, "periodType":"Hours"}', 50, now(), now());

CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.driver_pool_config
(
    merchant_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.merchant (id),
    min_radius_of_search int NOT NULL,
    max_radius_of_search int NOT NULL,
    radius_step_size int NOT NULL,
    driver_position_info_expiry int,
    intelligent_pool_percentage int,
    actual_distance_threshold int,
    max_driver_quotes_required int,
    driver_quote_limit int,
    driver_request_count_limit int,
    driver_batch_size int NOT NULL,
    min_driver_batch_size int NOT NULL,
    max_number_of_batches int NOT NULL,
    max_parallel_search_requests int NOT NULL,
    pool_sorting_type character varying(20) NOT NULL,
    single_batch_process_time int,
    trip_distance int NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY (merchant_id, trip_distance)
);
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config OWNER TO atlas_driver_offer_bpp_user;

INSERT INTO atlas_driver_offer_bpp.driver_pool_config(
	merchant_id, min_radius_of_search, max_radius_of_search, radius_step_size, driver_position_info_expiry, intelligent_pool_percentage, actual_distance_threshold, max_driver_quotes_required, driver_quote_limit, driver_request_count_limit, driver_batch_size, min_driver_batch_size, max_number_of_batches, max_parallel_search_requests, pool_sorting_type, single_batch_process_time, trip_distance, created_at, updated_at)
	VALUES ('favorit0-0000-0000-0000-00000favorit', 5000, 7000, 500, 36000, 50, 7000, 1, 2, 3, 5, 3, 3, 3, 'Intelligent', 0, 0, now(), now());

INSERT INTO atlas_driver_offer_bpp.driver_pool_config(
	merchant_id, min_radius_of_search, max_radius_of_search, radius_step_size, driver_position_info_expiry, intelligent_pool_percentage, actual_distance_threshold, max_driver_quotes_required, driver_quote_limit, driver_request_count_limit, driver_batch_size, min_driver_batch_size, max_number_of_batches, max_parallel_search_requests, pool_sorting_type, single_batch_process_time, trip_distance, created_at, updated_at)
	VALUES ('favorit0-0000-0000-0000-00000favorit', 3000, 5000, 500, 36000, 50, 7000, 1, 2, 3, 5, 3, 3, 3, 'Intelligent', 10, 10000, now(), now());

INSERT INTO atlas_driver_offer_bpp.driver_pool_config(
	merchant_id, min_radius_of_search, max_radius_of_search, radius_step_size, driver_position_info_expiry, intelligent_pool_percentage, actual_distance_threshold, max_driver_quotes_required, driver_quote_limit, driver_request_count_limit, driver_batch_size, min_driver_batch_size, max_number_of_batches, max_parallel_search_requests, pool_sorting_type, single_batch_process_time, trip_distance, created_at, updated_at)
	VALUES ('favorit0-0000-0000-0000-00000favorit', 1000, 3000, 500, 36000, 50, 7000, 1, 2, 3, 5, 3, 3, 3, 'Intelligent', 10, 20000, now(), now());
