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
