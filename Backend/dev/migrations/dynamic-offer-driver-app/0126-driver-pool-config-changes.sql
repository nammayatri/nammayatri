UPDATE atlas_driver_offer_bpp.transporter_config
SET popup_delay_to_add_as_penalty = 5, threshold_cancellation_score = 40, min_rides_for_cancellation_score = 5;

-- ONLY FOR LOCAL
With MerchantInfo AS (
    SELECT
     T1.merchant_id,
     T1.id, 70, 40, -40, '{"period":7, "periodType":"Days"}' :: json, '{"period":7, "periodType":"Days"}' :: json, '{"period":7, "periodType":"Days"}' :: json, 5, '{"period":24, "periodType":"Hours"}' :: json, 50, now(), now()
    FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
)
INSERT INTO atlas_driver_offer_bpp.driver_intelligent_pool_config(
	merchant_id, merchant_operating_city_id, availability_time_weightage, acceptance_ratio_weightage, cancellation_ratio_weightage, availability_time_window_option, acceptance_ratio_window_option, cancellation_and_ride_frequency_ratio_window_option, min_quotes_to_qualify_for_intelligent_pool, min_quotes_to_qualify_for_intelligent_pool_window_option, intelligent_pool_percentage, created_at, updated_at)
	(SELECT * FROM MerchantInfo);