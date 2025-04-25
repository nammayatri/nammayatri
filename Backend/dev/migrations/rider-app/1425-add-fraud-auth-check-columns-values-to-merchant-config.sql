INSERT INTO atlas_app.merchant_config
(
    id,
    merchant_id,
    merchant_operating_city_id,
    fraud_booking_cancellation_count_threshold,
    fraud_booking_total_count_threshold,
    fraud_booking_cancellation_count_window,
    fraud_booking_cancelled_by_driver_count_threshold,
    fraud_booking_cancelled_by_driver_count_window,
    fraud_search_count_threshold,
    fraud_search_count_window,
    enabled,
    fraud_ride_count_threshold,
    fraud_ride_count_window,
    fraud_auth_count_threshold,
    fraud_auth_count_window
)
SELECT
md5(random()::text || clock_timestamp()::text)::uuid,
    merchant_id,
    id as operating_city_id,
    1000,
    1000,
    '{"period":1, "periodType":"Minutes"}',
    1000,
    '{"period":1, "periodType":"Minutes"}',
    1000,
    '{"period":1, "periodType":"Minutes"}',
    true,
    1000,
    '{"period":1, "periodType":"Minutes"}',
    10,
    '{"period":20, "periodType":"Minutes"}'
FROM atlas_app.merchant_operating_city;

UPDATE atlas_app.rider_config
SET blocked_until_in_mins = 1440
WHERE blocked_until_in_mins IS NULL;
