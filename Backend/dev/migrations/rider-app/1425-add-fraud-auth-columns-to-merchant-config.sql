WITH MerchantIds AS (
    SELECT id as merchant_id FROM atlas_app.merchant
),
OperatingCityIds AS (
    SELECT id as operating_city_id FROM atlas_app.merchant_operating_city
)
INSERT INTO atlas_app.merchant_config
(
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
    m.merchant_id,
    o.operating_city_id,
    5,
    100,
    '{"period":30, "periodType":"Minutes"}',
    5,
    '{"period":30, "periodType":"Minutes"}',
    10,
    '{"period":30, "periodType":"Minutes"}',
    true,
    5,
    '{"period":30, "periodType":"Minutes"}',
    10,
    '{"period":20, "periodType":"Minutes"}'
FROM MerchantIds m, OperatingCityIds o
WHERE NOT EXISTS (
    SELECT 1 FROM atlas_app.merchant_config mc
    WHERE mc.merchant_id = m.merchant_id
    AND mc.merchant_operating_city_id = o.operating_city_id
);