-- ADD COLUMN: ride_duration_fare column.
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details ADD COLUMN ride_duration_fare double precision;

-- NOTE: Only for Local, DON'T RUN IN MASTER OR PROD
--------------------------------------------------------------------------------------------------------------------------------------------------
WITH PerMinRateSections AS (
    SELECT 10.0 AS per_min_rate, 0.0 :: integer AS ride_duration_in_min
    UNION ALL
    SELECT 15.0 AS per_min_rate, 5.0 :: integer AS ride_duration_in_min
    UNION ALL
    SELECT 20.0 AS per_min_rate, 10.0 :: integer AS ride_duration_in_min
),
OneWayOnDemandFP AS (
    -- Adds per_min_rate_sections for `OneWayOnDemandDynamicOffer` trips, accross Vehicle Variants
    SELECT fare_policy_id as fp_id FROM atlas_driver_offer_bpp.fare_product
    WHERE trip_category = 'OneWay_OneWayOnDemandDynamicOffer'
    and merchant_operating_city_id = 'favorit0-0000-0000-0000-00000000city'
)
INSERT INTO atlas_driver_offer_bpp.fare_policy_progressive_details_per_min_rate_section
    (currency, fare_policy_id, per_min_rate, ride_duration_in_min, created_at, updated_at)
SELECT
    'INR',
    fp_id,
    per_min_rate,
    ride_duration_in_min,
    now(),
    now()
FROM OneWayOnDemandFP CROSS JOIN PerMinRateSections
ON CONFLICT DO NOTHING;
------------------------------------------------------------------* END *-------------------------------------------------------------------------
