alter table atlas_driver_offer_bpp.fare_policy add column cancellation_fare_policy_id text;

--Only for Local
INSERT INTO atlas_driver_offer_bpp.cancellation_fare_policy(
    id,
    currency,
    free_cancellation_time_seconds,
    max_cancellation_charge,
    max_waiting_time_at_pickup_seconds,
    min_cancellation_charge,
    per_metre_cancellation_charge,
    per_minute_cancellation_charge,
    percentage_of_ride_fare_to_be_charged,
    description,
    created_at,
    updated_at)
SELECT
    md5(random()::text || clock_timestamp()::text || city.id::text)::uuid,
    'INR',
    300,
    100.00,
    300,
    10.00,
    0.5,
    0.5,
    0.1,
    'Default cancellation fare policy',
    now(),
    now()
FROM
    atlas_driver_offer_bpp.merchant_operating_city AS city;