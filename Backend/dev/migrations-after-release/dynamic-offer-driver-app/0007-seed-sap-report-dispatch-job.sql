-- Seed initial SAP Subscription Purchase Dispatch scheduler job (one per merchant operating city)
INSERT INTO atlas_driver_offer_bpp.scheduler_job (
    id,
    job_type,
    job_data,
    shard_id,
    scheduled_at,
    created_at,
    updated_at,
    max_errors,
    curr_errors,
    status,
    parent_job_id,
    merchant_id,
    merchant_operating_city_id
)
SELECT
    t.job_id,
    'SAPSubscriptionPurchaseDispatch',
    json_build_object(
        'merchantId', t.merchant_id,
        'merchantOperatingCityId', t.moc_id,
        'scheduledTime', '06:00:00',
        'timeDiffFromUtc', 19800,
        'maxApiRetries', 3,
        'startTime', to_char(date_trunc('day', NOW() + interval '5 hours 30 minutes') - interval '5 hours 30 minutes', 'YYYY-MM-DD"T"HH24:MI:SS"Z"'),
        'endTime', to_char(date_trunc('day', NOW() + interval '5 hours 30 minutes') - interval '5 hours 30 minutes' + interval '23 hours 59 minutes 59 seconds', 'YYYY-MM-DD"T"HH24:MI:SS"Z"'),
        'scheduleNextJob', true
    )::text,
    0,
    (CURRENT_DATE + 1) + interval '30 minutes',
    NOW(),
    NOW(),
    5,
    0,
    'Pending',
    t.job_id,
    t.merchant_id,
    t.moc_id
FROM (
    SELECT
        moc.id AS moc_id,
        moc.merchant_id,
        md5(random()::text || clock_timestamp()::text)::uuid AS job_id
    FROM atlas_driver_offer_bpp.merchant_operating_city moc
    WHERE moc.merchant_short_id = 'MSIL_PARTNER' AND moc.city = 'Hyderabad'
) t;

-- Seed initial SAP PG Settlement Dispatch scheduler job (one per merchant operating city)
INSERT INTO atlas_driver_offer_bpp.scheduler_job (
    id,
    job_type,
    job_data,
    shard_id,
    scheduled_at,
    created_at,
    updated_at,
    max_errors,
    curr_errors,
    status,
    parent_job_id,
    merchant_id,
    merchant_operating_city_id
)
SELECT
    t.job_id,
    'SAPPGSettlementDispatch',
    json_build_object(
        'merchantId', t.merchant_id,
        'merchantOperatingCityId', t.moc_id,
        'scheduledTime', '06:00:00',
        'timeDiffFromUtc', 19800,
        'maxApiRetries', 3,
        'startTime', to_char(date_trunc('day', NOW() + interval '5 hours 30 minutes') - interval '5 hours 30 minutes', 'YYYY-MM-DD"T"HH24:MI:SS"Z"'),
        'endTime', to_char(date_trunc('day', NOW() + interval '5 hours 30 minutes') - interval '5 hours 30 minutes' + interval '23 hours 59 minutes 59 seconds', 'YYYY-MM-DD"T"HH24:MI:SS"Z"'),
        'scheduleNextJob', true
    )::text,
    0,
    (CURRENT_DATE + 1) + interval '30 minutes',
    NOW(),
    NOW(),
    5,
    0,
    'Pending',
    t.job_id,
    t.merchant_id,
    t.moc_id
FROM (
    SELECT
        moc.id AS moc_id,
        moc.merchant_id,
        md5(random()::text || clock_timestamp()::text)::uuid AS job_id
    FROM atlas_driver_offer_bpp.merchant_operating_city moc
    WHERE moc.merchant_short_id = 'MSIL_PARTNER' AND moc.city = 'Hyderabad'
) t;
