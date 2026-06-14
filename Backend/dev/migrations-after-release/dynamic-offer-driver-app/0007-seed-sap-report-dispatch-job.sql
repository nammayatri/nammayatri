-- Seed initial SAP Report Dispatch scheduler job (one per merchant operating city)
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
    md5(random()::text || clock_timestamp()::text)::uuid,
    'SAPReportDispatch',
    json_build_object(
        'merchantId', moc.merchant_id,
        'merchantOperatingCityId', moc.id,
        'scheduledTime', '06:00:00',
        'timeDiffFromUtc', 19800,
        'maxApiRetries', 3
    )::text,
    0,
    (CURRENT_DATE + 1) + interval '30 minutes',
    NOW(),
    NOW(),
    5,
    0,
    'Pending',
    'PARENT_SAP_DISPATCH',
    moc.merchant_id,
    moc.id
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.merchant_short_id = 'MSIL_PARTNER';
