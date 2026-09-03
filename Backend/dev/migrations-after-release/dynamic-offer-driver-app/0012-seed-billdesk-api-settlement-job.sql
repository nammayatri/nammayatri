-- Seed per-service settlement ingestion job: BillDesk API
-- One job per (merchant, city, service) with service-specific schedule
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
    'SettlementReportIngestion',
    json_build_object(
        'merchantId', t.merchant_id,
        'merchantOperatingCityId', t.moc_id,
        'juspayServiceName', null,
        'settlementProvider', 'BillDesk',
        'startTime', to_char((CURRENT_DATE - 1) AT TIME ZONE 'Asia/Kolkata' AT TIME ZONE 'UTC', 'YYYY-MM-DD"T"HH24:MI:SS"Z"'),
        'endTime', to_char((CURRENT_DATE - interval '1 second') AT TIME ZONE 'Asia/Kolkata' AT TIME ZONE 'UTC', 'YYYY-MM-DD"T"HH24:MI:SS"Z"'),
        'scheduleNextJob', true
    )::text,
    0,
    (CURRENT_DATE + 1) + interval '2 hours' - interval '5 hours 30 minutes',
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
    WHERE moc.merchant_short_id = 'MSIL_PARTNER'
) t;
