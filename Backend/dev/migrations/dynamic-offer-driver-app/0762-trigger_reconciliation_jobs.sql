-- Trigger reconciliation jobs for local (NAMMA_YATRI_PARTNER / Kochi, yesterday).
-- scheduler_job.job_data is text; we store JSON as text.
-- shard_id and parent_job_id are REQUIRED - producer blocks without them.
-- parent_job_id = id for first/root jobs.
-- Time format in job_data: ISO 8601 (YYYY-MM-DDTHH:MM:SSZ).


--  DO NOT RUN IN MASTER / PROD [SKIP THIS MIGRATION IN MASTER / PROD]
INSERT INTO atlas_driver_offer_bpp.scheduler_job (
    id,
    job_type,
    job_data,
    scheduled_at,
    maximum_delay,
    created_at,
    updated_at,
    max_errors,
    curr_errors,
    status,
    shard_id,
    parent_job_id,
    merchant_operating_city_id,
    merchant_id
)
SELECT
    sub.id,
    'Reconciliation',
    jsonb_build_object(
        'merchantId', sub.merchant_id,
        'merchantOperatingCityId', sub.moc_id,
        'startTime', to_char(CURRENT_DATE - INTERVAL '1 day', 'YYYY-MM-DD') || 'T00:00:00Z',
        'endTime', to_char(CURRENT_DATE - INTERVAL '1 day', 'YYYY-MM-DD') || 'T23:59:59Z',
        'reconciliationType', 'DSR_VS_LEDGER'
    )::text,
    NOW(),
    NULL,
    NOW(),
    NOW(),
    5,
    0,
    'Pending',
    1,
    sub.id,
    sub.moc_id,
    sub.merchant_id
FROM (
    SELECT gen_random_uuid() AS id, m.id AS merchant_id, moc.id AS moc_id
    FROM atlas_driver_offer_bpp.merchant m
    CROSS JOIN atlas_driver_offer_bpp.merchant_operating_city moc
    WHERE m.short_id = 'NAMMA_YATRI_PARTNER'
      AND moc.city = 'Kochi'
      AND moc.merchant_short_id = 'NAMMA_YATRI_PARTNER'
) sub;

--  DO NOT RUN IN MASTER / PROD [SKIP THIS MIGRATION IN MASTER / PROD]

INSERT INTO atlas_driver_offer_bpp.scheduler_job (
    id,
    job_type,
    job_data,
    scheduled_at,
    maximum_delay,
    created_at,
    updated_at,
    max_errors,
    curr_errors,
    status,
    shard_id,
    parent_job_id,
    merchant_operating_city_id,
    merchant_id
)
SELECT
    sub.id,
    'Reconciliation',
    jsonb_build_object(
        'merchantId', sub.merchant_id,
        'merchantOperatingCityId', sub.moc_id,
        'startTime', to_char(CURRENT_DATE - INTERVAL '1 day', 'YYYY-MM-DD') || 'T00:00:00Z',
        'endTime', to_char(CURRENT_DATE - INTERVAL '1 day', 'YYYY-MM-DD') || 'T23:59:59Z',
        'reconciliationType', 'DSR_VS_SUBSCRIPTION'
    )::text,
    NOW(),
    NULL,
    NOW(),
    NOW(),
    5,
    0,
    'Pending',
    1,
    sub.id,
    sub.moc_id,
    sub.merchant_id
FROM (
    SELECT gen_random_uuid() AS id, m.id AS merchant_id, moc.id AS moc_id
    FROM atlas_driver_offer_bpp.merchant m
    CROSS JOIN atlas_driver_offer_bpp.merchant_operating_city moc
    WHERE m.short_id = 'NAMMA_YATRI_PARTNER'
      AND moc.city = 'Kochi'
      AND moc.merchant_short_id = 'NAMMA_YATRI_PARTNER'
) sub;

--  DO NOT RUN IN MASTER / PROD [SKIP THIS MIGRATION IN MASTER / PROD]

INSERT INTO atlas_driver_offer_bpp.scheduler_job (
    id,
    job_type,
    job_data,
    scheduled_at,
    maximum_delay,
    created_at,
    updated_at,
    max_errors,
    curr_errors,
    status,
    shard_id,
    parent_job_id,
    merchant_operating_city_id,
    merchant_id
)
SELECT
    sub.id,
    'Reconciliation',
    jsonb_build_object(
        'merchantId', sub.merchant_id,
        'merchantOperatingCityId', sub.moc_id,
        'startTime', to_char(CURRENT_DATE - INTERVAL '1 day', 'YYYY-MM-DD') || 'T00:00:00Z',
        'endTime', to_char(CURRENT_DATE - INTERVAL '1 day', 'YYYY-MM-DD') || 'T23:59:59Z',
        'reconciliationType', 'DSSR_VS_SUBSCRIPTION'
    )::text,
    NOW(),
    NULL,
    NOW(),
    NOW(),
    5,
    0,
    'Pending',
    1,
    sub.id,
    sub.moc_id,
    sub.merchant_id
FROM (
    SELECT gen_random_uuid() AS id, m.id AS merchant_id, moc.id AS moc_id
    FROM atlas_driver_offer_bpp.merchant m
    CROSS JOIN atlas_driver_offer_bpp.merchant_operating_city moc
    WHERE m.short_id = 'NAMMA_YATRI_PARTNER'
      AND moc.city = 'Kochi'
      AND moc.merchant_short_id = 'NAMMA_YATRI_PARTNER'
) sub;
