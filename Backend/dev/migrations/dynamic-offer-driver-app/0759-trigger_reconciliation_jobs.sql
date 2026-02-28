-- Trigger reconciliation jobs for local (NAMMA_YATRI_PARTNER / Kochi, yesterday).
-- scheduler_job.job_data is text; we store JSON as text.

INSERT INTO atlas_driver_offer_bpp.scheduler_job (
    id,
    job_type,
    job_data,
    scheduled_at,
    created_at,
    updated_at,
    max_errors,
    curr_errors,
    status,
    merchant_id,
    merchant_operating_city_id
)
SELECT
    gen_random_uuid(),
    'Reconciliation',
    jsonb_build_object(
        'merchantId', m.id,
        'merchantOperatingCityId', moc.id,
        'startTime', (CURRENT_DATE - INTERVAL '1 day')::text || 'T00:00:00Z',
        'endTime', (CURRENT_DATE - INTERVAL '1 day')::text || 'T23:59:59Z',
        'reconciliationType', 'DSR_VS_LEDGER'
    )::text,
    NOW(),
    NOW(),
    NOW(),
    3,
    0,
    'Pending',
    m.id,
    moc.id
FROM atlas_driver_offer_bpp.merchant m
CROSS JOIN atlas_driver_offer_bpp.merchant_operating_city moc
WHERE m.short_id = 'NAMMA_YATRI_PARTNER'
  AND moc.city = 'Kochi'
  AND moc.merchant_short_id = 'NAMMA_YATRI_PARTNER';

INSERT INTO atlas_driver_offer_bpp.scheduler_job (
    id,
    job_type,
    job_data,
    scheduled_at,
    created_at,
    updated_at,
    max_errors,
    curr_errors,
    status,
    merchant_id,
    merchant_operating_city_id
)
SELECT
    gen_random_uuid(),
    'Reconciliation',
    jsonb_build_object(
        'merchantId', m.id,
        'merchantOperatingCityId', moc.id,
        'startTime', (CURRENT_DATE - INTERVAL '1 day')::text || 'T00:00:00Z',
        'endTime', (CURRENT_DATE - INTERVAL '1 day')::text || 'T23:59:59Z',
        'reconciliationType', 'DSR_VS_SUBSCRIPTION'
    )::text,
    NOW(),
    NOW(),
    NOW(),
    3,
    0,
    'Pending',
    m.id,
    moc.id
FROM atlas_driver_offer_bpp.merchant m
CROSS JOIN atlas_driver_offer_bpp.merchant_operating_city moc
WHERE m.short_id = 'NAMMA_YATRI_PARTNER'
  AND moc.city = 'Kochi'
  AND moc.merchant_short_id = 'NAMMA_YATRI_PARTNER';

INSERT INTO atlas_driver_offer_bpp.scheduler_job (
    id,
    job_type,
    job_data,
    scheduled_at,
    created_at,
    updated_at,
    max_errors,
    curr_errors,
    status,
    merchant_id,
    merchant_operating_city_id
)
SELECT
    gen_random_uuid(),
    'Reconciliation',
    jsonb_build_object(
        'merchantId', m.id,
        'merchantOperatingCityId', moc.id,
        'startTime', (CURRENT_DATE - INTERVAL '1 day')::text || 'T00:00:00Z',
        'endTime', (CURRENT_DATE - INTERVAL '1 day')::text || 'T23:59:59Z',
        'reconciliationType', 'DSSR_VS_SUBSCRIPTION'
    )::text,
    NOW(),
    NOW(),
    NOW(),
    3,
    0,
    'Pending',
    m.id,
    moc.id
FROM atlas_driver_offer_bpp.merchant m
CROSS JOIN atlas_driver_offer_bpp.merchant_operating_city moc
WHERE m.short_id = 'NAMMA_YATRI_PARTNER'
  AND moc.city = 'Kochi'
  AND moc.merchant_short_id = 'NAMMA_YATRI_PARTNER';
