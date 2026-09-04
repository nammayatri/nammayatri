-- Key names follow existing "… A/C" convention used by SAPReportDispatch mkItem lookups.

-- NOTE: Did not found G/L for RIDE_FARE_REVENUE A/C, used the same as for BUYER_APP_POOL A/C: 1441420
UPDATE atlas_driver_offer_bpp.merchant_service_config AS msc
SET
    config_json = jsonb_set(
        msc.config_json::jsonb,
        '{accountMapping}',
        COALESCE(msc.config_json::jsonb -> 'accountMapping', '{}'::jsonb) || '{
          "BANK A/C": {
            "hkont": "2432212",
            "kostl": null,
            "prctr": "2000201"
          },
          "PG_CLEARING A/C": {
            "hkont": "2461431",
            "kostl": null,
            "prctr": "2000201"
          },
          "DEFERRED_REVENUE A/C": {
            "hkont": "2181201",
            "kostl": null,
            "prctr": "2000201"
          },
          "CGST_PAYABLE A/C": {
            "hkont": "1472329",
            "kostl": null,
            "prctr": "2000201"
          },
          "SGST_PAYABLE A/C": {
            "hkont": "1472330",
            "kostl": null,
            "prctr": "2000201"
          },
          "IGST_PAYABLE A/C": {
            "hkont": "1472331",
            "kostl": null,
            "prctr": "2000201"
          },
          "BUYER_APP_RECEIVABLE A/C": {
            "hkont": "2461414",
            "kostl": null,
            "prctr": "2000201"
          },
          "RIDE_FARE_REVENUE A/C": {
            "hkont": "1441420",
            "kostl": null,
            "prctr": "2000201"
          },
          "DRIVER_BALANCE A/C": {
            "hkont": "1479125",
            "kostl": null,
            "prctr": "2000201"
          },
          "PAYOUT_CLEARING A/C": {
            "hkont": "1479119",
            "kostl": null,
            "prctr": "2000201"
          },
          "TDS_PAYABLE A/C": {
            "hkont": "1471209",
            "kostl": null,
            "prctr": "2000201"
          },
          "TDS_RECEIVABLE A/C": {
            "hkont": "1461146",
            "kostl": null,
            "prctr": "2000201"
          },
          "SUBSCRIPTION_REVENUE A/C": {
            "hkont": "3121101",
            "kostl": null,
            "prctr": "2000201"
          }
        }'::jsonb
    )::json,
    updated_at = NOW()
FROM atlas_driver_offer_bpp.merchant_operating_city AS moc
WHERE msc.merchant_operating_city_id = moc.id
  AND msc.merchant_id = moc.merchant_id
  AND msc.service_name = 'SAP_Journal'
  AND moc.merchant_short_id = 'MSIL_PARTNER'
  AND moc.city = 'Hyderabad';

-- Seed initial SAP Ride Revenue Dispatch scheduler job (one per merchant operating city)
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
    'SAPRideRevenueDispatch',
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
