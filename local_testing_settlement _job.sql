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
VALUES (
    gen_random_uuid()::text,
    'SettlementReportIngestion',
    jsonb_build_object(
        'merchantId', 'bharat-t-bpp0-0000-0000-000000000000',
        'merchantOperatingCityId', 'bt-partn-city-delh-0000-000000000000'
    )::text,
    NOW(),
    NULL,
    NOW(),
    NOW(),
    5,
    0,
    'Pending',
    1,
    (SELECT gen_random_uuid()::text),  -- set parent_job_id = id: use a CTE or same variable
    'bt-partn-city-delh-0000-000000000000',
    'bharat-t-bpp0-0000-0000-000000000000'
);

INSERT INTO atlas_driver_offer_bpp.merchant_service_config (
  merchant_id,
  merchant_operating_city_id,
  service_name,
  config_json,
  created_at,
  updated_at
) VALUES (
  'bharat-t-bpp0-0000-0000-000000000000',
atlas_dev=# UPDATE atlas_driver_offer_bpp.merchant_service_config
SET
  merchant_operating_city_id = 'bt-partn-city-delh-0000-000000000000',
  config_json = $$
{
  "tag": "EmailSourceConfig",
  "contents": {
    "imapHost": "imap.gmail.com",
    "imapPort": 993,
    "username": "imap8182@gmail.com",
    "password": "0.1.0|2|aYfvxX5tUEpuhH/zc/jDo2r3PzdD3d0qgYid0DSrXDOahnZ4dHcVdP/NIJ/BHxloq5OcLWypiiIuDgUNBrB09fE=",
    "folderName": "INBOX",
    "subjectFilter": "Settlement Report"
  }
}
)