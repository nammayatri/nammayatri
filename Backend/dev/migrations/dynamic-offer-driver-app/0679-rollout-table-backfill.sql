INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_rollout (
    domain,
    experiment_status,
    is_base_version,
    merchant_id,
    merchant_operating_city_id,
    modified_by,
    percentage_rollout,
    time_bounds,
    version,
    created_at,
    updated_at
)
SELECT
    'DRIVER-CONFIG_DriverPoolConfig',
    NULL,
    NULL,
    NULL,
    merchant_operating_city_id,
    NULL,
    percentage_rollout,
    time_bounds,
    version,
    NOW(),
    NOW()
FROM atlas_driver_offer_bpp.app_dynamic_logic_rollout
WHERE domain = 'CONFIG_DriverPoolConfig';
