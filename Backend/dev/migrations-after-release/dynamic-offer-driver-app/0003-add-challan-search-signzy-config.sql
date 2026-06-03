INSERT INTO atlas_driver_offer_bpp.merchant_service_config (created_at, merchant_id, merchant_operating_city_id, config_json, service_name, updated_at)
SELECT
    now(),
    m.merchant_id,
    m.id,
    json_build_object(
        'url', 'https://api-preproduction.signzy.app',
        'apiKey', '<ENCRYPTED_API_KEY>'
    )::json,
    'ChallanSearch_Signzy',
    now()
FROM atlas_driver_offer_bpp.merchant_operating_city AS m
WHERE m.merchant_short_id = 'MSIL_PARTNER'
ON CONFLICT DO NOTHING;
