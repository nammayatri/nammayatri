-- To be run as soon as possible after Prod release
UPDATE atlas_app.merchant_service_config
SET config_json = jsonb_set(
    config_json::jsonb,
    '{googleRouteConfig}',
    jsonb_build_object(
        'computeAlternativeRoutes', true
    )
)::json
WHERE service_name = 'Maps_Google';