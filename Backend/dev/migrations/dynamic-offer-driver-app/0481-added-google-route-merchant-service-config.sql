UPDATE atlas_driver_offer_bpp.merchant_service_config
SET config_json = jsonb_set(
    config_json::jsonb,
    '{googleRouteConfig}',
    jsonb_build_object(
        'computeAlternativeRoutes', false,
        'routePreference', 'TRAFFIC_AWARE_OPTIMAL',
        'url', 'https://routes.googleapis.com/'
    )
)::json
WHERE service_name = 'Maps_Google';

UPDATE atlas_driver_offer_bpp.merchant_service_config
SET config_json = jsonb_set(
    config_json::jsonb,
    '{useAdvancedDirections}',
    json('true')::jsonb
)::json
WHERE service_name = 'Maps_Google';