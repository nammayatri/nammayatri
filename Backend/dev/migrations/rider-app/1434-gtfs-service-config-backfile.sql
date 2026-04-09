INSERT INTO atlas_app.merchant_service_config (merchant_id, service_name, config_json, merchant_operating_city_id)
SELECT m.merchant_id, 'MultiModalStaticData_OTPTransit',
 '{"baseUrl": {"baseUrlScheme": "Http", "baseUrlHost": "gtfs-inmemory-data-server.nandi.svc.cluster.local", "baseUrlPort": 8000, "baseUrlPath": ""}}'
    , m.id
FROM atlas_app.merchant_operating_city m;