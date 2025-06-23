INSERT INTO atlas_app.merchant_service_config (merchant_id, service_name, config_json, merchant_operating_city_id)
SELECT m.merchant_id, 'MultiModalStaticData_OTPTransit',
 '{"baseUrl": "http://gtfs-inmemory-data-server.nandi.svc.cluster.local:8000"}'
    , m.id
FROM atlas_app.merchant_operating_city m;