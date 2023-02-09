INSERT INTO atlas_app.merchant_service_config (merchant_id, service_name, config_json)
SELECT m.id, 'Maps_OSRM',
  json_build_object(
    'osrmUrl', 'localhost:5000',
    'radiusDeviation', 20
  )
FROM atlas_app.merchant m;
