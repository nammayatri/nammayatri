ALTER TABLE atlas_app.merchant_service_usage_config
ADD COLUMN calculate_multi_zonal character varying(100) NOT NULL DEFAULT 'OSRM_SHARDED';

INSERT INTO atlas_app.merchant_service_config (merchant_id, service_name, config_json)
SELECT m.id, 'Maps_OSRM_SHARDED',
  json_build_object(
    'cityToRegionUrlMap',
        json_build_object(
            'kerala', 'localhost:5001',
            'karnataka', 'localhost:5001'
        ),
    'radiusDeviation', 20
  )
FROM atlas_app.merchant m;
