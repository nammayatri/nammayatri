-- LOCAL SYNC
INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
SELECT m.merchant_id, m.id, 'Maps_OSRM',
  json_build_object(
    'osrmUrl', 'localhost:5001',
    'radiusDeviation', 20
  )
FROM atlas_driver_offer_bpp.merchant_operating_city m;
