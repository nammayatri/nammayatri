ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config
ADD COLUMN calculate_multi_zonal character varying(100) NOT NULL DEFAULT 'OSRM_SHARDED';


INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
SELECT m.id, moc.id, 'Maps_OSRM_SHARDED',
  json_build_object(
    'cityToRegionUrlMap',
        json_build_object(
            'kerala', 'localhost:5001',
            'karnataka', 'localhost:5001'
        ),
    'radiusDeviation', 20
  )
FROM atlas_driver_offer_bpp.merchant as m, atlas_driver_offer_bpp.merchant_operating_city as moc
WHERE moc.merchant_id = m.id;

