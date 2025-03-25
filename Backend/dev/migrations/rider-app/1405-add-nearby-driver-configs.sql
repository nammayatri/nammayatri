-- NOTE: Only for local, DON'T RUN IN MASTER / PROD

UPDATE atlas_app.rider_config
    SET variant_list_for_near_by_req = '{"SEDAN", "SUV"}'
    WHERE merchant_operating_city_id = 'namma-yatri-0-0000-0000-00000000city';

UPDATE atlas_app.rider_config
    SET near_by_driver_ring_bucket_cfg = '[{"radiusInMeters": 1000, "vehVariant": "SEDAN", "size": 2}, {"radiusInMeters": 2000, "vehVariant": "SUV", "size": 3}]'
    WHERE merchant_operating_city_id = 'namma-yatri-0-0000-0000-00000000city';
