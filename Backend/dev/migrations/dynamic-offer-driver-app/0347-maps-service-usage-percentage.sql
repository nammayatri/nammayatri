ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN search_request_id character(36) REFERENCES atlas_driver_offer_bpp.search_request;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN search_request_id character(36) REFERENCES atlas_driver_offer_bpp.search_request;

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN get_distances_percentage json;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN get_estimated_pickup_distances_percentage json;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN get_routes_percentage json;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN get_pickup_routes_percentage json;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN get_trip_routes_percentage json;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN snap_to_road_percentage json;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN get_place_name_percentage json;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN get_place_details_percentage json;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN auto_complete_percentage json;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN get_distances_for_cancel_ride_percentage json;

UPDATE atlas_driver_offer_bpp.merchant_service_usage_config SET
    get_distances_percentage =
        json_build_object(
            'usePercentage', false
            , 'googlePercentage', (CASE WHEN get_distances = 'Google' THEN 100 ELSE null END)
            , 'osrmPercentage', (CASE WHEN get_distances = 'OSRM' THEN 100 ELSE null END)
            , 'mmiPercentage', (CASE WHEN get_distances = 'MMI' THEN 100 ELSE null END)
            ),
    get_estimated_pickup_distances_percentage =
        json_build_object(
            'usePercentage', false
            , 'googlePercentage', (CASE WHEN get_estimated_pickup_distances = 'Google' THEN 100 ELSE null END)
            , 'osrmPercentage', (CASE WHEN get_estimated_pickup_distances = 'OSRM' THEN 100 ELSE null END)
            , 'mmiPercentage', (CASE WHEN get_estimated_pickup_distances = 'MMI' THEN 100 ELSE null END)
            ),
    get_routes_percentage =
        json_build_object(
            'usePercentage', false
            , 'googlePercentage', (CASE WHEN get_routes = 'Google' THEN 100 ELSE null END)
            , 'osrmPercentage', (CASE WHEN get_routes = 'OSRM' THEN 100 ELSE null END)
            , 'mmiPercentage', (CASE WHEN get_routes = 'MMI' THEN 100 ELSE null END)
            ),
    get_pickup_routes_percentage =
        json_build_object(
            'usePercentage', false
            , 'googlePercentage', (CASE WHEN get_pickup_routes = 'Google' THEN 100 ELSE null END)
            , 'osrmPercentage', (CASE WHEN get_pickup_routes = 'OSRM' THEN 100 ELSE null END)
            , 'mmiPercentage', (CASE WHEN get_pickup_routes = 'MMI' THEN 100 ELSE null END)
            ),
    get_trip_routes_percentage =
        json_build_object(
            'usePercentage', false
            , 'googlePercentage', (CASE WHEN get_trip_routes = 'Google' THEN 100 ELSE null END)
            , 'osrmPercentage', (CASE WHEN get_trip_routes = 'OSRM' THEN 100 ELSE null END)
            , 'mmiPercentage', (CASE WHEN get_trip_routes = 'MMI' THEN 100 ELSE null END)
            ),
    snap_to_road_percentage =
        json_build_object(
            'usePercentage', false
            , 'googlePercentage', (CASE WHEN snap_to_road = 'Google' THEN 100 ELSE null END)
            , 'osrmPercentage', (CASE WHEN snap_to_road = 'OSRM' THEN 100 ELSE null END)
            , 'mmiPercentage', (CASE WHEN snap_to_road = 'MMI' THEN 100 ELSE null END)
            ),
    get_place_name_percentage =
        json_build_object(
            'usePercentage', false
            , 'googlePercentage', (CASE WHEN get_place_name = 'Google' THEN 100 ELSE null END)
            , 'osrmPercentage', (CASE WHEN get_place_name = 'OSRM' THEN 100 ELSE null END)
            , 'mmiPercentage', (CASE WHEN get_place_name = 'MMI' THEN 100 ELSE null END)
            ),
    get_place_details_percentage =
        json_build_object(
            'usePercentage', false
            , 'googlePercentage', (CASE WHEN get_place_details = 'Google' THEN 100 ELSE null END)
            , 'osrmPercentage', (CASE WHEN get_place_details = 'OSRM' THEN 100 ELSE null END)
            , 'mmiPercentage', (CASE WHEN get_place_details = 'MMI' THEN 100 ELSE null END)
            ),
    auto_complete_percentage =
        json_build_object(
            'usePercentage', false
            , 'googlePercentage', (CASE WHEN auto_complete = 'Google' THEN 100 ELSE null END)
            , 'osrmPercentage', (CASE WHEN auto_complete = 'OSRM' THEN 100 ELSE null END)
            , 'mmiPercentage', (CASE WHEN auto_complete = 'MMI' THEN 100 ELSE null END)
            ),
    get_distances_for_cancel_ride_percentage =
        json_build_object(
            'usePercentage', false
            , 'googlePercentage', (CASE WHEN get_distances_for_cancel_ride = 'Google' THEN 100 ELSE null END)
            , 'osrmPercentage', (CASE WHEN get_distances_for_cancel_ride = 'OSRM' THEN 100 ELSE null END)
            , 'mmiPercentage', (CASE WHEN get_distances_for_cancel_ride = 'MMI' THEN 100 ELSE null END)
            );

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ALTER COLUMN get_distances_percentage SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ALTER COLUMN get_estimated_pickup_distances_percentage SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ALTER COLUMN get_routes_percentage SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ALTER COLUMN get_pickup_routes_percentage SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ALTER COLUMN get_trip_routes_percentage SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ALTER COLUMN snap_to_road_percentage SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ALTER COLUMN get_place_name_percentage SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ALTER COLUMN get_place_details_percentage SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ALTER COLUMN auto_complete_percentage SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ALTER COLUMN get_distances_for_cancel_ride_percentage SET NOT NULL;
