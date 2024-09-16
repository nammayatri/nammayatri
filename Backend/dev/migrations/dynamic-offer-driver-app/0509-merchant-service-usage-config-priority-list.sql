
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config SET get_trip_routes_priority_list = ARRAY[get_trip_routes]::text[];
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config SET get_routes_priority_list = ARRAY[get_routes]::text[];
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config SET get_place_name_priority_list = ARRAY[get_place_name]::text[];
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config SET get_place_details_priority_list = ARRAY[get_place_details]::text[];
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config SET get_pickup_routes_priority_list = ARRAY[get_pickup_routes]::text[];
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config SET get_estimated_pickup_distances_priority_list = ARRAY[get_estimated_pickup_distances]::text[];
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config SET get_distances_priority_list = ARRAY[get_distances]::text[];
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config SET get_distances_for_scheduled_rides_priority_list = ARRAY[get_distances_for_scheduled_rides]::text[];
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config SET get_distances_for_cancel_ride_priority_list = ARRAY[get_distances_for_cancel_ride]::text[];
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config SET auto_complete_priority_list = ARRAY[auto_complete]::text[];
