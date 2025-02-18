-- ONLY FOR LOCAL --
UPDATE atlas_driver_offer_bpp.fare_product
SET trip_category = 'Delivery_OneWayOnDemandDynamicOffer'
WHERE vehicle_variant like 'DELIVERY_TRUCK_%' AND trip_category = 'OneWay_OneWayOnDemandDynamicOffer';

-- ONLY FOR LOCAL --
update atlas_driver_offer_bpp.vehicle_service_tier
set allowed_vehicle_variant = '{DELIVERY_TRUCK_MINI,DELIVERY_TRUCK_SMALL,DELIVERY_TRUCK_MEDIUM,DELIVERY_TRUCK_LARGE,DELIVERY_TRUCK_ULTRA_LARGE}'
where service_tier_type = 'DELIVERY_TRUCK_MINI';

-- ONLY FOR LOCAL --
update atlas_driver_offer_bpp.vehicle_service_tier
set allowed_vehicle_variant = '{DELIVERY_TRUCK_SMALL,DELIVERY_TRUCK_MEDIUM,DELIVERY_TRUCK_LARGE,DELIVERY_TRUCK_ULTRA_LARGE}'
where service_tier_type = 'DELIVERY_TRUCK_SMALL';

-- ONLY FOR LOCAL --
update atlas_driver_offer_bpp.vehicle_service_tier
set allowed_vehicle_variant = '{DELIVERY_TRUCK_MEDIUM,DELIVERY_TRUCK_LARGE,DELIVERY_TRUCK_ULTRA_LARGE}'
where service_tier_type = 'DELIVERY_TRUCK_MEDIUM';

-- ONLY FOR LOCAL --
update atlas_driver_offer_bpp.vehicle_service_tier
set allowed_vehicle_variant = '{DELIVERY_TRUCK_LARGE,DELIVERY_TRUCK_ULTRA_LARGE}'
where service_tier_type = 'DELIVERY_TRUCK_LARGE';

-- ONLY FOR LOCAL --
update atlas_driver_offer_bpp.vehicle_service_tier
set allowed_vehicle_variant = '{DELIVERY_TRUCK_ULTRA_LARGE}'
where service_tier_type = 'DELIVERY_TRUCK_ULTRA_LARGE';


-- MASTER ONLY --
INSERT INTO atlas_driver_offer_bpp.beckn_config(
	buyer_finder_fee, collected_by, domain, gateway_url, id, payment_params_json, registry_url, settlement_type, settlement_window, static_terms_url, subscriber_id, subscriber_url, unique_key_id, vehicle_category, merchant_id, merchant_operating_city_id, created_at, updated_at, on_select_ttl_sec, on_search_ttl_sec, on_init_ttl_sec, on_confirm_ttl_sec, on_track_ttl_sec, on_status_ttl_sec, on_cancel_ttl_sec, on_update_ttl_sec)
	VALUES (null, 'BPP', 'MOBILITY', 'http://localhost:8015/v1', '194dac49-5a17-45fc-a742-5004c2ac3687', '{"bankAccNumber": "xyz@upi","bankCode": "xyz"}', 'http://localhost:8020', null, null, null, 'NAMMA_YATRI', 'http://localhost:8013/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 100, 'TRUCK', 'favorit0-0000-0000-0000-00000favorit', null, now(), now(), 120, 120, 120, 120, 120, 120, 120, 120);
