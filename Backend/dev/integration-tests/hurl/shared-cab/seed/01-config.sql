-- LOCAL e2e seed: Shillong shared-cab feed hosted on the (imported) Chennai NAMMA_YATRI city.
-- rider side
UPDATE atlas_app.merchant_service_config SET config_json = jsonb_set(config_json::jsonb,'{baseUrl}','"http://localhost:8089/otp/gtfs/v1/"')::json
 WHERE service_name='MultiModal_OTPTransit' AND merchant_operating_city_id='c7e3c3eb-cc15-46d4-ba04-5af55ac87874';
UPDATE atlas_app.merchant_service_config SET config_json = jsonb_set(config_json::jsonb,'{baseUrl}','"http://localhost:8090"')::json
 WHERE service_name='MultiModalStaticData_OTPTransit' AND merchant_operating_city_id='c7e3c3eb-cc15-46d4-ba04-5af55ac87874';

INSERT INTO atlas_app.integrated_bpp_config (id, agency_key, domain, feed_key, merchant_id, merchant_operating_city_id, platform_type, config_json, vehicle_category, created_at, updated_at)
SELECT md5('e2e-shared-cab-ibc')::uuid::text, 'shillong_shared_cab', 'FRFS', 'shillong_shared_cab', '4b17bd06-ae7e-48e9-85bf-282fb310209c', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', 'MULTIMODAL', config_json, 'BUS', now(), now()
FROM atlas_app.integrated_bpp_config WHERE id='b76b3f68-1581-4fe2-9c56-b8968212c95d'
ON CONFLICT DO NOTHING;

-- driver side
INSERT INTO atlas_driver_offer_bpp.integrated_bpp_config (id, agency_key, domain, feed_key, merchant_id, merchant_operating_city_id, platform_type, config_json, vehicle_category, city, created_at, updated_at)
SELECT md5('e2e-shared-cab-ibc')::uuid::text, 'shillong_shared_cab', 'FRFS', 'shillong_shared_cab', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'f8e9db0a-96c8-49e4-942a-3e3f7265d2da', 'APPLICATION', config_json, 'SHARED_CAB', 'Chennai', now(), now()
FROM atlas_driver_offer_bpp.integrated_bpp_config WHERE agency_key='chennai_bus' LIMIT 1
ON CONFLICT DO NOTHING;
UPDATE atlas_app.integrated_bpp_config SET agency_key='shillong_shared_cab:SHARED_CAB' WHERE id=md5('e2e-shared-cab-ibc')::uuid::text; UPDATE atlas_driver_offer_bpp.integrated_bpp_config SET agency_key='shillong_shared_cab:SHARED_CAB' WHERE id=md5('e2e-shared-cab-ibc')::uuid::text; -- R62: agency_key must match across DBs

-- /v2/sharedCab/routes/{code} looks for a BUS + APPLICATION config for the shared-cab agency: a second rider row.
INSERT INTO atlas_app.integrated_bpp_config (id, agency_key, domain, feed_key, merchant_id, merchant_operating_city_id, platform_type, config_json, vehicle_category, created_at, updated_at)
SELECT md5('e2e-shared-cab-ibc-app')::uuid::text, agency_key, domain, feed_key, merchant_id, merchant_operating_city_id, 'APPLICATION', config_json, vehicle_category, now(), now()
FROM atlas_app.integrated_bpp_config WHERE id = md5('e2e-shared-cab-ibc')::uuid::text
ON CONFLICT DO NOTHING;
