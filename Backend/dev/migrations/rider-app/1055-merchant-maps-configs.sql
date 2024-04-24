-- As we are using Merchant_Operating_City to update the below queries
-- we need the insert query here
-- for local testing only
INSERT INTO atlas_app.merchant_operating_city (id, merchant_id, merchant_short_id, city) VALUES
('namma-yatri-0-0000-0000-00000000city', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'NAMMA_YATRI', 'Kochi'),
('yatri-00-0000-0000-0000-00000000city', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51', 'YATRI', 'Kochi');

-- DROP Not Null for updates
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN sms_providers_priority_list DROP NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN whatsapp_providers_priority_list DROP NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN initiate_call DROP NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN notify_person DROP NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN get_distances_for_cancel_ride DROP NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN enable_dashboard_sms DROP NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN aadhaar_verification_service DROP NOT NULL;

WITH MerchantMapsConfigs AS (
  SELECT T1.merchant_id, T1.id, 'Google', 'Google', 'Google', 'Google', 'Google', 'Google'
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_service_usage_config (merchant_id, merchant_operating_city_id, get_distances, get_routes, snap_to_road, get_place_name, get_place_details, auto_complete)
  (SELECT * FROM MerchantMapsConfigs);

-- googleKey = "mock-google-key"
WITH MerchantMapsServiceConfigs AS (
  SELECT T1.merchant_id, T1.id, 'Maps_Google', CAST ('{
   "googleMapsUrl":"http://localhost:8019/",
   "googleRoadsUrl":"http://localhost:8019/",
   "googleKey":"0.1.0|0|Cb8TKe1UWD9IkUDfVg4eE+moqGGHZIlhvXmWDqG/kS1RgdpRQF5Wxigi762BJhW5dKS9iHNnFKrZ2dm+vpuT61Bp"
  }' AS json)
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
  (SELECT * FROM MerchantMapsServiceConfigs);
