CREATE TABLE atlas_app.merchant_service_config (
    merchant_id character(36) NOT NULL REFERENCES atlas_app.merchant (id),
    service_name character varying(30) NOT NULL,
    config_json json NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY (merchant_id, service_name)
);

-- DROP Not Null for updates
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN smsProvidersPriorityList DROP NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN whatsappProvidersPriorityList DROP NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN initiateCall DROP NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN notifyPerson DROP NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN getDistancesForCancelRide DROP NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN enableDashboardSms DROP NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN aadhaarVerificationService DROP NOT NULL;

WITH MerchantMapsConfigs AS (
  SELECT T1.merchant_id, T1.id, 'Google', 'Google', 'Google', 'Google', 'Google', 'Google'
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_service_usage_config (merchant_id, merchant_operating_city_id, get_distances, get_routes, snap_to_road, get_place_name, get_place_details, auto_complete)
  (SELECT * FROM MerchantMapsConfigs);

-- googleKey = "mock-google-key"
WITH MerchantMapsServiceConfigs AS (
  SELECT T1.id, 'Maps_Google', CAST ('{
   "googleMapsUrl":"http://localhost:8019/",
   "googleRoadsUrl":"http://localhost:8019/",
   "googleKey":"0.1.0|0|Cb8TKe1UWD9IkUDfVg4eE+moqGGHZIlhvXmWDqG/kS1RgdpRQF5Wxigi762BJhW5dKS9iHNnFKrZ2dm+vpuT61Bp"
  }' AS json)
  FROM atlas_app.merchant AS T1
)
INSERT INTO atlas_app.merchant_service_config (merchant_id, service_name, config_json)
  (SELECT * FROM MerchantMapsServiceConfigs);
