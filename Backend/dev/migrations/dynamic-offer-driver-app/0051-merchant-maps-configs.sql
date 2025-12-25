-- CREATE TABLE atlas_driver_offer_bpp.merchant_service_config (
--     merchant_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.merchant (id),
--     service_name character varying(30) NOT NULL,
--     config_json json NOT NULL,
--     updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
--     created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
--     PRIMARY KEY (merchant_id, service_name)
-- );

-- for local testing only
ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ALTER COLUMN state DROP NOT NULL;
INSERT INTO atlas_driver_offer_bpp.merchant_operating_city (id, merchant_id, merchant_short_id, city) VALUES
('favorit0-0000-0000-0000-00000000city', 'favorit0-0000-0000-0000-00000favorit', 'NAMMA_YATRI_PARTNER', 'Kochi'),
('nearest-drivers-testing-org00000city', 'nearest-drivers-testing-organization', 'OTHER_MERCHANT_2', 'Kochi');


-- ONLY FOR LOCAL
WITH MerchantMapsConfigs AS (
  SELECT T1.merchant_id, T1.id, 'Google', 'Google', 'Google', 'Google', 'Google', 'Google',
    '{"MyValueFirst", "ExotelSms", "GupShup"}':: text[],
    'Google',
    '{"GupShup"}' :: text[],
    'Idfy',
    'Exotel',
    'OSRM',
    'Gridline',
    '{"GovtData", "Idfy"}' :: text[],
    'SafetyPortal',
    'InternalScripts',
    '{"FCM", "GRPC"}' :: text[]
  FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_service_usage_config (merchant_id, merchant_operating_city_id, get_distances, get_routes, snap_to_road, get_place_name, get_place_details, auto_complete,
   sms_providers_priority_list,
   get_estimated_pickup_distances,
   whatsapp_providers_priority_list,
   verification_service,
   initiate_call,
   get_distances_for_cancel_ride,
   aadhaar_verification_service,
   verification_providers_priority_list,
   driver_background_verification_service,
   face_verification_service,
   send_search_request_to_driver
  )
  (SELECT * FROM MerchantMapsConfigs);

-- googleKey = "mock-google-key"
-- LOCAL ONLY
WITH MerchantMapsServiceConfigs AS (
  SELECT T1.merchant_id, T1.id, 'Maps_Google', CAST ('{
   "googleMapsUrl":"http://localhost:8019/",
   "googleRoadsUrl":"http://localhost:8019/",
   "googleKey":"0.1.0|0|Cb8TKe1UWD9IkUDfVg4eE+moqGGHZIlhvXmWDqG/kS1RgdpRQF5Wxigi762BJhW5dKS9iHNnFKrZ2dm+vpuT61Bp"
  }' AS json)
  FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
  (SELECT * FROM MerchantMapsServiceConfigs);
