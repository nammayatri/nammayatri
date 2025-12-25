WITH MerchantNotificationServiceConfigs AS (
  SELECT T1.merchant_id, T1.id, 'Notification_FCM', CAST ('{
   "fcmUrl":"https://fcm.googleapis.com/v1/projects/jp-beckn-dev/messages:send/",
   "fcmServiceAccount":"xxxxxxxx",
   "fcmTokenKeyPrefix":"NAMMA_YATRI"
  }' AS json)
  FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, merchant_operating_city_id ,service_name, config_json)
  (SELECT * FROM MerchantNotificationServiceConfigs);


WITH MerchantNotificationServiceConfigs AS (
  SELECT T1.merchant_id, T1.id, 'Notification_GRPC', CAST ('{
   "defaultTtl": 600
  }' AS json)
  FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, merchant_operating_city_id ,service_name, config_json)
  (SELECT * FROM MerchantNotificationServiceConfigs);