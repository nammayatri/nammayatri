-- local sync
WITH MerchantCallServiceConfigs AS (
  SELECT T1.merchant_id, T1.id, 'Call_Exotel', CAST ('{
   "exotelUrl":"https://api.exotel.com/",
   "url":"https://api.exotel.com/",
   "callbackUrl":"http://localhost:8016/ui/driver/ride/call/statusCallback",
   "apiKey":"xxxxxxx",
   "apiToken":"xxxxxxx",
   "accountSID":"xxxxxxx",
   "callerId":"xxxxxxx"
  }' AS json)
  FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
  (SELECT * FROM MerchantCallServiceConfigs);