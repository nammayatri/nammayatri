ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config
    ADD COLUMN initiate_call character varying(30);
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
    SET initiate_call = 'Exotel';
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ALTER COLUMN initiate_call SET NOT NULL;

WITH MerchantCallServiceConfigs AS (
  SELECT T1.id, 'Call_Exotel', CAST ('{
   "exotelUrl":"https://api.exotel.com/",
   "callbackUrl":"http://localhost:8016/ui/driver/ride/call/statusCallback",
   "apiKey":"xxxxxxx",
   "apiToken":"xxxxxxx",
   "accountSID":"xxxxxxx",
   "callerId":"xxxxxxx"
  }' AS json)
  FROM atlas_driver_offer_bpp.merchant AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, service_name, config_json)
  (SELECT * FROM MerchantCallServiceConfigs);
