ALTER TABLE atlas_app.merchant
    DROP COLUMN exo_phone,
    DROP COLUMN exo_phones,
    DROP COLUMN exo_phone_country_code;

ALTER TABLE atlas_app.merchant
    ADD COLUMN exo_phones character varying(255) [];

UPDATE atlas_app.merchant
    SET exo_phones = '{"+918069457995","+918035272983"}'
    WHERE short_id = 'NAMMA_YATRI';

UPDATE atlas_app.merchant
    SET exo_phones = '{"+918069457996","+918035272987"}'
    WHERE short_id = 'YATRI';

UPDATE atlas_app.merchant
    SET exo_phones = '{ExoPhone}'
    WHERE exo_phones IS NULL;

ALTER TABLE atlas_app.merchant ALTER COLUMN exo_phones SET NOT NULL;


UPDATE atlas_app.merchant_service_usage_config
    SET initiate_call = 'Exotel';
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN initiate_call SET NOT NULL;
--NOTE : DON'T RUN THIS QUERY IN MASTER/PROD (Only For Local)
WITH MerchantCallServiceConfigs AS (
  SELECT T1.merchant_id, T1.id, 'Call_Exotel', CAST ('{
   "exotelUrl":"https://api.exotel.com/",
   "url":"https://api.exotel.com/",
   "callbackUrl":"http://localhost:8013/v1/ride/call/statusCallback",
   "apiKey":"xxxxxxx",
   "apiToken":"xxxxxxx",
   "accountSID":"xxxxxxx",
   "callerId":"xxxxxxx"
  }' AS json)
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
  (SELECT * FROM MerchantCallServiceConfigs);