ALTER TABLE atlas_transporter.merchant
    ADD COLUMN exo_phones character varying(255) [];

UPDATE atlas_transporter.merchant
    SET exo_phones = '{ExoPhone}'
    WHERE exo_phones IS NULL;

UPDATE atlas_transporter.merchant
    SET exo_phones = '{+918047108594}'
    WHERE short_id = 'YATRI';

ALTER TABLE atlas_transporter.merchant ALTER COLUMN exo_phones SET NOT NULL;

ALTER TABLE atlas_transporter.booking
    ADD COLUMN provider_exo_phone character varying(255);

UPDATE atlas_transporter.booking
    SET provider_exo_phone = 'UNKNOWN';

ALTER TABLE atlas_transporter.booking ALTER COLUMN provider_exo_phone SET NOT NULL;

ALTER TABLE atlas_transporter.call_status
    RENAME COLUMN exotel_call_sid TO call_id;

ALTER TABLE atlas_transporter.merchant_service_usage_config
    ADD COLUMN initiate_call character varying(30);
UPDATE atlas_transporter.merchant_service_usage_config
    SET initiate_call = 'Exotel';
ALTER TABLE atlas_transporter.merchant_service_usage_config ALTER COLUMN initiate_call SET NOT NULL;

WITH MerchantCallServiceConfigs AS (
  SELECT T1.id, 'Call_Exotel', CAST ('{
   "exotelUrl":"https://api.exotel.com/",
   "callbackUrl":"http://localhost:8014/v1/driver/ride/call/statusCallback",
   "apiKey":"xxxxxxx",
   "apiToken":"xxxxxxx",
   "accountSID":"xxxxxxx",
   "callerId":"xxxxxxx"
  }' AS json)
  FROM atlas_transporter.merchant AS T1
)
INSERT INTO atlas_transporter.merchant_service_config (merchant_id, service_name, config_json)
  (SELECT * FROM MerchantCallServiceConfigs);
