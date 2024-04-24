CREATE INDEX idx_aadhaar_verification_aadhaar_number_hash ON atlas_app.aadhaar_verification  USING btree (aadhaar_number_hash);

UPDATE atlas_app.merchant_service_usage_config SET aadhaar_verification_service ='Gridline';
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN aadhaar_verification_service SET NOT NULL;

UPDATE atlas_app.merchant SET aadhaar_verification_try_limit=3;
UPDATE atlas_app.merchant SET aadhaar_key_expiry_time=86400;
ALTER TABLE atlas_app.merchant ALTER COLUMN aadhaar_verification_try_limit SET NOT NULL;

WITH MerchantMapsServiceConfigs AS (
  SELECT T1.merchant_id, T1.id, 'AadhaarVerification_Gridline', CAST ('{
    "url":"https://stoplight.io/mocks/gridlines/gridlines-api-docs/133154718",
    "apiKey":"0.1.0|2|nQYa7mvonFi2mfrmrDW9oiw49OYaTfm+OEoJfU02T0bIyk0SREXMsgzIsyIAB/tEArOOn3OjiTqv4cn3",
    "authType": "xxxxxxxx"
  }' AS json)
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
  (SELECT * FROM MerchantMapsServiceConfigs);
