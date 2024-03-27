ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN aadhaar_verification_service character varying(30);
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config SET aadhaar_verification_service ='Gridline';
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ALTER COLUMN aadhaar_verification_service SET NOT NULL;

WITH MerchantMapsServiceConfigs AS (
  SELECT T1.id, 'AadhaarVerification_Gridline', CAST ('{
    "url":"https://stoplight.io/mocks/gridlines/gridlines-api-docs/133154718",
    "apiKey":"0.1.0|2|nQYa7mvonFi2mfrmrDW9oiw49OYaTfm+OEoJfU02T0bIyk0SREXMsgzIsyIAB/tEArOOn3OjiTqv4cn3",
    "authType": "xxxxxxxx"
  }' AS json)
  FROM atlas_driver_offer_bpp.merchant AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, service_name, config_json)
  (SELECT * FROM MerchantMapsServiceConfigs);

ALTER TABLE atlas_driver_offer_bpp.place_name_cache ALTER COLUMN place_id DROP NOT NULL ;