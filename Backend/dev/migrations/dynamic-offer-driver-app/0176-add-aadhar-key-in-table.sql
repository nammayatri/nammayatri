ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN aadhaar_verification_service character varying(30);
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config SET aadhaar_verification_service ='Gridline';
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ALTER COLUMN aadhaar_verification_service SET NOT NULL;

WITH MerchantMapsServiceConfigs AS (
  SELECT T1.id, 'AadhaarVerification_Gridline', CAST ('{
    "url":"https://stoplight.io/mocks/gridlines/gridlines-api-docs/133154718",
    "apiKey":"xxxxxxx",
    "authType": "xxxxxxx"
  }' AS json)
  FROM atlas_driver_offer_bpp.merchant AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, service_name, config_json)
  (SELECT * FROM MerchantMapsServiceConfigs);


CREATE TABLE atlas_driver_offer_bpp.aadhaar_otp_req(
    id character(36) NOT NULL PRIMARY KEY,
    driver_id character(36) NOT NULL  REFERENCES atlas_driver_offer_bpp.person (id),
    request_id Text NOT NULL,
    status_code Text NOT NULL,
    request_message Text NOT NULL,
    transaction_id Text,
    created_at timestamp with time zone NOT NULL DEFAULT now()
);

CREATE TABLE atlas_driver_offer_bpp.aadhaar_otp_verify(
    id character(36) NOT NULL PRIMARY KEY,
    driver_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.person (id),
    request_id Text NOT NULL,
    status_code Text NOT NULL,
    request_message Text NOT NULL,
    transaction_id Text,
    created_at timestamp with time zone NOT NULL DEFAULT now()
);


CREATE TABLE atlas_driver_offer_bpp.aadhaar_verification(
    id character(36) NOT NULL PRIMARY KEY,
    driver_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.person (id),
    driver_name Text,
    driver_gender Text,
    driver_dob Text,
    driver_image Text,
    created_at timestamp with time zone NOT NULL DEFAULT now()
);
