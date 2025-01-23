--- NOTE: Don't run below query in master or prod
WITH MerchantWalletServiceConfigs AS (
  SELECT T1.merchant_id, T1.id, 'Wallet_GoogleWallet', CAST ('{
  "privateKeyId": "xxxxxxxxx",
  "clientEmail": "xx@example.com",
  "tokenUri": "https://oauth2.googleapis.com/token",
  "issuerId": "xxxxxxxxxxx"
  }' AS json)
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
  (SELECT * FROM MerchantWalletServiceConfigs);