
ALTER TABLE atlas_app.merchant_service_config DROP CONSTRAINT merchant_service_config_pkey;
ALTER TABLE atlas_app.merchant_service_config ADD PRIMARY KEY(service_name, merchant_operating_city_id);
--------- put appropriate creds here ----------------
INSERT INTO atlas_app.merchant_service_config (merchant_id, service_name, config_json)
SELECT m.id, 'MetroPayment_Juspay',
 '{"apiKey": "APIKEY", "returnUrl": "https://api.juspay.in/end", "url": "https://api.juspay.in/", "merchantId": "nammayatri", "username": "nammayatri", "password": "PASWORD"}'
FROM atlas_app.merchant m;