
------------- already in master pls run in prod -----------------
--------- put appropriate creds here ----------------
INSERT INTO atlas_app.merchant_service_config (merchant_id, service_name, config_json, merchant_operating_city_id)
SELECT m.merchant_id, 'MetroPayment_Juspay',
 '{"apiKey": "APIKEY", "returnUrl": "https://api.juspay.in/end", "url": "https://api.juspay.in/", "merchantId": "nammayatri", "username": "nammayatri", "password": "PASWORD"}'
    , m.id
FROM atlas_app.merchant_operating_city m;