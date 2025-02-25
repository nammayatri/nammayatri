--NOTE : DON'T RUN THIS QUERY IN MASTER/PROD (Only For Local)
INSERT INTO atlas_app.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
SELECT m.merchant_id, m.id, 'Sms_MyValueFirst',
  json_build_object(
      'username', '0.1.0|2|aH69syF+qmjP8wpjdwy5KdrHqhsTd1s7lH6TupSUYwMAS5rpi4jGDsA6Nt1uqGPWZxdMshc18cDhmQ==',
      'password', '0.1.0|0|MbGCmY0OMu39bi7dEokkZ4kvgN17S+whz29QJa+XXUy+mue72jMsAHfVGd4lM9AEWbCqRywCu2RTpA==',
      'url', 'http://localhost:4343',
      'token', 'dummy-token-12345'
  )
FROM atlas_app.merchant_operating_city m;

INSERT INTO atlas_app.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
SELECT m.merchant_id, m.id, 'Sms_ExotelSms',
  json_build_object(
      'apiKey','0.1.0|2|aH69syF+qmjP8wpjdwy5KdrHqhsTd1s7lH6TupSUYwMAS5rpi4jGDsA6Nt1uqGPWZxdMshc18cDhmQ=='
    , 'apiToken','0.1.0|0|MbGCmY0OMu39bi7dEokkZ4kvgN17S+whz29QJa+XXUy+mue72jMsAHfVGd4lM9AEWbCqRywCu2RTpA=='
    , 'sid','juspay2m'
    , 'url','api.in.exotel.com'
  )
FROM atlas_app.merchant_operating_city m;

-- ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN send_s_m_s character varying(30);

-- UPDATE atlas_app.merchant_service_usage_config
-- SET send_s_m_s = 'MyValueFirst';