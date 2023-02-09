INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, service_name, config_json)
SELECT m.id, 'Sms_MyValueFirst',
  json_build_object(
      'username','0.1.0|2|aH69syF+qmjP8wpjdwy5KdrHqhsTd1s7lH6TupSUYwMAS5rpi4jGDsA6Nt1uqGPWZxdMshc18cDhmQ=='
    , 'password','00.1.0|0|MbGCmY0OMu39bi7dEokkZ4kvgN17S+whz29QJa+XXUy+mue72jMsAHfVGd4lM9AEWbCqRywCu2RTpA=='
    , 'url','http://localhost:4343'
  )
FROM atlas_driver_offer_bpp.merchant m;

INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, service_name, config_json)
SELECT m.id, 'Sms_ExotelSms',
  json_build_object(
      'apiKey','0.1.0|2|aH69syF+qmjP8wpjdwy5KdrHqhsTd1s7lH6TupSUYwMAS5rpi4jGDsA6Nt1uqGPWZxdMshc18cDhmQ=='
    , 'apiToken','0.1.0|0|MbGCmY0OMu39bi7dEokkZ4kvgN17S+whz29QJa+XXUy+mue72jMsAHfVGd4lM9AEWbCqRywCu2RTpA=='
    , 'sid','juspay2m'
    , 'url','api.in.exotel.com'
  )
FROM atlas_driver_offer_bpp.merchant m;

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN send_s_m_s character varying(30);

UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
SET send_s_m_s = 'MyValueFirst';