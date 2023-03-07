INSERT INTO atlas_app.merchant_service_config (merchant_id, service_name, config_json)
SELECT m.id, 'Sms_GupShup',
  json_build_object(
      'userName','0.1.0|2|aH69syF+qmjP8wpjdwy5KdrHqhsTd1s7lH6TupSUYwMAS5rpi4jGDsA6Nt1uqGPWZxdMshc18cDhmQ=='
    , 'password','0.1.0|0|MbGCmY0OMu39bi7dEokkZ4kvgN17S+whz29QJa+XXUy+mue72jMsAHfVGd4lM9AEWbCqRywCu2RTpA=='
    , 'url','enterprise.smsgupshup.com'
    , 'templateId','0.1.0|0|MbGCmY0OMu39bi7dEokkZ4kvgN17S+whz29QJa+XXUy+mue72jMsAHfVGd4lM9AEWbCqRywCu2RTpA=='
  )
FROM atlas_app.merchant m;

UPDATE atlas_app.merchant_service_usage_config SET sms_providers_priority_list='{"MyValueFirst", "ExotelSms", "GupShup"}';