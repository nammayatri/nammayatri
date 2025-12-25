INSERT INTO atlas_app.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
SELECT m.merchant_id, m.id, 'Sms_GupShup',
  json_build_object(
      'userName','0.1.0|2|z47JjbeG+bn+dOzrt3lfOtFe71jyw1rIiXI9ledgFzrSaky7M5quZXE5YpSs2hbfcyjC00BV4pia6cTUDw=='
    , 'password','0.1.0|2|z47JjbeG+bn+dOzrt3lfOtFe71jyw1rIiXI9ledgFzrSaky7M5quZXE5YpSs2hbfcyjC00BV4pia6cTUDw=='
    , 'url','enterprise.smsgupshup.com'
    , 'templateId','0.1.0|2|z47JjbeG+bn+dOzrt3lfOtFe71jyw1rIiXI9ledgFzrSaky7M5quZXE5YpSs2hbfcyjC00BV4pia6cTUDw=='
  )
FROM atlas_app.merchant_operating_city m;

UPDATE atlas_app.merchant_service_usage_config SET sms_providers_priority_list='{"MyValueFirst", "ExotelSms", "GupShup"}';