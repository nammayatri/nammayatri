UPDATE atlas_app.merchant_service_usage_config SET whatsapp_providers_priority_list='{"GupShup"}';
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN whatsapp_providers_priority_list SET NOT NULL;




INSERT INTO atlas_app.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
SELECT m.merchant_id, m.id, 'Whatsapp_GupShup',
  json_build_object(
      'userid','0.1.0|2|4TznBOoSN+IX1nqtp//6xKcsUkE2h5taPgSI4fk0bnKM8Naas4LzfhULFaloqeAY+TDj28exmIRAFg=='
    , 'password','0.1.0|2|4TznBOoSN+IX1nqtp//6xKcsUkE2h5taPgSI4fk0bnKM8Naas4LzfhULFaloqeAY+TDj28exmIRAFg=='
    , 'auth_scheme','plain'
    , 'channel','whatsapp'
    , 'v','1.1'
    , 'url','https://media.smsgupshup.com/GatewayAPI/rest'
    , 'format','json'
    , 'otp_cfg',json_build_object(
          'msg_type','Text'
        , 'method','SendMessage'
        , 'template_id','6602327'
    )
  )
FROM atlas_app.merchant_operating_city m;
