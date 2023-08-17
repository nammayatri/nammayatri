UPDATE atlas_app.merchant_service_config SET config_json = json_build_object(
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
        , 'template_id','6786861'
    )
  )
  WHERE merchant_id = 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51' AND service_name = 'Whatsapp_GupShup';  -- Change the merchant_id to merchant id of yatri sathi

UPDATE atlas_app.merchant_service_config SET config_json = json_build_object(
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
        , 'template_id','6786864'
    )
  )
  WHERE merchant_id = 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52' AND service_name = 'Whatsapp_GupShup'; -- Change the merchant_id to merchant id of namma yatri