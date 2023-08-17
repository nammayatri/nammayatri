UPDATE atlas_driver_offer_bpp.merchant_service_config
SET config_json =
  json_build_object(
      'apiKey', config_json ->> 'apiKey'
    , 'returnUrl', config_json ->> 'returnUrl'
    , 'url', config_json ->> 'url'
    , 'merchantId', 'yatrisathi'
    , 'username', config_json ->> 'username'
    , 'password', config_json ->> 'password'
	)
 WHERE merchant_service_config.service_name='Payment_Juspay';
