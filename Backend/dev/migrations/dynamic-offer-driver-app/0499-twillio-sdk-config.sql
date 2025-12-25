INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
SELECT m.merchant_id, m.id, 'Call_TwillioCall',
  json_build_object(
    'apiKey','0.1.0|2|mpe/RP8r5di6zw6ZcajApuhodiyViAmy6tSDzb84zgk1vxkIzOe0Uo52RIVF64V4ByN26ulbv6XiYVlqou6fSYM7zREcZ97kGm8E/1cygY0qbMkqfQ==',
    'apiKeySecret','0.1.0|2|mpe/RP8r5di6zw6ZcajApuhodiyViAmy6tSDzb84zgk1vxkIzOe0Uo52RIVF64V4ByN26ulbv6XiYVlqou6fSYM7zREcZ97kGm8E/1cygY0qbMkqfQ==',
    'applicationSid','abcd',
    'accountSid', 'abcd',
    'pushCredentialSidAndroid','abcd',
    'pushCredentialSidIos','abcd'
  )
FROM atlas_driver_offer_bpp.merchant_operating_city m;