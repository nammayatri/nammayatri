UPDATE atlas_app.integrated_bpp_config SET config_json = '{
  "tag": "ONDC",
  "contents": {
    "fareCachingAllowed": true,
    "overrideCity": "std:011",
    "redisPrefix": "delhi"
  }
}' where merchant_operating_city_id = '4e9ad934-68b5-4da3-bf79-aefe29160032' and platform_type = 'MULTIMODAL' and vehicle_category = 'METRO';