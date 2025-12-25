INSERT INTO atlas_app.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
VALUES ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'f8e9db0a-96c8-49e4-942a-3e3f7265d2da', 'Payment_AAJuspay',
  json_build_object(
      'apiKey','0.1.0|0|VY3WecEzNJn3rvaDw/79+GZ7646zswnsxky32OCyQmfebZnmH/Ij8y1bD1/B6iYUSvKmEBfWXgN8gqYPZN/YLjQpQoyuasnr7pcPMuwZ120u'
    , 'returnUrl','https://router.sandbox.juspay.in/end'
    , 'url','https://router.sandbox.juspay.in/'
    , 'merchantId', 'nammayatri'
    , 'username', 'nammayatri'
    , 'password','0.1.0|2|uPWdROAMEp2GPWze72pSsa610cjp/5vZz7wSrgru+w1soG6tw2hJT3LwkpLIcB8ZN/Kqx+EmC/PQFYqeL+UzbgEH'
    , 'serviceMode', 'AA'
  ));