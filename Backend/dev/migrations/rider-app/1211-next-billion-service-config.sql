INSERT INTO atlas_app.merchant_service_config (merchant_id, service_name, config_json)
SELECT m.id, 'Maps_NextBillion',
  json_build_object(
    'nextBillionDirectionsUrl', 'https://api.nextbillion.io',
    'nextBillionKey', '23456sdftghjk45' --change this in prod and master
  )
FROM atlas_app.merchant m;