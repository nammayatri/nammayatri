-- DON'T RUN THIS MIGRATION IN PRODUCTION/MASTER
INSERT INTO atlas_app.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
SELECT m.merchant_id, m.id, 'Payment_Stripe',
  json_build_object(
      'apiKey','0.1.0|2|aH69syF+qmjP8wpjdwy5KdrHqhsTd1s7lH6TupSUYwMAS5rpi4jGDsA6Nt1uqGPWZxdMshc18cDhmQ=='
    , 'returnUrl','dummyReturnUrl'
    , 'refreshUrl','dummyRefreshUrl'
    , 'url','dummyUrl'
    , 'chargeDestination', 'Platform'
  )
FROM atlas_app.merchant_operating_city m where m.id in (select id from atlas_app.merchant_operating_city where city = 'Kochi');

ALTER TABLE atlas_app.payment_order add column service_provider text default 'Juspay';