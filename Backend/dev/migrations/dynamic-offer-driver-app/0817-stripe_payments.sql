-- DUMMY CONFIG ONLY FOR LOCAL --
-- dummy apiKey is 2345678901
INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
SELECT m.merchant_id, m.id, 'Payment_Stripe',
  json_build_object(
      'apiKey','0.1.0|0|HUzSrOsk4BCSSYE1WhOV6IIY0H+yqXFP1j+8PNjsrEnIac7qHbvSw0US5Pfs6cikXX+UbpYBOPfRRdc8OA=='
    , 'returnUrl','dummyReturnUrl'
    , 'refreshUrl','dummyRefreshUrl'
    , 'url', 'dummyUrl'
    , 'businessProfile', null
    , 'chargeDestination', 'ConnectedAccount'
    , 'webhookEndpointSecret', null
    , 'webhookToleranceSeconds', null
  )
FROM atlas_driver_offer_bpp.merchant_operating_city m;
