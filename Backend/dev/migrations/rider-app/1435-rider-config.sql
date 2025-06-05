update atlas_app.rider_config set user_service_tier_order_config = ARRAY[
    '{"vehicle": "AUTO_RICKSHAW", "orderArray": ["AUTO_RICKSHAW","ECO","TAXI","COMFY","SUV", "SUV_PLUS"]}'::jsonb,
    '{"vehicle": "TAXI", "orderArray": ["ECO","TAXI","AUTO_RICKSHAW","COMFY","SUV", "SUV_PLUS"]}'::jsonb
  ] where merchant_operating_city_id = (
    SELECT id
    FROM atlas_app.merchant_operating_city
    WHERE city = 'Bangalore' AND merchant_short_id = 'NAMMA_YATRI'
);