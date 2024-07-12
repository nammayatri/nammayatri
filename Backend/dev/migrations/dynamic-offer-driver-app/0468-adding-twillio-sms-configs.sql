insert into atlas_driver_offer_bpp.merchant_service_config(merchant_id, merchant_operating_city_id, service_name, config_json, created_at, updated_at)
select
    city.merchant_id,
    city.id,
    'Sms_TwillioSms',
    '{"messageServiceId": "0.1.0|2|aH69syF+qmjP8wpjdwy5KdrHqhsTd1s7lH6TupSUYwMAS5rpi4jGDsA6Nt1uqGPWZxdMshc18cDhmQ==","accountSid": "0.1.0|2|aH69syF+qmjP8wpjdwy5KdrHqhsTd1s7lH6TupSUYwMAS5rpi4jGDsA6Nt1uqGPWZxdMshc18cDhmQ==","authToken": "0.1.0|2|aH69syF+qmjP8wpjdwy5KdrHqhsTd1s7lH6TupSUYwMAS5rpi4jGDsA6Nt1uqGPWZxdMshc18cDhmQ==","url": "https://dummy-url.com"}',
    now(),
    now()
from atlas_driver_offer_bpp.merchant_operating_city city where merchant_short_id = 'BRIDGE_CABS_PARTNER';