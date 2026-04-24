-- Seed Aarokya insurance SDK config per merchant-operating-city.
-- apiKey is passetto-encrypted for the DEV master key; re-encrypt before running in sandbox/prod.
INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
SELECT m.merchant_id, m.id, 'InsuranceSdk_Aarokya',
  json_build_object(
      'url', 'https://426c-2405-201-d047-d024-bc8f-be96-21fc-ad23.ngrok-free.app'
    , 'apiKey', '0.1.0|3|9mjwFELebidEnnf7ONBU1wm7GXu8ByW0Aa6zVtKhNxz+TBrIRbpwvZsUS185O8T+Mhv62hdplDU+CXo='
    , 'platformId', '019db4e2-5458-7e42-a1fb-91eec5d83bac'
  )
FROM atlas_driver_offer_bpp.merchant_operating_city m
WHERE m.merchant_short_id = 'NAMMA_YATRI_PARTNER' AND m.city = 'Kochi'
ON CONFLICT DO NOTHING;
