-- Seed Aarokya insurance SDK config per merchant-operating-city.
-- apiKey is passetto-encrypted for the DEV master key; re-encrypt before running in sandbox/prod.
INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
SELECT m.merchant_id, m.id, 'PartnerSdk_Aarokya',
  json_build_object(
      'url', 'https://aarokya.sandbox.juspay.in/'
    , 'apiKey', '0.1.0|2|S5jEpEGqTAXxcL1ZEFp1GoEaBglJ/cUiLwhnqFLy1xAjFY7n1CSdxRfk0G7QDX5rbL/jcn1g3fAzFtIFegI='
    , 'platformId', '019db4e2-5458-7e42-a1fb-91eec5d83bac'
  )
FROM atlas_driver_offer_bpp.merchant_operating_city m
WHERE m.merchant_short_id = 'NAMMA_YATRI_PARTNER' AND m.city = 'Kochi'
ON CONFLICT DO NOTHING;
