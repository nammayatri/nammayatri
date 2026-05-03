-- Seed Aarokya insurance SDK config per merchant-operating-city.
-- basicToken is passetto-encrypted for the DEV master key; re-encrypt before running in sandbox/prod.
-- Stored value is the base64 of "username:password" (no "Basic " prefix); the prefix is added in Flow.hs.
INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
SELECT m.merchant_id, m.id, 'PartnerSdk_Aarokya',
  json_build_object(
      'url', 'https://aarokya.sandbox.juspay.in'
    , 'basicToken', '0.1.0|1|kFdG5jiAwqaX3Q7xunLldGH1JltWMF8NByRMIZ2zIAo/pD66ZVV5UMYwLDV50tXerx5e8AlxbZ8qVGOBg+uQm9PcvmEmHyb47lCgba6oBzV57YfQ10rH+8JC84+l8aS/p37+beJyzdM6xx0ZheeRSuakxUgZ9WosXcBtH9CmeMmM22zL8aN/03ECNfrdPxUuSVNIOmWTf/7bTIUb3VrxBhhP/ThRjC06o/CcQalYjb6CcvFGG18RaKPgiZzveZU59SgJo+s317GLjn701Hw1'
  )
FROM atlas_driver_offer_bpp.merchant_operating_city m
WHERE m.merchant_short_id = 'NAMMA_YATRI_PARTNER' AND m.city = 'Kochi'
ON CONFLICT DO NOTHING;
