-- Update Aarokya insurance SDK basicToken for Kochi (NAMMA_YATRI_PARTNER).
-- basicToken is passetto-encrypted for the DEV master key; re-encrypt before running in sandbox/prod.
UPDATE atlas_driver_offer_bpp.merchant_service_config
SET config_json = json_build_object(
      'url', 'https://aarokya.sandbox.juspay.in'
    , 'basicToken', '0.1.0|2|yY3wAPv6ZVXOolwHWhb0HMqIHXxJe/TXmaVBsGdS9UQ6ru6dzCO/VRivQxZtrmAOLhdJrvMp/Nf0QA3KJJMXfP7HBeRCgdxEt95rWZ0dI4cyzeQ06qTJuQlmtmxmTv3dT7hJDPJgBMAasmk0RzSuHYtFqdGeAbG0PH++cMpV7Z6kKtSKwkZAkjymzu/mmi6BMQyuWHcG+q+X+gw2fPPPMuf4G2t7BGYC1wRd2laCmikzz+97s2gnx/1s7N+tjpLGydLIpmg4l8kL8rNB7wxH'
  )
WHERE service_name = 'PartnerSdk_Aarokya'
  AND merchant_operating_city_id IN (
    SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'NAMMA_YATRI_PARTNER' AND city = 'Kochi'
  );
