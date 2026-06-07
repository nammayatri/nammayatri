UPDATE atlas_driver_offer_bpp.merchant_service_config
SET config_json = config_json || '{"faceCompareRetryLimit": 1}'::jsonb,
    updated_at = now()
WHERE service_name = 'Verification_Idfy'
  AND merchant_operating_city_id IN (
    SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'MSIL_PARTNER'
  );
