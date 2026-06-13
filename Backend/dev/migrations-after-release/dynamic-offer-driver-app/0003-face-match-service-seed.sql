-- Seed face_match_service and face_match_source_doc for MSIL_PARTNER onboarding.
-- For MSIL, all driver onboarding docs (DL/PAN/Aadhaar) must be face-matched against the selfie.
-- face_match_service: IDfy's /v3/tasks/sync/compare/face API (two-image face comparison).
-- face_match_source_doc: ProfilePhoto (the selfie is the trusted reference).

-- Set face_match_service = 'Idfy' for all MSIL opCities' merchant_service_usage_config.
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
SET face_match_service = 'Idfy'
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER')
);

-- Set face_match_source_doc = 'ProfilePhoto' for DL/PAN/Aadhaar in MSIL opCities.
UPDATE atlas_driver_offer_bpp.document_verification_config
SET face_match_source_doc = 'ProfilePhoto'
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER')
)
AND document_type IN ('DriverLicense', 'PanCard', 'AadhaarCard');

-- Add faceCompareRetryLimit to MSIL's Idfy merchant_service_config.
-- This enables automatic retry on transient IDfy API failures (timeout, network hiccup).
UPDATE atlas_driver_offer_bpp.merchant_service_config
SET config_json = COALESCE(config_json, '{}'::jsonb) || '{"faceCompareRetryLimit": 1}'::jsonb,
    updated_at = now()
WHERE service_name = 'Verification_Idfy'
AND merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER')
);
