
-- Disable image validation for MSIL_PARTNER Delhi.
UPDATE atlas_driver_offer_bpp.document_verification_config
SET is_image_validation_required = false
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_short_id = 'MSIL_PARTNER' AND city = 'Delhi'
);

-- Enable mark_image_valid_on_validation_skip for all MSIL_PARTNER cities.
-- When true, image validation is skipped and the image is directly marked VALID.
UPDATE atlas_driver_offer_bpp.document_verification_config
SET mark_image_valid_on_validation_skip = true
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_short_id = 'MSIL_PARTNER'
);

UPDATE atlas_driver_offer_bpp.fleet_owner_document_verification_config
SET mark_image_valid_on_validation_skip = true
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_short_id = 'MSIL_PARTNER'
);

-- Allow operator/fleet roles to upload these document types for MSIL_PARTNER.
UPDATE atlas_driver_offer_bpp.document_verification_config
SET roles_allowed_to_upload_document_text = '{OPERATOR,FLEET_OWNER,FLEET_BUSINESS,DRIVER}'::text[]
WHERE document_type IN (
    'OperatorPartnerCode',
    'MedicalCertificate',
    'BankingDetails',
    'NomineeDetails',
    'DrivingSchoolCertificate',
    'BotApproval',
    'DriverInspectionHub',
    'Rating',
    'InspectionHub'
  )
  AND merchant_operating_city_id IN (
    SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'MSIL_PARTNER'
  );

UPDATE atlas_driver_offer_bpp.fleet_owner_document_verification_config
SET roles_allowed_to_upload_document_text = '{OPERATOR,FLEET_OWNER,FLEET_BUSINESS,DRIVER}'::text[]
WHERE document_type IN (
    'OperatorPartnerCode',
    'BankingDetails',
    'BotApproval'
  )
  AND merchant_operating_city_id IN (
    SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'MSIL_PARTNER'
  );

ALTER TABLE atlas_driver_offer_bpp.driver_udyam ALTER COLUMN document_image_id SET DEFAULT '29e68d5e-cd70-4d39-bd55-a2fe831524eb'; -- Please do not run this query in production.
