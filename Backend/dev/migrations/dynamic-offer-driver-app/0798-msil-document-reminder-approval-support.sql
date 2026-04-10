-- Step 1: Set is_approval_supported = true for all MSIL docs by default
UPDATE atlas_driver_offer_bpp.document_verification_config
SET is_approval_supported = true
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_short_id = 'MSIL_PARTNER'
);

-- Step 2: Disable approval for BankingDetails, DriverInspectionHub, InspectionHub
UPDATE atlas_driver_offer_bpp.document_verification_config
SET is_approval_supported = false
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_short_id = 'MSIL_PARTNER'
)
AND document_type IN ('BankingDetails', 'DriverInspectionHub', 'InspectionHub');

-- Step 3: Set is_reminder_supported = true for supported document types
UPDATE atlas_driver_offer_bpp.document_verification_config
SET is_reminder_supported = true
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_short_id = 'MSIL_PARTNER'
);

-- Step 4: Set is_reminder_supported = false for all other document types
UPDATE atlas_driver_offer_bpp.document_verification_config
SET is_reminder_supported = false
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_short_id = 'MSIL_PARTNER'
)
AND document_type NOT IN (
  'DriverLicense',
  'VehicleRegistrationCertificate',
  'VehicleInsurance',
  'VehiclePermit',
  'VehiclePUC',
  'VehicleFitnessCertificate',
  'BusinessLicense',
  'InspectionHub',
  'DriverInspectionHub',
  'TrainingForm'
);

-- Step 5: Set createDocumentRequired = true for MSIL_PARTNER
UPDATE atlas_driver_offer_bpp.transporter_config
SET create_document_required = true
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_short_id = 'MSIL_PARTNER'
);
