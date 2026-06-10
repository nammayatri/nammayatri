-- BOT-flow onboarding: DocumentVerificationConfig + FleetOwnerDocumentVerificationConfig seed
-- for the MSIL_PARTNER merchant.
--
-- Seeds ONLY the document configs the enableBotFlow flow relies on. It does NOT flip
-- transporter_config.enable_bot_flow (deliberate separate step) — applying this is inert until the
-- flag is turned on for the city.
--
-- This city onboards BOTH DCO drivers and fleet drivers, so applicableTo matters:
--   * Driver docs required by everyone  -> applicable_to = 'FLEET_AND_INDIVIDUAL'
--   * Driver OperatorCode (DCO-only)     -> applicable_to = 'INDIVIDUAL'
--   * Fleet OperatorCode (fleet enable)  -> lives in fleet_owner_document_verification_config (no applicable_to)
--   * Rating (optional screenshot)       -> is_mandatory=false, is_mandatory_for_enabling=false (gates nothing)
--
-- Targets every operating city of merchant short_id = 'MSIL_PARTNER', vehicle_category = 'CAR'.
-- Idempotent: UPDATEs are conditional; INSERTs use ON CONFLICT DO NOTHING.

-- ============================================================
-- Driver App (atlas_driver_offer_bpp)
-- ============================================================

-- RUN IN MASTER

------------------------------------------------------------------------------------------------------
-- A) Fix applicableTo on mandatory DRIVER docs: 'FLEET' -> 'FLEET_AND_INDIVIDUAL'.
--    With both DCOs and fleet drivers in this city, applicable_to='FLEET' would make these docs
--    NON-blocking for DCOs (applicableToDriver (Just False) FLEET = False) → a DCO could be
--    verified/enabled with no documents. They must apply to everyone.
--    (Vehicle docs are excluded — the vehicle path ignores applicable_to.)
------------------------------------------------------------------------------------------------------
UPDATE atlas_driver_offer_bpp.document_verification_config dvc
SET applicable_to = 'FLEET_AND_INDIVIDUAL',
    updated_at = CURRENT_TIMESTAMP
WHERE dvc.document_type IN (
        'DriverLicense', 'AadhaarCard', 'PanCard', 'ProfilePhoto',
        'LocalResidenceProof', 'PoliceVerificationCertificate', 'DriverInspectionHub', 'Permissions'
      )
  AND dvc.vehicle_category = 'CAR'
  AND dvc.applicable_to = 'FLEET'
  AND dvc.merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER');

------------------------------------------------------------------------------------------------------
-- B) Inspection-hub docs mandatory so `verified` waits on ops-inspection approval.
--    (Already true in this city — kept for idempotency / other cities.)
------------------------------------------------------------------------------------------------------
UPDATE atlas_driver_offer_bpp.document_verification_config dvc
SET is_mandatory = true,
    updated_at = CURRENT_TIMESTAMP
WHERE dvc.document_type IN ('InspectionHub', 'DriverInspectionHub')
  AND dvc.vehicle_category = 'CAR'
  AND dvc.merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER');

------------------------------------------------------------------------------------------------------
-- C) MSDS (DrivingSchoolCertificate): re-enable + make it enable-only.
--    Currently is_disabled=true → won't be active. Set is_disabled=false so the BOT can upload it
--    and it gates `enabled`. Excluded from `verified` (isMandatory=false), required for `enabled`.
------------------------------------------------------------------------------------------------------
UPDATE atlas_driver_offer_bpp.document_verification_config dvc
SET is_disabled = false,
    is_mandatory = false,
    is_mandatory_for_enabling = true,
    applicable_to = 'FLEET_AND_INDIVIDUAL',
    -- BOT-uploaded; no verification provider for MSDS. is_image_validation_required=true makes the
    -- upload path mark it VALID directly (castImageType -> RC, Idfy validationAvailable=False -> no
    -- checkErrors -> VALID), same mechanism as PoliceVerificationCertificate / LocalResidenceProof.
    is_image_validation_required = true,
    updated_at = CURRENT_TIMESTAMP
WHERE dvc.document_type = 'DrivingSchoolCertificate'
  AND dvc.vehicle_category = 'CAR'
  AND dvc.merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER');

------------------------------------------------------------------------------------------------------
-- D) MedicalCertificate (enable-only, applies to everyone). Insert if absent; copy NOT-NULL columns
--    from an existing CAR config template, then override.
------------------------------------------------------------------------------------------------------
INSERT INTO atlas_driver_offer_bpp.document_verification_config (
    document_type, merchant_id, merchant_operating_city_id, vehicle_category,
    title, description, is_mandatory, is_mandatory_for_enabling, is_disabled, is_hidden,
    dependency_document_type, check_extraction, check_expiry, supported_vehicle_classes_json,
    vehicle_class_check_type, rc_number_prefix_list, max_retry_count,
    is_default_enabled_on_manual_verification, is_image_validation_required, do_strict_verifcation,
    "order", applicable_to, document_category, created_at, updated_at
)
SELECT
    'MedicalCertificate', tmpl.merchant_id, tmpl.merchant_operating_city_id, tmpl.vehicle_category,
    'Medical Certificate', NULL, false, true, false, false,
    '{}', false, false, tmpl.supported_vehicle_classes_json,
    tmpl.vehicle_class_check_type, '{}', tmpl.max_retry_count,
    -- is_image_validation_required=true: BOT-uploaded, no provider → VALID on upload (castImageType
    -- -> RC, Idfy validationAvailable=False -> no checkErrors -> VALID).
    true, true, false,
    tmpl."order", 'FLEET_AND_INDIVIDUAL', 'Driver', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
FROM atlas_driver_offer_bpp.document_verification_config tmpl
WHERE tmpl.document_type = 'DriverLicense'
  AND tmpl.vehicle_category = 'CAR'
  AND tmpl.merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER')
ON CONFLICT DO NOTHING;

------------------------------------------------------------------------------------------------------
-- E) Driver OperatorCode (DCO-only enable gate, applicable_to='INDIVIDUAL'). Insert if absent.
--    Fleet drivers skip it (applicableToDriver (Just True) INDIVIDUAL = False); DCOs require it.
------------------------------------------------------------------------------------------------------
INSERT INTO atlas_driver_offer_bpp.document_verification_config (
    document_type, merchant_id, merchant_operating_city_id, vehicle_category,
    title, description, is_mandatory, is_mandatory_for_enabling, is_disabled, is_hidden,
    dependency_document_type, check_extraction, check_expiry, supported_vehicle_classes_json,
    vehicle_class_check_type, rc_number_prefix_list, max_retry_count,
    is_default_enabled_on_manual_verification, is_image_validation_required, do_strict_verifcation,
    "order", applicable_to, document_category, created_at, updated_at
)
SELECT
    'OperatorCode', tmpl.merchant_id, tmpl.merchant_operating_city_id, tmpl.vehicle_category,
    'Operator Code', NULL, false, true, false, false,
    '{}', false, false, tmpl.supported_vehicle_classes_json,
    tmpl.vehicle_class_check_type, '{}', tmpl.max_retry_count,
    true, false, false,
    tmpl."order", 'INDIVIDUAL', 'Driver', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
FROM atlas_driver_offer_bpp.document_verification_config tmpl
WHERE tmpl.document_type = 'DriverLicense'
  AND tmpl.vehicle_category = 'CAR'
  AND tmpl.merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER')
ON CONFLICT DO NOTHING;

-- Normalise D/E flags if the rows already existed:
UPDATE atlas_driver_offer_bpp.document_verification_config dvc
SET is_mandatory = false, is_mandatory_for_enabling = true, is_disabled = false, updated_at = CURRENT_TIMESTAMP
WHERE dvc.document_type IN ('MedicalCertificate', 'OperatorCode')
  AND dvc.vehicle_category = 'CAR'
  AND dvc.merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER');

UPDATE atlas_driver_offer_bpp.document_verification_config dvc
SET applicable_to = 'INDIVIDUAL', updated_at = CURRENT_TIMESTAMP
WHERE dvc.document_type = 'OperatorCode'
  AND dvc.vehicle_category = 'CAR'
  AND dvc.merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER');

-- Medical may not exist in the master DB (the INSERT above creates it), but in environments where the
-- row already exists the INSERT is a no-op (ON CONFLICT DO NOTHING) and wouldn't update its flags.
-- This UPDATE forces is_image_validation_required=true regardless, so VALID-on-upload always applies.
UPDATE atlas_driver_offer_bpp.document_verification_config dvc
SET applicable_to = 'FLEET_AND_INDIVIDUAL', is_image_validation_required = true, updated_at = CURRENT_TIMESTAMP
WHERE dvc.document_type = 'MedicalCertificate'
  AND dvc.vehicle_category = 'CAR'
  AND dvc.merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER');

------------------------------------------------------------------------------------------------------
-- F) Fleet OperatorCode — the fleet `enabled` gate. Lives in fleet_owner_document_verification_config
--    (no vehicle_category / applicable_to columns; PK = document_type + merchant_operating_city_id + role).
--    Seed for BOTH fleet roles. isMandatory=false (excluded from fleet verified),
--    isMandatoryForEnabling=true (required for fleet enabled; only the BOT can make it VALID).
------------------------------------------------------------------------------------------------------
INSERT INTO atlas_driver_offer_bpp.fleet_owner_document_verification_config (
    document_type, merchant_id, merchant_operating_city_id, role,
    title, description, is_mandatory, is_mandatory_for_enabling, is_disabled, is_hidden,
    dependency_document_type, check_extraction, check_expiry, document_category,
    is_default_enabled_on_manual_verification, is_image_validation_required, do_strict_verifcation,
    "order", max_retry_count, created_at, updated_at
)
SELECT
    'OperatorCode', tmpl.merchant_id, tmpl.merchant_operating_city_id, tmpl.role,
    'Operator Code', NULL, false, true, false, true,
    '{}', false, false, NULL,
    tmpl.is_default_enabled_on_manual_verification, tmpl.is_image_validation_required, tmpl.do_strict_verifcation,
    99, tmpl.max_retry_count, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
FROM atlas_driver_offer_bpp.fleet_owner_document_verification_config tmpl
WHERE tmpl.document_type = 'AadhaarCard'
  AND tmpl.role IN ('FLEET_OWNER', 'FLEET_BUSINESS')
  AND tmpl.merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER')
ON CONFLICT DO NOTHING;

-- Normalise fleet OperatorCode flags if rows already existed (force is_disabled=false so a
-- pre-existing disabled row still functions as the fleet enable gate — idempotent across envs):
UPDATE atlas_driver_offer_bpp.fleet_owner_document_verification_config fodvc
SET is_mandatory = false, is_mandatory_for_enabling = true, is_disabled = false, updated_at = CURRENT_TIMESTAMP
WHERE fodvc.document_type = 'OperatorCode'
  AND fodvc.role IN ('FLEET_OWNER', 'FLEET_BUSINESS')
  AND fodvc.merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER');

------------------------------------------------------------------------------------------------------
-- G) Rating (OPTIONAL): driver uploads a screenshot of their rating from other platforms (Uber/Rapido/etc).
--    is_mandatory=false AND is_mandatory_for_enabling=false → does NOT gate verified or enabled
--    (purely informational; collected for review). Image-based: status read via commonDocStatus from the
--    uploaded doc/image. applicable_to = FLEET_AND_INDIVIDUAL.
------------------------------------------------------------------------------------------------------
INSERT INTO atlas_driver_offer_bpp.document_verification_config (
    document_type, merchant_id, merchant_operating_city_id, vehicle_category,
    title, description, is_mandatory, is_mandatory_for_enabling, is_disabled, is_hidden,
    dependency_document_type, check_extraction, check_expiry, supported_vehicle_classes_json,
    vehicle_class_check_type, rc_number_prefix_list, max_retry_count,
    is_default_enabled_on_manual_verification, is_image_validation_required, do_strict_verifcation,
    "order", applicable_to, document_category, created_at, updated_at
)
SELECT
    'Rating', tmpl.merchant_id, tmpl.merchant_operating_city_id, tmpl.vehicle_category,
    'Other Platform Rating Screenshot', NULL, false, false, false, false,
    '{}', false, false, tmpl.supported_vehicle_classes_json,
    tmpl.vehicle_class_check_type, '{}', tmpl.max_retry_count,
    -- is_image_validation_required=true: OPS-uploaded screenshot, no provider to auto-validate it.
    -- This makes the upload path mark it VALID directly (castImageType -> RC, Idfy
    -- validationAvailable=False -> no checkErrors -> VALID), so it lands VALID, not MANUAL.
    true, true, false,
    tmpl."order", 'FLEET_AND_INDIVIDUAL', 'Driver', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
FROM atlas_driver_offer_bpp.document_verification_config tmpl
WHERE tmpl.document_type = 'DriverLicense'
  AND tmpl.vehicle_category = 'CAR'
  AND tmpl.merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER')
ON CONFLICT DO NOTHING;

-- Rating may not exist in the master DB (the INSERT above creates it), but where the row already
-- exists the INSERT is a no-op (ON CONFLICT DO NOTHING). This UPDATE forces the flags regardless:
-- keeps it optional (gates nothing) and is_image_validation_required=true so VALID-on-upload applies.
UPDATE atlas_driver_offer_bpp.document_verification_config dvc
SET is_mandatory = false, is_mandatory_for_enabling = false, is_image_validation_required = true, updated_at = CURRENT_TIMESTAMP
WHERE dvc.document_type = 'Rating'
  AND dvc.vehicle_category = 'CAR'
  AND dvc.merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER');
