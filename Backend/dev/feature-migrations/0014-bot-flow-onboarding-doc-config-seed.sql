-- BOT-flow onboarding: DocumentVerificationConfig + FleetOwnerDocumentVerificationConfig seed
-- for the MSIL_PARTNER merchant.
--
-- Seeds ONLY the document configs the enableBotFlow flow relies on. It does NOT flip
-- transporter_config.enable_bot_flow (deliberate separate step) — applying this is inert until the
-- flag is turned on for the city.
--
-- This city onboards BOTH DCO drivers and fleet drivers, so applicableTo matters:
--   * Driver docs required by everyone     -> applicable_to = 'FLEET_AND_INDIVIDUAL'
--   * Driver OperatorPartnerCode (DCO-only) -> applicable_to = 'INDIVIDUAL'
--   * Fleet OperatorPartnerCode (fleet enable) -> fleet_owner_document_verification_config (no applicable_to)
--
-- Enable-only docs (gate `enabled`, not `verified`): is_mandatory=false, is_mandatory_for_enabling=true.
--   * MSDS (DrivingSchoolCertificate) + MedicalCertificate are BOT/ops-uploaded -> is_hidden=true,
--     roles_allowed_to_upload_document_text='{OPERATOR,ADMIN}', is_image_validation_required=true
--     (no provider -> VALID on upload).
--   * OperatorPartnerCode is derived from the driver/fleet<->operator association (not an upload).
--   * Rating is an optional driver-uploaded screenshot (gates nothing).
--
-- Targets every operating city of merchant short_id = 'MSIL_PARTNER', vehicle_category = 'CAR'.
-- Idempotent: UPDATEs are conditional; INSERTs use ON CONFLICT DO NOTHING + a normalise UPDATE after.

-- ============================================================
-- atlas_driver_offer_bpp
-- ============================================================

-- RUN IN MASTER

------------------------------------------------------------------------------------------------------
-- A) Driver docs must apply to everyone: applicable_to 'FLEET' -> 'FLEET_AND_INDIVIDUAL'.
--    With both DCOs and fleet drivers here, 'FLEET' would make these NON-blocking for DCOs
--    (applicableToDriver (Just False) FLEET = False) -> a DCO could be verified/enabled with no docs.
--    (Vehicle docs are excluded — the vehicle path ignores applicable_to.)
------------------------------------------------------------------------------------------------------
UPDATE atlas_driver_offer_bpp.document_verification_config
SET applicable_to = 'FLEET_AND_INDIVIDUAL', updated_at = CURRENT_TIMESTAMP
WHERE document_type IN (
        'DriverLicense', 'AadhaarCard', 'PanCard', 'ProfilePhoto',
        'LocalResidenceProof', 'PoliceVerificationCertificate', 'DriverInspectionHub', 'Permissions'
      )
  AND vehicle_category = 'CAR'
  AND applicable_to = 'FLEET'
  AND merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER');

------------------------------------------------------------------------------------------------------
-- B) Inspection-hub docs mandatory so `verified` waits on ops-inspection approval.
------------------------------------------------------------------------------------------------------
UPDATE atlas_driver_offer_bpp.document_verification_config
SET is_mandatory = true, updated_at = CURRENT_TIMESTAMP
WHERE document_type IN ('InspectionHub', 'DriverInspectionHub')
  AND vehicle_category = 'CAR'
  AND merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER');

------------------------------------------------------------------------------------------------------
-- C) MSDS (DrivingSchoolCertificate): re-enable + make it enable-only (BOT/ops-uploaded).
--    Base config has is_disabled=true, is_mandatory=true. Flip to active enable-only doc.
--    Keeps its existing is_hidden=true + roles_allowed_to_upload_document_text='{OPERATOR,ADMIN}'.
------------------------------------------------------------------------------------------------------
UPDATE atlas_driver_offer_bpp.document_verification_config
SET is_disabled = false,
    is_mandatory = false,
    is_mandatory_for_enabling = true,
    applicable_to = 'FLEET_AND_INDIVIDUAL',
    is_image_validation_required = true,
    updated_at = CURRENT_TIMESTAMP
WHERE document_type = 'DrivingSchoolCertificate'
  AND vehicle_category = 'CAR'
  AND merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER');

------------------------------------------------------------------------------------------------------
-- D) MedicalCertificate (enable-only, BOT/ops-uploaded — mirrors MSDS). Insert if absent; copy
--    NOT-NULL columns from the DriverLicense CAR template, override the rest.
------------------------------------------------------------------------------------------------------
INSERT INTO atlas_driver_offer_bpp.document_verification_config (
    document_type, merchant_id, merchant_operating_city_id, vehicle_category,
    title, description, is_mandatory, is_mandatory_for_enabling, is_disabled, is_hidden,
    dependency_document_type, check_extraction, check_expiry, supported_vehicle_classes_json,
    vehicle_class_check_type, rc_number_prefix_list, max_retry_count,
    is_default_enabled_on_manual_verification, is_image_validation_required, do_strict_verifcation,
    "order", applicable_to, document_category, roles_allowed_to_upload_document_text, created_at, updated_at
)
SELECT
    'MedicalCertificate', tmpl.merchant_id, tmpl.merchant_operating_city_id, tmpl.vehicle_category,
    'Medical Certificate', NULL, false, true, false, true,
    '{}', false, false, tmpl.supported_vehicle_classes_json,
    tmpl.vehicle_class_check_type, '{}', tmpl.max_retry_count,
    true, true, false,
    14, 'FLEET_AND_INDIVIDUAL', 'Driver', '{OPERATOR,ADMIN}', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
FROM atlas_driver_offer_bpp.document_verification_config tmpl
WHERE tmpl.document_type = 'DriverLicense'
  AND tmpl.vehicle_category = 'CAR'
  AND tmpl.merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER')
ON CONFLICT DO NOTHING;

-- normalise MedicalCertificate flags if the row already existed
UPDATE atlas_driver_offer_bpp.document_verification_config
SET is_mandatory = false, is_mandatory_for_enabling = true, is_disabled = false, is_hidden = true,
    applicable_to = 'FLEET_AND_INDIVIDUAL', is_image_validation_required = true,
    roles_allowed_to_upload_document_text = '{OPERATOR,ADMIN}', updated_at = CURRENT_TIMESTAMP
WHERE document_type = 'MedicalCertificate'
  AND vehicle_category = 'CAR'
  AND merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER');

------------------------------------------------------------------------------------------------------
-- E) Driver OperatorPartnerCode (DCO-only enable gate). applicable_to='INDIVIDUAL' so fleet drivers
--    skip it. Status is derived from the active driver<->operator association (not an upload), so it
--    is driver-facing (is_hidden=false) and is_image_validation_required=false.
------------------------------------------------------------------------------------------------------
INSERT INTO atlas_driver_offer_bpp.document_verification_config (
    document_type, merchant_id, merchant_operating_city_id, vehicle_category,
    title, description, is_mandatory, is_mandatory_for_enabling, is_disabled, is_hidden,
    dependency_document_type, check_extraction, check_expiry, supported_vehicle_classes_json,
    vehicle_class_check_type, rc_number_prefix_list, max_retry_count,
    is_default_enabled_on_manual_verification, is_image_validation_required, do_strict_verifcation,
    "order", applicable_to, document_category, roles_allowed_to_upload_document_text, created_at, updated_at
)
SELECT
    'OperatorPartnerCode', tmpl.merchant_id, tmpl.merchant_operating_city_id, tmpl.vehicle_category,
    'Operator Partner Code', NULL, false, true, false, false,
    '{}', false, false, tmpl.supported_vehicle_classes_json,
    tmpl.vehicle_class_check_type, '{}', tmpl.max_retry_count,
    true, false, false,
    15, 'INDIVIDUAL', 'Driver', NULL, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
FROM atlas_driver_offer_bpp.document_verification_config tmpl
WHERE tmpl.document_type = 'DriverLicense'
  AND tmpl.vehicle_category = 'CAR'
  AND tmpl.merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER')
ON CONFLICT DO NOTHING;

-- normalise driver OperatorPartnerCode flags if the row already existed
UPDATE atlas_driver_offer_bpp.document_verification_config
SET is_mandatory = false, is_mandatory_for_enabling = true, is_disabled = false,
    applicable_to = 'INDIVIDUAL', updated_at = CURRENT_TIMESTAMP
WHERE document_type = 'OperatorPartnerCode'
  AND vehicle_category = 'CAR'
  AND merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER');

------------------------------------------------------------------------------------------------------
-- F) Fleet OperatorPartnerCode — the fleet `enabled` gate. Lives in
--    fleet_owner_document_verification_config (PK = document_type + merchant_operating_city_id + role).
--    Seed for BOTH fleet roles. is_mandatory=false (excluded from fleet verified),
--    is_mandatory_for_enabling=true (required for fleet enabled; set only via fleet<->operator assoc).
------------------------------------------------------------------------------------------------------
INSERT INTO atlas_driver_offer_bpp.fleet_owner_document_verification_config (
    document_type, merchant_id, merchant_operating_city_id, role,
    title, description, is_mandatory, is_mandatory_for_enabling, is_disabled, is_hidden,
    dependency_document_type, check_extraction, check_expiry, document_category,
    is_default_enabled_on_manual_verification, is_image_validation_required, do_strict_verifcation,
    "order", max_retry_count, created_at, updated_at
)
SELECT
    'OperatorPartnerCode', tmpl.merchant_id, tmpl.merchant_operating_city_id, tmpl.role,
    'Operator Partner Code', NULL, false, true, false, true,
    '{}', false, false, NULL,
    tmpl.is_default_enabled_on_manual_verification, tmpl.is_image_validation_required, tmpl.do_strict_verifcation,
    99, tmpl.max_retry_count, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
FROM atlas_driver_offer_bpp.fleet_owner_document_verification_config tmpl
WHERE tmpl.document_type = 'AadhaarCard'
  AND tmpl.role IN ('FLEET_OWNER', 'FLEET_BUSINESS')
  AND tmpl.merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER')
ON CONFLICT DO NOTHING;

-- normalise fleet OperatorPartnerCode flags if rows already existed
UPDATE atlas_driver_offer_bpp.fleet_owner_document_verification_config
SET is_mandatory = false, is_mandatory_for_enabling = true, is_disabled = false, updated_at = CURRENT_TIMESTAMP
WHERE document_type = 'OperatorPartnerCode'
  AND role IN ('FLEET_OWNER', 'FLEET_BUSINESS')
  AND merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER');

------------------------------------------------------------------------------------------------------
-- G) Rating (OPTIONAL, driver-uploaded screenshot of ratings from other platforms). Gates nothing:
--    is_mandatory=false AND is_mandatory_for_enabling=false. is_image_validation_required=true so the
--    uploaded screenshot lands VALID (no provider to auto-validate it).
------------------------------------------------------------------------------------------------------
INSERT INTO atlas_driver_offer_bpp.document_verification_config (
    document_type, merchant_id, merchant_operating_city_id, vehicle_category,
    title, description, is_mandatory, is_mandatory_for_enabling, is_disabled, is_hidden,
    dependency_document_type, check_extraction, check_expiry, supported_vehicle_classes_json,
    vehicle_class_check_type, rc_number_prefix_list, max_retry_count,
    is_default_enabled_on_manual_verification, is_image_validation_required, do_strict_verifcation,
    "order", applicable_to, document_category, roles_allowed_to_upload_document_text, created_at, updated_at
)
SELECT
    'Rating', tmpl.merchant_id, tmpl.merchant_operating_city_id, tmpl.vehicle_category,
    'Other Platform Rating Screenshot', NULL, false, false, false, false,
    '{}', false, false, tmpl.supported_vehicle_classes_json,
    tmpl.vehicle_class_check_type, '{}', tmpl.max_retry_count,
    true, true, false,
    16, 'FLEET_AND_INDIVIDUAL', 'Driver', NULL, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
FROM atlas_driver_offer_bpp.document_verification_config tmpl
WHERE tmpl.document_type = 'DriverLicense'
  AND tmpl.vehicle_category = 'CAR'
  AND tmpl.merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER')
ON CONFLICT DO NOTHING;

-- normalise Rating flags if the row already existed
UPDATE atlas_driver_offer_bpp.document_verification_config
SET is_mandatory = false, is_mandatory_for_enabling = false, is_image_validation_required = true,
    updated_at = CURRENT_TIMESTAMP
WHERE document_type = 'Rating'
  AND vehicle_category = 'CAR'
  AND merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER');
