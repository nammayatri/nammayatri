-- Local-testing config overrides for the BOT onboarding flow.
--
-- Every .sql file in Backend/dev/local-testing-data/ is applied automatically on
-- each local stack startup by test-context-api's run_startup_local_testing_data()
-- (see Backend/dev/test-tool/context-api/server.py). Files are applied in sorted
-- order, AFTER the Haskell services report ready, so keep every statement idempotent
-- (UPDATEs are naturally re-appliable; guard INSERTs with NOT EXISTS / ON CONFLICT).
--
-- NOTE: transporter_config, document_verification_config,
-- fleet_owner_document_verification_config and merchant_service_usage_config are cached
-- in-process by driver-app, so edits here land in Postgres but take effect only after
-- driver-app restarts.
--
-- Each file is applied in its OWN transaction, so everything below is all-or-nothing:
-- one failing statement rolls back the whole file, BOT role/token seeds included.
--
-- All tables below live in the same DB, so they can be updated in one transaction.

-- BEGIN;

-- ────────────────────────────────────────────────────────────────────────
-- 1. transporter_config  (atlas_driver_offer_bpp)
-- ────────────────────────────────────────────────────────────────────────

update atlas_driver_offer_bpp.transporter_config set
    requires_onboarding_inspection = true,
    requires_driver_onboarding_inspection = true,
    enable_bot_flow = true,
    enable_pull_pending_doc_verification = true
WHERE merchant_id = '840327a8-f17c-4d7c-8199-a583cfaadc5f'
  AND merchant_operating_city_id = 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e';




-- ────────────────────────────────────────────────────────────────────────
-- 2. document_verification_config (DVC)  (atlas_driver_offer_bpp)
-- ────────────────────────────────────────────────────────────────────────
-- The per-document onboarding rules the driver + vehicle flows read: which documents are
-- mandatory for `verified`, which gate `enabled`, upload order, dependencies, and which
-- roles may upload on someone else's behalf.
--
-- FULL REPLACE for (this city, vehicle_category = 'CAR'): every row below is upserted on
-- PK (document_type, merchant_operating_city_id, vehicle_category), then anything else on
-- CAR is deleted. Rows for other vehicle categories and other cities are never touched.
--
-- 21 rows: 14 Driver + 6 Vehicle, then
-- VehicleRegistrationCertificate is a plain UPDATE, not part of the upsert above. Its payload
-- deliberately leaves out supported_vehicle_classes_json and is_default_verified_on_manual_verification,
-- and an UPDATE never writes a column outside its SET list — so neither appears anywhere in this file.
-- (An INSERT could not do that: supported_vehicle_classes_json is NOT NULL with no default, so it
-- would have to invent a value.) The RC's accepted class list is owned upstream by config-sync;
-- writing '[]' over it would match no vehicle class and RC registration would silently never
-- produce a row. The UPDATE is simply a no-op if config-sync has not seeded the row yet.
--
-- Two flags decide different things and are easy to confuse (Common.hs isDocRequiredFor):
--   ForVerified  reads is_mandatory
--   ForEnabling  reads is_mandatory_for_enabling (falling back to is_mandatory when NULL)
-- and applicable_to is only consulted for a document that is required in that mode, so an
-- INDIVIDUAL-only row with is_mandatory = false can never affect a driver's `verified`.
--
-- NOTE: cached in-process by driver-app -> a change here needs a driver-app RESTART.

INSERT INTO atlas_driver_offer_bpp.document_verification_config
    (document_type, merchant_operating_city_id, vehicle_category, merchant_id, do_strict_verifcation, role, document_fields_json, rc_number_prefix_list, document_onboarding_stage, is_mandatory_for_enabling, is_approval_supported, applicable_to, check_expiry, is_mandatory, is_reminder_supported, disable_warning, max_retry_count, is_disabled, updated_at, filter_for_old_apks, is_hidden, allow_license_transfer, "order", title, only_image_verification_status_lookup_required, document_category, face_match_source_doc, created_at, vehicle_class_check_type, supported_vehicle_classes_json, is_default_verified_on_manual_verification, is_default_enabled_on_manual_verification, is_image_validation_required, document_flow_grouping, check_extraction, dependency_document_type, mark_image_valid_on_validation_skip, roles_allowed_to_upload_document_text, description)
VALUES
    ('OperatorPartnerCode', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'CAR', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, '{}', NULL, true, NULL, 'INDIVIDUAL', false, true, NULL, NULL, 4, false, '2026-06-23T14:09:55.501018Z', false, true, NULL, 0, 'Operator Partner Code', NULL, 'Driver', NULL, '2026-06-15T11:45:29.294626Z', 'Infix', '[]'::json, NULL, false, false, 'STANDARD', true, '{}', true, '{OPERATOR,FLEET_OWNER,FLEET_BUSINESS,DRIVER}', 'Operator Partner Code to link with the Operator'),
    ('BankingDetails', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'CAR', '840327a8-f17c-4d7c-8199-a583cfaadc5f', false, NULL, NULL, '{DL,KA,HR,TS,TG,AP}', NULL, false, false, 'INDIVIDUAL', false, false, false, NULL, 4, false, '2026-06-23T14:09:55.501018Z', false, false, true, 6, 'Banking Details', NULL, 'Driver', NULL, '2026-02-14T11:25:42.272866Z', 'Infix', '[]'::json, NULL, false, false, 'COMMON', true, '{}', true, '{OPERATOR,FLEET_OWNER,FLEET_BUSINESS,DRIVER}', 'Bank account information for payments (optional)'),
    ('PoliceVerificationCertificate', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'CAR', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, '{DL,KA,HR,TS,TG,AP}', NULL, true, true, 'FLEET_AND_INDIVIDUAL', false, true, false, NULL, 4, false, '2026-06-23T14:09:55.501018Z', false, false, NULL, 12, 'Police Verification Certificate', NULL, 'Driver', NULL, '2026-02-13T08:44:30.147931Z', 'Infix', '[]'::json, NULL, false, false, 'STANDARD', true, '{}', true, NULL, 'Police Verification Certificate is required for the driver to start driving.'),
    ('NomineeDetails', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'CAR', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, '{}', NULL, false, NULL, 'FLEET_AND_INDIVIDUAL', false, true, NULL, NULL, 4, false, '2026-06-23T14:09:55.501018Z', false, false, NULL, 13, 'Nominee Details', NULL, 'Driver', NULL, '2026-06-16T14:30:29.00163Z', 'Infix', '[]'::json, NULL, false, false, 'STANDARD', true, '{}', true, '{OPERATOR,FLEET_OWNER,FLEET_BUSINESS,DRIVER}', 'Nominee Details of the Driver'),
    ('LocalResidenceProof', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'CAR', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, '{DL,KA,HR,TS,TG,AP}', NULL, true, true, 'FLEET_AND_INDIVIDUAL', false, true, false, NULL, 4, false, '2026-06-23T14:09:55.501018Z', false, false, NULL, 11, 'Local Residence Proof', NULL, 'Driver', NULL, '2026-02-13T08:44:30.139809Z', 'Infix', '[]'::json, NULL, false, false, 'STANDARD', true, '{}', true, NULL, 'Local Residence Proof is required for the driver to start driving.'),
    ('DriverLicense', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'CAR', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, '{DL,KA,HR,TS,TG,AP}', NULL, true, true, 'FLEET_AND_INDIVIDUAL', true, true, true, '', 4, false, '2026-06-23T14:01:23.83966Z', false, false, false, 2, 'Driving License', NULL, 'Driver', 'ProfilePhoto', '2025-02-26T10:19:34.42611Z', 'Infix', '["AUTORICKSHAW", "LMV", "3W-NT", "3WT", "3W-T", "LIGHT MOTOR VEHICLE", "3W-CAB", "ARNT"]'::json, NULL, false, false, 'STANDARD', true, '{ProfilePhoto}', true, NULL, NULL),
    ('DrivingSchoolCertificate', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'CAR', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, '{DL,KA,HR,TS,TG,AP}', NULL, true, true, 'FLEET_AND_INDIVIDUAL', false, false, false, NULL, 4, false, '2026-06-23T14:09:55.501018Z', false, true, NULL, 13, 'Driving School Certificate', NULL, 'Driver', NULL, '2026-02-13T08:44:30.152679Z', 'Infix', '[]'::json, NULL, false, false, 'STANDARD', true, '{}', true, '{OPERATOR,FLEET_OWNER,FLEET_BUSINESS,DRIVER}', 'MSDS Certificate is required for the driver to start driving.'),
    ('MedicalCertificate', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'CAR', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, '{}', NULL, true, NULL, 'FLEET_AND_INDIVIDUAL', false, false, NULL, NULL, 4, false, '2026-06-23T14:09:55.501018Z', false, true, NULL, 0, 'Medical Test Certificate', NULL, 'Driver', NULL, '2026-06-15T15:15:14.664229Z', 'Infix', '[]'::json, NULL, false, false, 'STANDARD', true, '{}', true, '{OPERATOR,FLEET_OWNER,FLEET_BUSINESS,DRIVER}', 'Medical Test Certificate of the Driver'),
    ('DriverVehicleNOC', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'CAR', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, '{}', NULL, NULL, NULL, 'FLEET_AND_INDIVIDUAL', false, false, NULL, NULL, 4, false, '2026-06-23T14:09:55.478953Z', false, false, NULL, 0, 'Driver NOC', NULL, 'Driver', NULL, '2026-06-18T13:53:08.100054Z', 'Infix', '[]'::json, NULL, false, false, 'STANDARD', true, '{}', true, NULL, 'Driver NOC'),
    ('BotApproval', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'CAR', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, '{}', NULL, true, NULL, 'FLEET_AND_INDIVIDUAL', false, false, NULL, NULL, 4, false, '2026-06-23T14:09:55.478953Z', false, true, NULL, 17, 'BOT Approval', NULL, 'Driver', NULL, '2026-06-22T00:00:00Z', 'Infix', '[]'::json, NULL, false, false, 'STANDARD', false, '{ProfilePhoto,DriverLicense,AadhaarCard,DriverInspectionHub,OperatorPartnerCode,MedicalCertificate,DrivingSchoolCertificate}', true, '{OPERATOR,FLEET_OWNER,FLEET_BUSINESS,DRIVER}', 'BOT approval gate for the driver'),
    ('DriverInspectionHub', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'CAR', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, '{DL,KA,HR,TS,TG,AP}', NULL, true, false, 'FLEET_AND_INDIVIDUAL', false, true, true, NULL, 4, false, '2026-07-01T15:23:17.066645Z', false, false, NULL, 14, 'Driver Operation Hub Flow', NULL, 'Driver', NULL, '2026-02-15T13:03:15.030389Z', 'Infix', '[]'::json, NULL, false, false, 'STANDARD', true, '{ProfilePhoto,PanCard,AadhaarCard,LocalResidenceProof,PoliceVerificationCertificate,NomineeDetails}', true, '{OPERATOR,FLEET_OWNER,FLEET_BUSINESS,DRIVER}', 'Driver Inspection Hub flow'),
    ('ProfilePhoto', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'CAR', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, '{DL,KA,HR,TS,TG,AP}', NULL, true, true, 'FLEET_AND_INDIVIDUAL', false, true, false, '', 4, false, '2026-06-23T14:09:55.501018Z', false, false, true, 1, 'Profile Photo', NULL, 'Driver', NULL, '2025-02-26T10:19:34.42611Z', 'Infix', '[]'::json, NULL, false, false, 'STANDARD', true, '{}', true, NULL, NULL),
    ('PanCard', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'CAR', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, '{DL,KA,HR,TS,TG,AP}', NULL, true, true, 'FLEET_AND_INDIVIDUAL', false, true, false, '', 4, false, '2026-06-23T14:01:23.83966Z', false, false, true, 5, 'PAN Card', NULL, 'Driver', 'ProfilePhoto', '2025-02-26T10:19:34.42611Z', 'Infix', '[]'::json, NULL, false, false, 'STANDARD', true, '{ProfilePhoto,DriverLicense}', true, NULL, NULL),
    ('AadhaarCard', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'CAR', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, '{DL,KA,HR,TS,TG,AP}', NULL, true, true, 'FLEET_AND_INDIVIDUAL', false, true, false, '', 4, false, '2026-06-23T14:01:23.83966Z', false, false, true, 4, 'Aadhaar Card', NULL, 'Driver', 'ProfilePhoto', '2025-02-26T10:19:34.42611Z', 'Infix', '[]'::json, NULL, false, false, 'STANDARD', true, '{ProfilePhoto,DriverLicense}', true, NULL, NULL),
    ('VehiclePUC', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'CAR', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, '{DL,KA,HR,TS,TG,AP}', NULL, true, true, 'INDIVIDUAL', false, true, true, 'RC is Mandatory', 4, false, '2026-06-23T14:09:55.501018Z', false, false, true, 10, 'Vehicle PUC Certificate', true, 'Vehicle', NULL, '2025-02-26T10:19:34.42611Z', 'Infix', '[]'::json, NULL, true, false, 'STANDARD', true, '{VehicleRegistrationCertificate}', true, NULL, NULL),
    ('VehicleFitnessCertificate', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'CAR', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, '{DL,KA,HR,TS,TG,AP}', NULL, true, true, 'INDIVIDUAL', false, true, true, 'RC is Mandatory', 4, false, '2026-06-23T14:09:55.501018Z', false, false, true, 8, 'Fitness Certificate (FC)', true, 'Vehicle', NULL, '2025-02-26T10:19:34.42611Z', 'Infix', '[]'::json, NULL, true, false, 'STANDARD', true, '{VehicleRegistrationCertificate}', true, NULL, NULL),
    ('VehicleInspectionForm', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'CAR', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, '{DL,KA,HR,TS,TG,AP}', NULL, true, true, 'INDIVIDUAL', false, true, false, NULL, 4, false, '2026-06-23T14:09:55.501018Z', false, false, true, 12, 'Vehicle Photos', true, 'Vehicle', NULL, '2025-03-19T11:27:10.66144Z', 'Infix', '[]'::json, NULL, true, false, 'STANDARD', true, '{VehicleRegistrationCertificate}', true, NULL, 'Vehicle Inspection Form'),
    ('VehiclePermit', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'CAR', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, '{DL,KA,HR,TS,TG,AP}', NULL, true, true, 'INDIVIDUAL', false, true, true, 'RC is Mandatory', 4, false, '2026-06-23T14:09:55.501018Z', false, false, true, 7, 'Vehicle Permit', true, 'Vehicle', NULL, '2025-02-26T10:19:34.42611Z', 'Infix', '[]'::json, NULL, true, false, 'STANDARD', true, '{VehicleRegistrationCertificate}', true, NULL, NULL),
    ('InspectionHub', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'CAR', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, '{DL,KA,HR,TS,TG,AP}', NULL, true, false, 'INDIVIDUAL', false, true, true, NULL, 4, false, '2026-06-23T14:09:55.501018Z', false, false, true, 13, 'Vehicle Operation Hub Flow', true, 'Vehicle', NULL, '2025-04-07T10:29:11.3468Z', 'Infix', '[]'::json, NULL, true, false, 'STANDARD', true, '{VehicleRegistrationCertificate,VehicleInsurance,VehicleFitnessCertificate,VehiclePUC,VehicleInspectionForm}', true, '{OPERATOR,FLEET_OWNER,FLEET_BUSINESS,DRIVER}', 'Operation Hub Flow'),
    ('VehicleInsurance', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'CAR', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, '{DL,KA,HR,TS,TG,AP}', NULL, true, true, 'INDIVIDUAL', false, true, true, 'RC is Mandatory', 4, false, '2026-06-23T14:09:55.501018Z', false, false, true, 9, 'Vehicle Insurance', true, 'Vehicle', NULL, '2025-02-26T10:19:34.42611Z', 'Infix', '[]'::json, NULL, true, false, 'STANDARD', true, '{VehicleRegistrationCertificate}', true, NULL, NULL)
ON CONFLICT (document_type, merchant_operating_city_id, vehicle_category) DO UPDATE SET
    merchant_id = EXCLUDED.merchant_id,
    do_strict_verifcation = EXCLUDED.do_strict_verifcation,
    role = EXCLUDED.role,
    document_fields_json = EXCLUDED.document_fields_json,
    rc_number_prefix_list = EXCLUDED.rc_number_prefix_list,
    document_onboarding_stage = EXCLUDED.document_onboarding_stage,
    is_mandatory_for_enabling = EXCLUDED.is_mandatory_for_enabling,
    is_approval_supported = EXCLUDED.is_approval_supported,
    applicable_to = EXCLUDED.applicable_to,
    check_expiry = EXCLUDED.check_expiry,
    is_mandatory = EXCLUDED.is_mandatory,
    is_reminder_supported = EXCLUDED.is_reminder_supported,
    disable_warning = EXCLUDED.disable_warning,
    max_retry_count = EXCLUDED.max_retry_count,
    is_disabled = EXCLUDED.is_disabled,
    updated_at = EXCLUDED.updated_at,
    filter_for_old_apks = EXCLUDED.filter_for_old_apks,
    is_hidden = EXCLUDED.is_hidden,
    allow_license_transfer = EXCLUDED.allow_license_transfer,
    "order" = EXCLUDED."order",
    title = EXCLUDED.title,
    only_image_verification_status_lookup_required = EXCLUDED.only_image_verification_status_lookup_required,
    document_category = EXCLUDED.document_category,
    face_match_source_doc = EXCLUDED.face_match_source_doc,
    created_at = EXCLUDED.created_at,
    vehicle_class_check_type = EXCLUDED.vehicle_class_check_type,
    supported_vehicle_classes_json = EXCLUDED.supported_vehicle_classes_json,
    is_default_verified_on_manual_verification = EXCLUDED.is_default_verified_on_manual_verification,
    is_default_enabled_on_manual_verification = EXCLUDED.is_default_enabled_on_manual_verification,
    is_image_validation_required = EXCLUDED.is_image_validation_required,
    document_flow_grouping = EXCLUDED.document_flow_grouping,
    check_extraction = EXCLUDED.check_extraction,
    dependency_document_type = EXCLUDED.dependency_document_type,
    mark_image_valid_on_validation_skip = EXCLUDED.mark_image_valid_on_validation_skip,
    roles_allowed_to_upload_document_text = EXCLUDED.roles_allowed_to_upload_document_text,
    description = EXCLUDED.description;

-- VehicleRegistrationCertificate — see the note above.
UPDATE atlas_driver_offer_bpp.document_verification_config SET
    merchant_id = '840327a8-f17c-4d7c-8199-a583cfaadc5f',
    do_strict_verifcation = true,
    role = NULL,
    document_fields_json = NULL,
    rc_number_prefix_list = '{DL,KA,HR,TS,TG,AP}',
    document_onboarding_stage = NULL,
    is_mandatory_for_enabling = true,
    is_approval_supported = true,
    applicable_to = 'INDIVIDUAL',
    check_expiry = false,
    is_mandatory = true,
    is_reminder_supported = true,
    disable_warning = '',
    max_retry_count = 4,
    is_disabled = false,
    updated_at = '2026-06-30T10:15:05.603592Z',
    filter_for_old_apks = false,
    is_hidden = false,
    allow_license_transfer = true,
    "order" = 1,
    title = 'Vehicle Registration Certificate',
    only_image_verification_status_lookup_required = NULL,
    document_category = 'Vehicle',
    face_match_source_doc = NULL,
    created_at = '2025-02-26T10:19:34.42611Z',
    vehicle_class_check_type = 'Infix',
    is_default_enabled_on_manual_verification = true,
    is_image_validation_required = false,
    document_flow_grouping = 'STANDARD',
    check_extraction = true,
    dependency_document_type = '{}',
    mark_image_valid_on_validation_skip = true,
    roles_allowed_to_upload_document_text = NULL,
    description = NULL
WHERE document_type = 'VehicleRegistrationCertificate'
  AND merchant_operating_city_id = 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e'
  AND vehicle_category = 'CAR';

-- Anything else on CAR for this city is stale (e.g. Permissions, Rating) and would keep
-- taking part in the verified/enabling checks. Scoped to vehicle_category = 'CAR' so other
-- categories — and every other city — are untouched.
DELETE FROM atlas_driver_offer_bpp.document_verification_config
WHERE merchant_operating_city_id = 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e'
  AND vehicle_category = 'CAR'
  AND document_type NOT IN (
        'AadhaarCard',
        'BankingDetails',
        'BotApproval',
        'DriverInspectionHub',
        'DriverLicense',
        'DriverVehicleNOC',
        'DrivingSchoolCertificate',
        'InspectionHub',
        'LocalResidenceProof',
        'MedicalCertificate',
        'NomineeDetails',
        'OperatorPartnerCode',
        'PanCard',
        'PoliceVerificationCertificate',
        'ProfilePhoto',
        'VehicleFitnessCertificate',
        'VehicleInspectionForm',
        'VehicleInsurance',
        'VehiclePUC',
        'VehiclePermit',
        'VehicleRegistrationCertificate'
      );

-- ────────────────────────────────────────────────────────────────────────
-- 3. fleet_owner_document_verification_config (Fleet Owner DVC)  (atlas_driver_offer_bpp)
-- ────────────────────────────────────────────────────────────────────────
-- The fleet owner's own document set, a separate table from the driver+vehicle DVC above and
-- keyed by role rather than vehicle category — FLEET_OWNER rows drive the individual fleet
-- flow, FLEET_BUSINESS rows the business fleet flow.
--
-- FULL REPLACE for this city: 22 rows upserted on PK
-- (document_type, merchant_operating_city_id, role), then anything else for this city is
-- deleted. This table has no vehicle_category column.
--
-- NOTE: cached in-process by driver-app -> a change here needs a driver-app RESTART.

INSERT INTO atlas_driver_offer_bpp.fleet_owner_document_verification_config
    (document_type, merchant_operating_city_id, role, merchant_id, do_strict_verifcation, document_fields_json, document_onboarding_stage, is_mandatory_for_enabling, is_approval_supported, check_expiry, is_mandatory, disable_warning, max_retry_count, is_disabled, updated_at, is_hidden, "order", title, only_image_verification_status_lookup_required, document_category, created_at, is_default_verified_on_manual_verification, is_default_enabled_on_manual_verification, is_image_validation_required, check_extraction, dependency_document_type, mark_image_valid_on_validation_skip, roles_allowed_to_upload_document_text, description)
VALUES
    ('AadhaarCard', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'FLEET_OWNER', '840327a8-f17c-4d7c-8199-a583cfaadc5f', false, NULL, NULL, NULL, NULL, false, true, '', 4, false, '2025-04-07T07:40:53.81892Z', false, 1, 'Aadhaar Card', NULL, NULL, '2025-04-07T07:40:53.81892Z', NULL, false, true, false, '{}', true, NULL, NULL),
    ('PanCard', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'FLEET_OWNER', '840327a8-f17c-4d7c-8199-a583cfaadc5f', false, NULL, NULL, NULL, NULL, false, true, '', 4, false, '2025-04-07T07:40:53.81892Z', false, 2, 'Pan Card', NULL, NULL, '2025-04-07T07:40:53.81892Z', NULL, false, true, false, '{}', true, NULL, NULL),
    ('AadhaarCard', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'FLEET_BUSINESS', '840327a8-f17c-4d7c-8199-a583cfaadc5f', false, NULL, NULL, NULL, NULL, false, true, '', 4, false, '2025-04-07T07:40:53.81892Z', false, 1, 'Aadhaar Card', NULL, NULL, '2025-04-07T07:40:53.81892Z', NULL, false, true, false, '{}', true, NULL, NULL),
    ('PanCard', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'FLEET_BUSINESS', '840327a8-f17c-4d7c-8199-a583cfaadc5f', false, NULL, NULL, NULL, NULL, false, true, '', 4, false, '2025-04-07T07:40:53.81892Z', false, 2, 'Business Pan Card', NULL, NULL, '2025-04-07T07:40:53.81892Z', NULL, false, true, false, '{}', true, NULL, NULL),
    ('GSTCertificate', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'FLEET_BUSINESS', '840327a8-f17c-4d7c-8199-a583cfaadc5f', false, NULL, NULL, NULL, NULL, false, true, '', 4, false, '2025-04-07T07:40:53.81892Z', false, 4, 'GST Certificate', NULL, NULL, '2025-04-07T07:40:53.81892Z', NULL, false, true, false, '{}', true, NULL, NULL),
    ('OperatorPartnerCode', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'FLEET_BUSINESS', '840327a8-f17c-4d7c-8199-a583cfaadc5f', false, NULL, NULL, true, NULL, false, false, NULL, 4, false, '2026-06-30T07:54:54.768944Z', false, 99, 'Operator Partner Code', NULL, NULL, '2026-06-16T07:17:18.803642Z', NULL, false, true, false, '{}', true, '{OPERATOR,FLEET_OWNER,FLEET_BUSINESS,DRIVER}', NULL),
    ('UDYAMCertificate', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'FLEET_BUSINESS', '840327a8-f17c-4d7c-8199-a583cfaadc5f', false, NULL, NULL, NULL, NULL, false, false, '', 4, false, '2026-05-04T07:55:46.579119Z', false, 5, 'Udyam Certificate', NULL, NULL, '2026-05-04T07:55:46.579119Z', NULL, false, false, false, '{}', false, NULL, NULL),
    ('TANCertificate', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'FLEET_BUSINESS', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, NULL, NULL, false, false, '', 4, false, '2026-05-04T07:55:46.584481Z', false, 6, 'TAN Certificate', NULL, NULL, '2026-05-04T07:55:46.584481Z', NULL, false, false, false, '{}', false, NULL, NULL),
    ('LDCCertificate', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'FLEET_BUSINESS', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, NULL, NULL, false, false, '', 4, false, '2026-05-04T07:55:46.589721Z', false, 7, 'LDC Certificate', NULL, NULL, '2026-05-04T07:55:46.589721Z', NULL, false, false, false, '{}', false, NULL, NULL),
    ('UDYAMCertificate', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'FLEET_OWNER', '840327a8-f17c-4d7c-8199-a583cfaadc5f', false, NULL, NULL, NULL, NULL, false, false, '', 4, true, '2026-05-04T07:55:46.582011Z', true, 5, 'Udyam Certificate', NULL, NULL, '2026-05-04T07:55:46.582011Z', NULL, false, false, false, '{}', false, NULL, NULL),
    ('TANCertificate', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'FLEET_OWNER', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, NULL, NULL, false, false, '', 4, true, '2026-05-04T07:55:46.586902Z', true, 6, 'TAN Certificate', NULL, NULL, '2026-05-04T07:55:46.586902Z', NULL, false, false, false, '{}', false, NULL, NULL),
    ('LDCCertificate', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'FLEET_OWNER', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, NULL, NULL, false, false, '', 4, true, '2026-05-04T07:55:46.592345Z', true, 7, 'LDC Certificate', NULL, NULL, '2026-05-04T07:55:46.592345Z', NULL, false, false, false, '{}', false, NULL, NULL),
    ('FleetRegistration', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'FLEET_BUSINESS', '840327a8-f17c-4d7c-8199-a583cfaadc5f', false, NULL, NULL, NULL, NULL, false, true, NULL, 4, false, '2026-06-24T11:13:57.769629Z', true, 100, 'Fleet Registration', NULL, NULL, '2026-06-24T11:13:57.769629Z', NULL, false, true, false, '{}', true, NULL, NULL),
    ('FleetRegistration', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'FLEET_OWNER', '840327a8-f17c-4d7c-8199-a583cfaadc5f', false, NULL, NULL, NULL, NULL, false, true, NULL, 4, false, '2026-06-24T11:13:57.769629Z', true, 100, 'Fleet Registration', NULL, NULL, '2026-06-24T11:13:57.769629Z', NULL, false, true, false, '{}', true, NULL, NULL),
    ('BusinessLicense', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'FLEET_BUSINESS', '840327a8-f17c-4d7c-8199-a583cfaadc5f', false, NULL, NULL, NULL, NULL, false, false, '', 4, false, '2025-04-07T07:40:53.81892Z', false, 3, 'Business License', NULL, NULL, '2025-04-07T07:40:53.81892Z', NULL, false, true, false, '{}', true, NULL, NULL),
    ('LocalResidenceProof', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'FLEET_OWNER', '840327a8-f17c-4d7c-8199-a583cfaadc5f', false, NULL, NULL, true, NULL, false, true, NULL, 4, false, '2026-07-09T17:04:34.73424Z', false, 17, 'Local Residence Proof', NULL, 'Driver', '2026-07-09T17:04:34.73424Z', NULL, false, true, false, '{}', true, NULL, NULL),
    ('LocalResidenceProof', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'FLEET_BUSINESS', '840327a8-f17c-4d7c-8199-a583cfaadc5f', false, NULL, NULL, true, NULL, false, true, NULL, 4, false, '2026-07-09T17:04:34.73424Z', false, 17, 'Local Residence Proof', NULL, 'Driver', '2026-07-09T17:04:34.73424Z', NULL, false, true, false, '{}', true, NULL, NULL),
    ('BotApproval', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'FLEET_BUSINESS', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, true, NULL, false, false, NULL, 4, false, '2026-06-22T00:00:00Z', true, 100, 'BOT Approval', NULL, NULL, '2026-06-22T00:00:00Z', NULL, false, false, false, '{AadhaarCard,PanCard,OperatorPartnerCode}', true, '{OPERATOR,FLEET_OWNER,FLEET_BUSINESS,DRIVER}', 'BOT approval gate for the fleet business'),
    ('BotApproval', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'FLEET_OWNER', '840327a8-f17c-4d7c-8199-a583cfaadc5f', true, NULL, NULL, true, NULL, false, false, NULL, 4, false, '2026-06-22T00:00:00Z', true, 100, 'BOT Approval', NULL, NULL, '2026-06-22T00:00:00Z', NULL, false, false, false, '{AadhaarCard,PanCard,OperatorPartnerCode}', true, '{OPERATOR,FLEET_OWNER,FLEET_BUSINESS,DRIVER}', 'BOT approval gate for the fleet owner'),
    ('BankingDetails', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'FLEET_BUSINESS', '840327a8-f17c-4d7c-8199-a583cfaadc5f', false, NULL, NULL, false, NULL, false, false, NULL, 4, false, '2026-06-28T14:54:07.036392Z', false, 50, 'Banking Details', NULL, NULL, '2026-06-26T12:06:02.929347Z', NULL, false, true, false, '{}', true, '{OPERATOR,FLEET_OWNER,FLEET_BUSINESS,DRIVER}', NULL),
    ('BankingDetails', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'FLEET_OWNER', '840327a8-f17c-4d7c-8199-a583cfaadc5f', false, NULL, NULL, false, NULL, false, false, NULL, 4, false, '2026-06-28T14:54:07.036392Z', false, 50, 'Banking Details', NULL, NULL, '2026-06-26T12:06:02.929347Z', NULL, false, true, false, '{}', true, '{OPERATOR,FLEET_OWNER,FLEET_BUSINESS,DRIVER}', NULL),
    ('OperatorPartnerCode', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'FLEET_OWNER', '840327a8-f17c-4d7c-8199-a583cfaadc5f', false, NULL, NULL, true, NULL, false, false, NULL, 4, false, '2026-06-30T07:54:54.768944Z', false, 99, 'Operator Partner Code', NULL, NULL, '2026-06-16T07:17:18.803642Z', NULL, false, true, false, '{}', true, '{OPERATOR,FLEET_OWNER,FLEET_BUSINESS,DRIVER}', NULL)
ON CONFLICT (document_type, merchant_operating_city_id, role) DO UPDATE SET
    merchant_id = EXCLUDED.merchant_id,
    do_strict_verifcation = EXCLUDED.do_strict_verifcation,
    document_fields_json = EXCLUDED.document_fields_json,
    document_onboarding_stage = EXCLUDED.document_onboarding_stage,
    is_mandatory_for_enabling = EXCLUDED.is_mandatory_for_enabling,
    is_approval_supported = EXCLUDED.is_approval_supported,
    check_expiry = EXCLUDED.check_expiry,
    is_mandatory = EXCLUDED.is_mandatory,
    disable_warning = EXCLUDED.disable_warning,
    max_retry_count = EXCLUDED.max_retry_count,
    is_disabled = EXCLUDED.is_disabled,
    updated_at = EXCLUDED.updated_at,
    is_hidden = EXCLUDED.is_hidden,
    "order" = EXCLUDED."order",
    title = EXCLUDED.title,
    only_image_verification_status_lookup_required = EXCLUDED.only_image_verification_status_lookup_required,
    document_category = EXCLUDED.document_category,
    created_at = EXCLUDED.created_at,
    is_default_verified_on_manual_verification = EXCLUDED.is_default_verified_on_manual_verification,
    is_default_enabled_on_manual_verification = EXCLUDED.is_default_enabled_on_manual_verification,
    is_image_validation_required = EXCLUDED.is_image_validation_required,
    check_extraction = EXCLUDED.check_extraction,
    dependency_document_type = EXCLUDED.dependency_document_type,
    mark_image_valid_on_validation_skip = EXCLUDED.mark_image_valid_on_validation_skip,
    roles_allowed_to_upload_document_text = EXCLUDED.roles_allowed_to_upload_document_text,
    description = EXCLUDED.description;

-- Same full-replace rule on the fleet-owner side, keyed by (document_type, role).
DELETE FROM atlas_driver_offer_bpp.fleet_owner_document_verification_config
WHERE merchant_operating_city_id = 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e'
  AND (document_type, role) NOT IN (
        ('AadhaarCard', 'FLEET_BUSINESS'),
        ('BankingDetails', 'FLEET_BUSINESS'),
        ('BotApproval', 'FLEET_BUSINESS'),
        ('BusinessLicense', 'FLEET_BUSINESS'),
        ('FleetRegistration', 'FLEET_BUSINESS'),
        ('GSTCertificate', 'FLEET_BUSINESS'),
        ('LDCCertificate', 'FLEET_BUSINESS'),
        ('LocalResidenceProof', 'FLEET_BUSINESS'),
        ('OperatorPartnerCode', 'FLEET_BUSINESS'),
        ('PanCard', 'FLEET_BUSINESS'),
        ('TANCertificate', 'FLEET_BUSINESS'),
        ('UDYAMCertificate', 'FLEET_BUSINESS'),
        ('AadhaarCard', 'FLEET_OWNER'),
        ('BankingDetails', 'FLEET_OWNER'),
        ('BotApproval', 'FLEET_OWNER'),
        ('FleetRegistration', 'FLEET_OWNER'),
        ('LDCCertificate', 'FLEET_OWNER'),
        ('LocalResidenceProof', 'FLEET_OWNER'),
        ('OperatorPartnerCode', 'FLEET_OWNER'),
        ('PanCard', 'FLEET_OWNER'),
        ('TANCertificate', 'FLEET_OWNER'),
        ('UDYAMCertificate', 'FLEET_OWNER')
      );

-- ────────────────────────────────────────────────────────────────────────
-- 4. merchant_service_usage_config  (atlas_driver_offer_bpp)
-- ────────────────────────────────────────────────────────────────────────
-- Which external provider backs each verification call. Points the onboarding
-- verifications at the providers the local mock servers implement — Idfy for
-- PAN/GST/RC/DL and the verification priority lists, Gridline for Aadhaar — so
-- register/verify calls land on Backend/dev/mock-servers instead of a real vendor.
--
-- PK = (merchant_operating_city_id). Upsert, so a re-run refreshes the row.
--
-- NOTE: cached in-process by driver-app -> a change here needs a driver-app RESTART
-- (this is what config-sync does after writing it).

INSERT INTO atlas_driver_offer_bpp.merchant_service_usage_config
    (aadhaar_verification_service, auto_complete, created_at, driver_background_verification_service, face_verification_service, get_distances, get_distances_for_cancel_ride, get_estimated_pickup_distances, get_exophone, get_pickup_routes, get_place_details, get_place_name, get_routes, get_trip_routes, initiate_call, issue_ticket_service, merchant_id, merchant_operating_city_id, rectify_distant_points_failure, send_search_request_to_driver, sms_providers_priority_list, snap_to_road, snap_to_road_providers_list, updated_at, verification_providers_priority_list, verification_service, whatsapp_providers_priority_list, retry_bank_account_link, get_bank_account, create_bank_account, background_verification, sdk_verification_service, get_distances_for_scheduled_rides, llm_chat_completion, pan_verification_service, gst_verification_service, dashboard_pan_verification_service, dashboard_gst_verification_service, toto_verification_priority_list, udyam_verification_service, dashboard_udyam_verification_service, category_based_verification_priority_list, payout_order_status, create_payout_order, challan_providers_priority_list, face_match_service, image_extraction_providers_priority_list)
VALUES
    ('Gridline', 'Google', '2025-03-05T14:04:46.132469Z', 'SafetyPortal', 'InternalScripts', 'Google', 'Google', 'OSRM', 'Exotel', 'OSRM', 'Google', 'Google', 'Google', 'OSRM', 'TataClickToCall', 'Kapture', '840327a8-f17c-4d7c-8199-a583cfaadc5f', 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', 'Google', '{FCM,GRPC}', '{KaleyraSms,ExotelSms}', 'Google', '{OSRM,Google}', '2026-06-01T11:34:20.94452Z', '{Idfy,HyperVergeRCDL}', 'Idfy', '{TataCommunications}', 'Stripe', 'Stripe', 'Stripe', 'Checkr', 'HyperVerge', 'OSRM', 'AzureOpenAI', 'Idfy', 'Idfy', 'Idfy', 'Idfy', '{Tten}', NULL, NULL, '{"BUS": ["Idfy", "HyperVergeRCDL"], "CAR": ["Idfy", "HyperVergeRCDL"], "BOAT": ["Idfy", "HyperVergeRCDL"], "TOTO": ["Idfy"], "TRAIN": ["Idfy", "HyperVergeRCDL"], "TRUCK": ["Idfy", "HyperVergeRCDL"], "FLIGHT": ["Idfy", "HyperVergeRCDL"], "AMBULANCE": ["Idfy", "HyperVergeRCDL"], "TOTO_UDIN": ["Tten"], "MOTORCYCLE": ["Idfy", "HyperVergeRCDL"], "AUTO_CATEGORY": ["Idfy", "HyperVergeRCDL"]}'::jsonb, 'Juspay', 'Juspay', '{Signzy}', 'Idfy', NULL)
ON CONFLICT (merchant_operating_city_id) DO UPDATE SET
    aadhaar_verification_service = EXCLUDED.aadhaar_verification_service,
    auto_complete = EXCLUDED.auto_complete,
    driver_background_verification_service = EXCLUDED.driver_background_verification_service,
    face_verification_service = EXCLUDED.face_verification_service,
    get_distances = EXCLUDED.get_distances,
    get_distances_for_cancel_ride = EXCLUDED.get_distances_for_cancel_ride,
    get_estimated_pickup_distances = EXCLUDED.get_estimated_pickup_distances,
    get_exophone = EXCLUDED.get_exophone,
    get_pickup_routes = EXCLUDED.get_pickup_routes,
    get_place_details = EXCLUDED.get_place_details,
    get_place_name = EXCLUDED.get_place_name,
    get_routes = EXCLUDED.get_routes,
    get_trip_routes = EXCLUDED.get_trip_routes,
    initiate_call = EXCLUDED.initiate_call,
    issue_ticket_service = EXCLUDED.issue_ticket_service,
    merchant_id = EXCLUDED.merchant_id,
    rectify_distant_points_failure = EXCLUDED.rectify_distant_points_failure,
    send_search_request_to_driver = EXCLUDED.send_search_request_to_driver,
    sms_providers_priority_list = EXCLUDED.sms_providers_priority_list,
    snap_to_road = EXCLUDED.snap_to_road,
    snap_to_road_providers_list = EXCLUDED.snap_to_road_providers_list,
    updated_at = EXCLUDED.updated_at,
    verification_providers_priority_list = EXCLUDED.verification_providers_priority_list,
    verification_service = EXCLUDED.verification_service,
    whatsapp_providers_priority_list = EXCLUDED.whatsapp_providers_priority_list,
    retry_bank_account_link = EXCLUDED.retry_bank_account_link,
    get_bank_account = EXCLUDED.get_bank_account,
    create_bank_account = EXCLUDED.create_bank_account,
    background_verification = EXCLUDED.background_verification,
    sdk_verification_service = EXCLUDED.sdk_verification_service,
    get_distances_for_scheduled_rides = EXCLUDED.get_distances_for_scheduled_rides,
    llm_chat_completion = EXCLUDED.llm_chat_completion,
    pan_verification_service = EXCLUDED.pan_verification_service,
    gst_verification_service = EXCLUDED.gst_verification_service,
    dashboard_pan_verification_service = EXCLUDED.dashboard_pan_verification_service,
    dashboard_gst_verification_service = EXCLUDED.dashboard_gst_verification_service,
    toto_verification_priority_list = EXCLUDED.toto_verification_priority_list,
    udyam_verification_service = EXCLUDED.udyam_verification_service,
    dashboard_udyam_verification_service = EXCLUDED.dashboard_udyam_verification_service,
    category_based_verification_priority_list = EXCLUDED.category_based_verification_priority_list,
    payout_order_status = EXCLUDED.payout_order_status,
    create_payout_order = EXCLUDED.create_payout_order,
    challan_providers_priority_list = EXCLUDED.challan_providers_priority_list,
    face_match_service = EXCLUDED.face_match_service,
    image_extraction_providers_priority_list = EXCLUDED.image_extraction_providers_priority_list;


-- ────────────────────────────────────────────────────────────────────────
-- 5. rc_validation_rules  (atlas_driver_offer_bpp)
-- ────────────────────────────────────────────────────────────────────────
-- Drop the OEM allow-list so the mock RC webhook's hardcoded manufacturer
-- (gridline mock returns "TOYOTA"; seed data only allowed "MARUTI") no longer
-- fails validateRCResponse's InvalidOEM check and blocks RC verificationStatus
-- from reaching VALID.

UPDATE atlas_driver_offer_bpp.rc_validation_rules
SET vehicle_oem = NULL
WHERE merchant_operating_city_id = 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e';

-- ────────────────────────────────────────────────────────────────────────
-- 6. merchant  (atlas_bpp_dashboard)
-- ────────────────────────────────────────────────────────────────────────
-- Auto-verify fleet owners on login instead of requiring a separate admin approval.
--
-- createFleetOwnerDashboardOnly sets the dashboard person.verified to
--   not requireAdminApprovalForFleetOnboarding && verifyFleetWhileLogin == Just True
-- and both columns default to false, so a locally onboarded fleet owner starts unverified.
-- checkFleetOwnerVerification then rejects fleet addVehicle with "Fleet owner is not verified",
-- which blocks the whole fleet-vehicle onboarding path locally.
--
-- NOTE: read per request by the dashboard, but if a fleet was created BEFORE this ran its
-- person.verified is already false — the admin POST /account/verifyAccount still fixes those.
UPDATE atlas_bpp_dashboard.merchant
SET verify_fleet_while_login = true,
    require_admin_approval_for_fleet_onboarding = false
WHERE short_id = 'MSIL_PARTNER';

-- ────────────────────────────────────────────────────────────────────────
-- 7. BOT role + access_matrix  (atlas_bpp_dashboard)
-- ────────────────────────────────────────────────────────────────────────
-- BOT dashboard user reviews onboarding details submitted via the BOT flow
-- (see enable_bot_flow above); needs read/write access to the fleet-driver
-- onboarding + operator-hub-review DSL endpoints it drives.

INSERT INTO atlas_bpp_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at, accessible_roles)
VALUES ('5275f034-33bc-490f-af96-61023660a805', 'BOT', 'DASHBOARD_USER', 'Bot will be used for reviewing onboarding details', '2026-06-11T07:09:40.843261Z', '2026-06-11T07:09:40.843261Z', '{}')
ON CONFLICT (id) DO NOTHING;

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at) VALUES
('aaab39e2-98c6-40c2-847c-3824d46341f7', '5275f034-33bc-490f-af96-61023660a805', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_OPERATOR_INFO', '2026-06-16T10:45:40.741764Z', '2026-06-16T10:45:40.741764Z'),
('28262817-dcf5-403b-a8bf-eafd9b330f62', '5275f034-33bc-490f-af96-61023660a805', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_VEHICLE_ASSOCIATION', '2026-07-01T09:09:14.915484Z', '2026-07-01T09:09:14.915484Z'),
('5ca9187c-e806-4acc-8692-1235f2113bdf', '5275f034-33bc-490f-af96-61023660a805', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_VEHICLE_INFO', '2026-06-17T07:11:20.602196Z', '2026-06-17T07:11:20.602196Z'),
('116aa9b3-246b-44b8-8350-fb6bae4cf418', '5275f034-33bc-490f-af96-61023660a805', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/ONBOARDING/GET_ONBOARDING_DOCUMENT_CONFIGS', '2026-06-16T10:17:07.153817Z', '2026-06-16T10:17:07.153817Z'),
('2440f4a9-08e0-4645-b5c1-af51e8e2d82d', '5275f034-33bc-490f-af96-61023660a805', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/ONBOARDING/GET_ONBOARDING_REGISTER_STATUS', '2026-06-16T10:17:28.43492Z', '2026-06-16T10:17:28.43492Z'),
('2811b84e-876c-4420-a779-19967ec8038b', '5275f034-33bc-490f-af96-61023660a805', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/ONBOARDING/GET_ONBOARDING_REGISTER_VEHICLE_STATUS', '2026-07-07T09:19:41.292718Z', '2026-07-07T09:19:41.292718Z'),
('ac0bdc12-d662-410f-ba16-d17844212778', '5275f034-33bc-490f-af96-61023660a805', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/ONBOARDING/POST_ONBOARDING_VERIFY', '2026-06-16T10:18:08.422192Z', '2026-06-16T10:18:08.422192Z'),
('a55cc3b7-21c7-48e4-b024-1f2ff101b25b', '5275f034-33bc-490f-af96-61023660a805', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_DOCUMENTS_INFO', '2026-06-16T10:14:17.912919Z', '2026-06-16T10:14:17.912919Z'),
('3aa289d5-29b4-4b04-8274-773523fa2f93', '5275f034-33bc-490f-af96-61023660a805', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_IDENTITY_INFO', '2026-06-19T14:05:57.416871Z', '2026-06-19T14:05:57.416871Z'),
('ba511c04-71e7-4886-8808-0105616b71c3', '5275f034-33bc-490f-af96-61023660a805', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_PAN_AADHAR_SELFIE_DETAILS', '2026-06-17T07:58:16.417571Z', '2026-06-17T07:58:16.417571Z'),
('be20d81b-4c86-4e58-a4b4-25edba8376d7', '5275f034-33bc-490f-af96-61023660a805', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_PAN_AADHAR_SELFIE_DETAILS_LIST', '2026-06-17T07:59:22.696977Z', '2026-06-17T07:59:22.696977Z'),
('8c5dcad4-e16c-4dc0-b418-3474a57f7b02', '5275f034-33bc-490f-af96-61023660a805', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_ASSOCIATION_CHANGE', '2026-07-02T08:01:19.598688Z', '2026-07-02T08:01:19.598688Z'),
('e5970845-9ab9-4892-b349-0d2dc43e9301', '5275f034-33bc-490f-af96-61023660a805', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/GET_DRIVER_REGISTRATION_DOCUMENTS_LIST', '2026-06-16T10:30:44.238761Z', '2026-06-16T10:30:44.238761Z'),
('91236a0d-a319-4e6a-90ea-64516855bdd9', '5275f034-33bc-490f-af96-61023660a805', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/GET_DRIVER_REGISTRATION_GET_DOCUMENT', '2026-06-16T10:31:19.208757Z', '2026-06-16T10:31:19.208757Z'),
('1ebad090-34d1-4e1e-8f9b-93168f2b1e89', '5275f034-33bc-490f-af96-61023660a805', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_DOCUMENT_UPLOAD', '2026-06-16T10:31:41.717199Z', '2026-06-16T10:31:41.717199Z'),
('6763e232-d958-4ff1-bef8-d3233cfa5a84', '5275f034-33bc-490f-af96-61023660a805', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MEDIA_FILE_DOCUMENT/GET_MEDIA_FILE_DOCUMENT_DOWNLOAD_LINK', '2026-06-24T14:08:02.904723Z', '2026-06-24T14:08:02.904723Z'),
('bfb76200-9c23-4638-8bb7-5ac6ac129987', '5275f034-33bc-490f-af96-61023660a805', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_OPERATOR/DRIVER/GET_DRIVER_OPERATOR_FETCH_HUB_REQUESTS', '2026-06-17T11:03:09.708489Z', '2026-06-17T11:03:09.708489Z'),
('d8d6073a-5826-43ec-b9f1-3afc66112a64', '5275f034-33bc-490f-af96-61023660a805', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_OPERATOR/DRIVER/GET_DRIVER_REQUEST_REVIEW_HISTORY', '2026-06-15T10:17:52.144751Z', '2026-06-15T10:17:52.144751Z'),
('fbfbf765-9ed1-45ac-ba13-f27dc9c13858', '5275f034-33bc-490f-af96-61023660a805', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_OPERATOR/DRIVER/GET_DRIVER_REVIEW_QUEUE_REQUEST', '2026-06-15T10:16:39.269071Z', '2026-06-15T10:16:39.269071Z'),
('41ec5e5f-a09a-4f4e-98a0-5d0586f97996', '5275f034-33bc-490f-af96-61023660a805', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_OPERATOR/DRIVER/POST_DRIVER_SUBMIT_REVIEW_REQUEST', '2026-06-15T10:17:12.139404Z', '2026-06-15T10:17:12.139404Z'),
('7de283ca-6bdd-48bf-bebf-f13710d1c44c', '5275f034-33bc-490f-af96-61023660a805', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/DRIVER/GET_DRIVER_INFO', '2026-06-17T07:38:09.661279Z', '2026-06-17T07:38:09.661279Z')
ON CONFLICT (role_id, api_entity, user_action_type) DO NOTHING;

-- ────────────────────────────────────────────────────────────────────────
-- 8. BOT person + registration_token + merchant_access  (atlas_bpp_dashboard)
-- ────────────────────────────────────────────────────────────────────────
-- Equivalent of hitting /admin/person/create (roleId = BOT role above) then
-- /admin/person/:id/assignMerchantCityAccess for MSIL_PARTNER/Delhi.
-- email_hash/mobile_number_hash/password_hash = SHA256(utf8(encHashSalt) <> utf8(value))
-- (Kernel/External/Encryption.hs), salt = Backend/dhall-configs/dev/secrets/provider-dashboard.dhall.
-- email_encrypted/mobile_number_encrypted came from a real local passetto (:8021) /encrypt call.
-- unencrypted: email: msil.bot@gmail.com, mobile: 9999999918, password: 9999999918

INSERT INTO atlas_bpp_dashboard.person (id, first_name, last_name, role_id, dashboard_access_type, email_encrypted, email_hash, mobile_number_encrypted, mobile_number_hash, mobile_country_code, password_hash, created_at, updated_at) VALUES
	('38201a3d-47c8-45e2-a3c2-23caebf8b604', 'MSIL', 'BOT', '5275f034-33bc-490f-af96-61023660a805', 'DASHBOARD_USER', '0.1.0|2|efhUz4BozCSAh5hXLt4F5fStDJgzJIoc8Jx9EyExnjoef3ddgOuJzzNLDsqdmu0FuTr0KPfsPnFGGBia19POYk3w7YmJ', '\xffadb8089f7159cd2731f45def2e8dbacb64184569767829429351953d31395a', '0.1.0|0|eMpCzukXQBcmihSilSAAGazHu2bUrdnVlPxRf+Lj41khzHRYFBzwPjW/N7XzxN03ARvFHWXhdBU+fYHO2w==', '\x6dbb43a2af42146d644d0dfcf940ec2efbef248ad609a9b3dd56056c82dfc6e9', '+91', '\x6dbb43a2af42146d644d0dfcf940ec2efbef248ad609a9b3dd56056c82dfc6e9', now(), now())
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.registration_token (id, token, person_id, merchant_id, operating_city, enabled, created_at)
SELECT
    'local-bot-token-delhi-id-00000000000',
    'local-bot-token-delhi-msil-partner',
    '38201a3d-47c8-45e2-a3c2-23caebf8b604',
    m.id,
    'Delhi',
    true,
    now()
FROM atlas_bpp_dashboard.merchant m
WHERE m.short_id = 'MSIL_PARTNER'
ON CONFLICT DO NOTHING;

-- id is gen_random_uuid() per row, so ON CONFLICT DO NOTHING needs the real
-- unique_person_id_merchant_id_operating_city constraint as an explicit target
-- (matching the NOT EXISTS-guard style used elsewhere in this file's sibling seed).
INSERT INTO atlas_bpp_dashboard.merchant_access (id, person_id, merchant_id, merchant_short_id, operating_city, created_at)
SELECT gen_random_uuid()::text, '38201a3d-47c8-45e2-a3c2-23caebf8b604', m.id, m.short_id, 'Delhi', now()
FROM atlas_bpp_dashboard.merchant m
WHERE m.short_id = 'MSIL_PARTNER'
ON CONFLICT (person_id, merchant_id, operating_city) DO NOTHING;

-- ────────────────────────────────────────────────────────────────────────
-- 9. kv_configs shard rotation  (atlas_driver_offer_bpp)
-- ────────────────────────────────────────────────────────────────────────
-- Cut KV -> Postgres drain latency ~30x by making the drainer scan 4 shards instead
-- of 128. The BOT flow writes operation_hub_requests / review_request / driver_gstin
-- through KV and the tests read them straight back, so the shard rotation period is a
-- hard floor on how long every poll in the collections has to wait.
--
-- BOTH statements are required — either one alone is a no-op:
--   drainer  (dynamic-offer-driver-drainer/src/Utils/Utils.hs:148)
--     getNumOfStreams = max defaultShardMod
--                           (fromMaybe numberOfStreamsForKV (getMaxValue tableShardModRange))
--     numberOfStreamsForKV is a hardcoded 128, so with tableShardModRange = {} the
--     fallback wins and defaultShardMod can only ever RAISE the shard count.
--   app      (mobility-core Kernel/Beam/Functions.hs:183)
--     tableShardModRange' = HM.lookupDefault (0, defaultShardMod) modelName tableShardModRange
--     so defaultShardMod is what decides where the app WRITES.
-- Setting only the range would leave the drainer at max(128, 4) = 128; setting only
-- defaultShardMod would leave it at max(4, 128) = 128. Together: max(4, 4) = 4, and
-- writer and reader both land on shards 0..3.
--
-- MEASURED ON THIS STACK: drainer loop ~6.07 iterations/sec (one shard each)
--   -> 128-shard rotation ~21.1s, 4-shard rotation ~0.66s.
--
-- The driver_information entry is deliberately (0, 4) — identical to what that table
-- already gets from the defaultShardMod fallback — so it exists only to pull
-- getMaxValue down to 4 without changing any table's write placement.
--
-- LOCAL ONLY. No revert file: this seed is re-applied on every stack startup, and both
-- statements are idempotent. To undo, restore defaultShardMod to 128 and
-- tableShardModRange to {} (widening is always safe — the rotation only grows, so no
-- pending entry is ever orphaned).
--
-- Narrowing 128 -> 4 CAN orphan entries already sitting in shards 4..127, which is why
-- this runs at startup (nothing pending on a cold stack). If you apply it by hand mid-run,
-- drain first and confirm every stream is empty:
--   for i in $(seq 0 127); do redis-cli -p 30001 -c xlen "driver-db-sync-stream{shard-$i}"; done
-- The drainer re-reads kv_configs within kvConfigUpdateFrequency (60s, driver-drainer.dhall),
-- so no drainer restart is needed; the app re-reads it on its own refresh.

UPDATE atlas_driver_offer_bpp.system_configs
SET config_value = jsonb_set(
      jsonb_set(config_value::jsonb, '{defaultShardMod}', '4'::jsonb),
      '{tableShardModRange}',
      '{"driver_information": [0, 4]}'::jsonb
    )::text
WHERE id = 'kv_configs';

-- COMMIT;
