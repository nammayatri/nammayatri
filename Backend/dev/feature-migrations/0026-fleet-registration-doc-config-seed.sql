------------------------------------------------------------------------------------------------------
-- FleetRegistration onboarding doc config.
--    fleet_owner_document_verification_config (PK = document_type + merchant_operating_city_id + role).
--    Seed for BOTH fleet roles by cloning the AadhaarCard row (same merchant/cities/roles), mirroring the
--    OperatorPartnerCode seed in 0014: clone the verification "infra" flags
--    (is_default_enabled_on_manual_verification, is_image_validation_required, do_strict_verifcation,
--    max_retry_count) from the template; only document_type/title/order and the mandatory flags are set.
--      is_mandatory = true, is_mandatory_for_enabling = NULL (unset) -> same as PAN/Aadhaar, so the
--      ForEnabling check falls back to is_mandatory and this doc gates BOTH `verified` and `enabled`.
--    Validity is computed from FleetOwnerInformation.registeredAt (no image / no external verification;
--    getProcessedDriverDocuments returns VALID iff registeredAt is set), so the cloned image flags are
--    inert for this doc — it stays VALID only after the complete-registration API is called.
------------------------------------------------------------------------------------------------------
INSERT INTO atlas_driver_offer_bpp.fleet_owner_document_verification_config (
    document_type, merchant_id, merchant_operating_city_id, role,
    title, description, is_mandatory, is_mandatory_for_enabling, is_disabled, is_hidden,
    dependency_document_type, check_extraction, check_expiry, document_category,
    is_default_enabled_on_manual_verification, is_image_validation_required, do_strict_verifcation,
    "order", max_retry_count, created_at, updated_at
)
SELECT
    'FleetRegistration', tmpl.merchant_id, tmpl.merchant_operating_city_id, tmpl.role,
    'Fleet Registration', NULL, true, NULL, false, true,
    '{}', false, false, NULL,
    tmpl.is_default_enabled_on_manual_verification, tmpl.is_image_validation_required, tmpl.do_strict_verifcation,
    100, tmpl.max_retry_count, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
FROM atlas_driver_offer_bpp.fleet_owner_document_verification_config tmpl
WHERE tmpl.document_type = 'AadhaarCard'
  AND tmpl.role IN ('FLEET_OWNER', 'FLEET_BUSINESS')
  AND tmpl.merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER')
ON CONFLICT DO NOTHING;
