INSERT INTO atlas_driver_offer_bpp.fleet_owner_document_verification_config (
    document_type, merchant_id, merchant_operating_city_id, role,
    title, description, is_mandatory, is_mandatory_for_enabling, is_disabled, is_hidden,
    dependency_document_type, check_extraction, check_expiry, document_category,
    is_default_enabled_on_manual_verification, is_image_validation_required, do_strict_verifcation,
    "order", max_retry_count, created_at, updated_at
)
SELECT
    'BankingDetails', tmpl.merchant_id, tmpl.merchant_operating_city_id, tmpl.role,
    'Banking Details', NULL, true, false, false, false,
    '{}', false, false, NULL,
    tmpl.is_default_enabled_on_manual_verification, tmpl.is_image_validation_required, tmpl.do_strict_verifcation,
    50, tmpl.max_retry_count, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
FROM atlas_driver_offer_bpp.fleet_owner_document_verification_config tmpl
WHERE tmpl.document_type = 'AadhaarCard'
  AND tmpl.role IN ('FLEET_OWNER', 'FLEET_BUSINESS')
  AND tmpl.merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER')
ON CONFLICT DO NOTHING;
