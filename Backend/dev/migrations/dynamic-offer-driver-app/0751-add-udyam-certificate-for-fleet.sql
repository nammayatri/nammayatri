INSERT INTO atlas_driver_offer_bpp.fleet_owner_document_verification_config
(check_expiry, check_extraction, dependency_document_type, description, disable_warning, document_type, is_disabled, is_hidden, is_mandatory, max_retry_count, merchant_id, merchant_operating_city_id, "order",is_image_validation_required,is_default_enabled_on_manual_verification, do_strict_verifcation , title, created_at, updated_at, role, document_category)
VALUES
(false,
false,
'{}',
null,
'',
'UDYAMCertificate',
false,
false,
false,
4,
(select id from atlas_driver_offer_bpp.merchant where short_id = 'MSIL_PARTNER'),
(select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Delhi' and merchant_short_id = 'MSIL_PARTNER'),
5,
true,
false,
false,
'UDYAM Certificate',
now(),
now(),
'FLEET_BUSINESS',
'') ON CONFLICT DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.fleet_owner_document_verification_config
(check_expiry, check_extraction, dependency_document_type, description, disable_warning, document_type, is_disabled, is_hidden, is_mandatory, max_retry_count, merchant_id, merchant_operating_city_id, "order",is_image_validation_required,is_default_enabled_on_manual_verification, do_strict_verifcation , title, created_at, updated_at, role, document_category)
VALUES
(false,
false,
'{}',
null,
'',
'UDYAMCertificate',
false,
false,
false,
4,
(select id from atlas_driver_offer_bpp.merchant where short_id = 'MSIL_PARTNER'),
(select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Delhi' and merchant_short_id = 'MSIL_PARTNER'),
5,
true,
false,
false,
'UDYAM Certificate',
now(),
now(),
'FLEET_OWNER',
'') ON CONFLICT DO NOTHING;