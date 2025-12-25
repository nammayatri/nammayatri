CREATE INDEX idx_workflow_txn_id ON atlas_driver_offer_bpp.image USING btree (workflow_transaction_id);

INSERT INTO atlas_driver_offer_bpp.document_verification_config (
    check_expiry,
    check_extraction,
    dependency_document_type,
    description,
    disable_warning,
    document_type,
    is_disabled,
    is_hidden,
    is_mandatory,
    max_retry_count,
    merchant_id,
    merchant_operating_city_id,
    rc_number_prefix_list,
    supported_vehicle_classes_json,
    title,
    vehicle_category,
    vehicle_class_check_type,
    created_at,
    updated_at,
    is_default_enabled_on_manual_verification,
    is_image_validation_required,
    do_strict_verifcation
)
SELECT
    false,
    false,
    '{}',
    'Profile Photo',
    null,
    'ProfilePhoto',
    false,
    false,
    true,
    4,
    merchant_id,
    id,
    '{}',
    '{}',
    'Profile Photo',
    'CAR',
    'Infix',
    CURRENT_TIMESTAMP as created_at,
    CURRENT_TIMESTAMP as updated_at,
    false,
    false,
    false
FROM
    atlas_driver_offer_bpp.merchant_operating_city
WHERE
    merchant_short_id = 'NAMMA_YATRI_PARTNER' AND city = 'Bangalore';


INSERT INTO atlas_driver_offer_bpp.document_verification_config (
    check_expiry,
    check_extraction,
    dependency_document_type,
    description,
    disable_warning,
    document_type,
    is_disabled,
    is_hidden,
    is_mandatory,
    max_retry_count,
    merchant_id,
    merchant_operating_city_id,
    rc_number_prefix_list,
    supported_vehicle_classes_json,
    title,
    vehicle_category,
    vehicle_class_check_type,
    created_at,
    updated_at,
    is_default_enabled_on_manual_verification,
    is_image_validation_required,
    do_strict_verifcation
)
SELECT
    false,
    false,
    '{ProfilePhoto}',
    'Aadhaar Card',
    null,
    'AadhaarCard',
    false,
    false,
    true,
    4,
    merchant_id,
    id,
    '{}',
    '{}',
    'Aadhaar Card',
    'CAR',
    'Infix',
    CURRENT_TIMESTAMP as created_at,
    CURRENT_TIMESTAMP as updated_at,
    false,
    false,
    false
FROM
    atlas_driver_offer_bpp.merchant_operating_city
WHERE
   merchant_short_id = 'NAMMA_YATRI_PARTNER' AND city = 'Bangalore';


INSERT INTO atlas_driver_offer_bpp.document_verification_config (
    check_expiry,
    check_extraction,
    dependency_document_type,
    description,
    disable_warning,
    document_type,
    is_disabled,
    is_hidden,
    is_mandatory,
    max_retry_count,
    merchant_id,
    merchant_operating_city_id,
    rc_number_prefix_list,
    supported_vehicle_classes_json,
    title,
    vehicle_category,
    vehicle_class_check_type,
    created_at,
    updated_at,
    is_default_enabled_on_manual_verification,
    is_image_validation_required,
    do_strict_verifcation
)
SELECT
    false,
    false,
    '{ProfilePhoto}',
    'Pan Card',
    null,
    'PanCard',
    false,
    false,
    true,
    4,
    merchant_id,
    id,
    '{}',
    '{}',
    'Pan Card',
    'CAR',
    'Infix',
    CURRENT_TIMESTAMP as created_at,
    CURRENT_TIMESTAMP as updated_at,
    false,
    false,
    false
FROM
    atlas_driver_offer_bpp.merchant_operating_city
WHERE
    merchant_short_id = 'NAMMA_YATRI_PARTNER' AND city = 'Bangalore';


--------------------------------- For Local Only -----------------------

UPDATE atlas_driver_offer_bpp.document_verification_config SET dependency_document_type = '{ProfilePhoto}', is_mandatory = true WHERE document_type = 'AadhaarCard' AND vehicle_category = 'CAR' AND merchant_operating_city_id like '%1e7b7%';
UPDATE atlas_driver_offer_bpp.document_verification_config SET dependency_document_type = '{ProfilePhoto}', is_mandatory = true WHERE document_type = 'PanCard' AND vehicle_category = 'CAR' AND merchant_operating_city_id like '%1e7b7%';