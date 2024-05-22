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
    is_image_validation_required
)
SELECT
    check_expiry,
    check_extraction,
    dependency_document_type,
    description,
    disable_warning,
    document_type,
    is_disabled,
    is_hidden,
    CASE
        WHEN document_type = 'AadharCard' THEN true
        ELSE is_mandatory
    END AS is_mandatory,
    max_retry_count,
    merchant_id,
    merchant_operating_city_id,
    '{WB,JH,BR,OR}',
    supported_vehicle_classes_json,
    title,
    'MOTORCYCLE' AS vehicle_category,
    vehicle_class_check_type,
    CURRENT_TIMESTAMP as created_at,
    CURRENT_TIMESTAMP as updated_at,
    is_default_enabled_on_manual_verification,
    is_image_validation_required
FROM
    atlas_driver_offer_bpp.document_verification_config
WHERE
    vehicle_category = 'CAR' AND rc_number_prefix_list = '{WB}';

update atlas_driver_offer_bpp.document_verification_config set supported_vehicle_classes_json =
    json_build_array('MCWG', 'MCG') where document_type = 'DriverLicense' and vehicle_category = 'MOTORCYCLE';

update atlas_driver_offer_bpp.document_verification_config set supported_vehicle_classes_json =
    json_build_array(json_build_object('vehicleClass', '2WN', 'vehicleVariant', 'BIKE')) where document_type = 'VehicleRegistrationCertificate' and vehicle_category = 'MOTORCYCLE';