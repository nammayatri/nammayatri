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
        WHEN document_type = 'AadhaarCard' THEN true
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

-- SUBSCRIPTION PLAN FOR BIKE SPECIFIC IN YS WITH MANDATORY
insert into atlas_driver_offer_bpp.document_verification_config (check_expiry, check_extraction, dependency_document_type, description, disable_warning, document_type, is_disabled, is_hidden, is_mandatory, max_retry_count, merchant_id, merchant_operating_city_id, rc_number_prefix_list, supported_vehicle_classes_json, title, vehicle_category, vehicle_class_check_type, created_at, updated_at, is_default_enabled_on_manual_verification, is_image_validation_required)
    values (false, false, '{VehicleRegistrationCertificate}', null, null, 'SubscriptionPlan', false, false, true, 3, '96dd7f78-787e-4a0b-8675-e9e6fe93bb8f', '7a0a3891-5945-4f86-a2f2-90c89c197c56', '{WB,JH,BR,OR}', '[]', 'Yatri Sathi Subscription Plan', 'MOTORCYCLE', 'Infix', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, true, true);

-- RUN THIS ONLY AFTER CHANGE IN plan.sql FILE: ADDITION OF NULLABLE vehicle_variant COLUMN
INSERT INTO atlas_driver_offer_bpp.plan
	(id, merchant_id, payment_mode, plan_type, name, description, max_amount, registration_amount, plan_base_amount, is_offer_applicable, max_credit_limit, free_ride_count, frequency, cgst_percentage, sgst_percentage, max_mandate_amount, subscribed_flag_toggle_allowed, is_deprecated, eligible_for_coin_discount, merchant_op_city_id, service_name, based_on_entity, vehicle_variant)
	values ( atlas_driver_offer_bpp.uuid_generate_v4(), '96dd7f78-787e-4a0b-8675-e9e6fe93bb8f', 'AUTOPAY', 'SUBSCRIPTION', 'DAILY PER RIDE', 'Unlimited number of rides per day', 5000, 1, 'PERRIDE_2', true, 350, 10, 'DAILY', 0.09, 0.09, 150.0, true, false, true, '7a0a3891-5945-4f86-a2f2-90c89c197c56', 'YATRI_SUBSCRIPTION', 'RIDE', 'BIKE');