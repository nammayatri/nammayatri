-- Don't run anything in prod --

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
    "order"
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
    is_mandatory,
    max_retry_count,
    merchant_id,
    (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Delhi' and merchant_short_id = 'MSIL_PARTNER') as merchant_operating_city_id,
    '{DL}',
    supported_vehicle_classes_json,
    title,
    vehicle_category,
    vehicle_class_check_type,
    CURRENT_TIMESTAMP as created_at,
    CURRENT_TIMESTAMP as updated_at,
    is_default_enabled_on_manual_verification,
    is_image_validation_required,
    "order"
FROM
    atlas_driver_offer_bpp.document_verification_config
WHERE
    vehicle_category = 'CAR' AND merchant_operating_city_id = (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Delhi' and merchant_short_id = 'NAMMA_YATRI_PARTNER') AND document_type in ('DriverLicense','AadhaarCard','PanCard','Permissions','VehicleRegistrationCertificate','ProfilePhoto','VehiclePermit','VehicleInsurance','VehiclePUC','VehicleFitnessCertificate') ON CONFLICT DO NOTHING;

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
    "order"
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
    is_mandatory,
    max_retry_count,
    (select id from atlas_driver_offer_bpp.merchant where short_id = 'MSIL_PARTNER') as merchant_id,
   (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Delhi' and merchant_short_id = 'MSIL_PARTNER') as merchant_operating_city_id,
    '{DL}',
    supported_vehicle_classes_json,
    title,
    vehicle_category,
    vehicle_class_check_type,
    CURRENT_TIMESTAMP as created_at,
    CURRENT_TIMESTAMP as updated_at,
    is_default_enabled_on_manual_verification,
    is_image_validation_required,
    "order"
FROM
    atlas_driver_offer_bpp.document_verification_config
WHERE
    vehicle_category = 'CAR' AND merchant_operating_city_id = (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Minneapolis' and merchant_short_id = 'BRIDGE_CABS_PARTNER') AND document_type in ('VehicleInspectionForm') ON CONFLICT DO NOTHING;

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
    "order"
)
SELECT
    check_expiry,
    check_extraction,
    dependency_document_type,
    description,
    disable_warning,
    'VehicleNOC',
    is_disabled,
    is_hidden,
    is_mandatory,
    max_retry_count,
    (select id from atlas_driver_offer_bpp.merchant where short_id = 'MSIL_PARTNER') as merchant_id,
   (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Delhi' and merchant_short_id = 'MSIL_PARTNER') as merchant_operating_city_id,
    '{DL}',
    supported_vehicle_classes_json,
    title,
    vehicle_category,
    vehicle_class_check_type,
    CURRENT_TIMESTAMP as created_at,
    CURRENT_TIMESTAMP as updated_at,
    is_default_enabled_on_manual_verification,
    is_image_validation_required,
    11
FROM
    atlas_driver_offer_bpp.document_verification_config
WHERE
    vehicle_category = 'CAR' AND merchant_operating_city_id = (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Minneapolis' and merchant_short_id = 'BRIDGE_CABS_PARTNER') AND document_type = 'BackgroundVerification' ON CONFLICT DO NOTHING;

UPDATE atlas_driver_offer_bpp.document_verification_config set document_category = 'Driver' where document_type in ('BackgroundVerification','DriverLicense','SubscriptionPlan','ProfilePhoto','AadhaarCard','SocialSecurityNumber','ProfileDetails','PanCard');
UPDATE atlas_driver_offer_bpp.document_verification_config set document_category = 'Vehicle' where document_type in ('VehicleInsurance','VehiclePermit','VehicleRegistrationCertificate','VehicleInspectionForm','VehiclePUC','VehicleFitnessCertificate');
UPDATE atlas_driver_offer_bpp.document_verification_config set document_category = 'Permission' where document_type in ('Permissions');
UPDATE atlas_driver_offer_bpp.document_verification_config set document_category = 'Training' where document_type in ('Training');

UPDATE atlas_driver_offer_bpp.fleet_owner_document_verification_config set role = 'FLEET_OWNER';

-- Fleet Onboarding Documents (MASTER) -- run even if commented --

-- INSERT INTO atlas_driver_offer_bpp.fleet_owner_document_verification_config
-- (check_expiry, check_extraction, dependency_document_type, description, disable_warning, document_type, is_disabled, is_hidden, is_mandatory, max_retry_count, merchant_id, merchant_operating_city_id, "order",is_image_validation_required,is_default_enabled_on_manual_verification, do_strict_verifcation , title, created_at, updated_at, role)
-- VALUES
-- (false,
-- false,
-- '{}',
-- null,
-- '',
-- 'AadhaarCard',
-- false,
-- false,
-- true,
-- 4,
-- (select id from atlas_driver_offer_bpp.merchant where short_id = 'MSIL_PARTNER'),
-- (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Delhi' and merchant_short_id = 'MSIL_PARTNER'),
-- 1,
-- true,
-- false,
-- false,
-- 'Aadhaar Card',
-- now(),
-- now(),
-- 'FLEET_OWNER');

-- INSERT INTO atlas_driver_offer_bpp.fleet_owner_document_verification_config
-- (check_expiry, check_extraction, dependency_document_type, description, disable_warning, document_type, is_disabled, is_hidden, is_mandatory, max_retry_count, merchant_id, merchant_operating_city_id, "order",is_image_validation_required,is_default_enabled_on_manual_verification, do_strict_verifcation , title, created_at, updated_at, role)
-- VALUES
-- (false,
-- false,
-- '{}',
-- null,
-- '',
-- 'PanCard',
-- false,
-- false,
-- true,
-- 4,
-- (select id from atlas_driver_offer_bpp.merchant where short_id = 'MSIL_PARTNER'),
-- (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Delhi' and merchant_short_id = 'MSIL_PARTNER'),
-- 2,
-- true,
-- false,
-- false,
-- 'Pan Card',
-- now(),
-- now(),
-- 'FLEET_OWNER');

-- Fleet Onboarding Documents (LOCAL) --

INSERT INTO atlas_driver_offer_bpp.fleet_owner_document_verification_config
(check_expiry, check_extraction, dependency_document_type, description, disable_warning, document_type, is_disabled, is_hidden, is_mandatory, max_retry_count, merchant_id, merchant_operating_city_id, "order",is_image_validation_required,is_default_enabled_on_manual_verification, do_strict_verifcation , title, created_at, updated_at, role)
VALUES
(false,
false,
'{}',
null,
'',
'AadhaarCard',
false,
false,
true,
4,
(select id from atlas_driver_offer_bpp.merchant where short_id = 'NAMMA_YATRI_PARTNER'),
(select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kochi' and merchant_short_id = 'NAMMA_YATRI_PARTNER'),
1,
true,
false,
false,
'Aadhaar Card',
now(),
now(),
'FLEET_OWNER') ON CONFLICT DO NOTHING;;

INSERT INTO atlas_driver_offer_bpp.fleet_owner_document_verification_config
(check_expiry, check_extraction, dependency_document_type, description, disable_warning, document_type, is_disabled, is_hidden, is_mandatory, max_retry_count, merchant_id, merchant_operating_city_id, "order",is_image_validation_required,is_default_enabled_on_manual_verification, do_strict_verifcation , title, created_at, updated_at, role)
VALUES
(false,
false,
'{}',
null,
'',
'PanCard',
false,
false,
true,
4,
(select id from atlas_driver_offer_bpp.merchant where short_id = 'NAMMA_YATRI_PARTNER'),
(select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kochi' and merchant_short_id = 'NAMMA_YATRI_PARTNER'),
2,
true,
false,
false,
'Pan Card',
now(),
now(),
'FLEET_OWNER') ON CONFLICT DO NOTHING;;

-- Fleet Business Onboarding Documents (MASTER) --

-- INSERT INTO atlas_driver_offer_bpp.fleet_owner_document_verification_config
-- (check_expiry, check_extraction, dependency_document_type, description, disable_warning, document_type, is_disabled, is_hidden, is_mandatory, max_retry_count, merchant_id, merchant_operating_city_id, "order",is_image_validation_required,is_default_enabled_on_manual_verification, do_strict_verifcation , title, created_at, updated_at, role)
-- VALUES
-- (false,
-- false,
-- '{}',
-- null,
-- '',
-- 'AadhaarCard',
-- false,
-- false,
-- true,
-- 4,
-- (select id from atlas_driver_offer_bpp.merchant where short_id = 'MSIL_PARTNER'),
-- (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Delhi' and merchant_short_id = 'MSIL_PARTNER'),
-- 1,
-- true,
-- false,
-- false,
-- 'Aadhaar Card',
-- now(),
-- now(),
-- 'FLEET_BUSINESS');

-- INSERT INTO atlas_driver_offer_bpp.fleet_owner_document_verification_config
-- (check_expiry, check_extraction, dependency_document_type, description, disable_warning, document_type, is_disabled, is_hidden, is_mandatory, max_retry_count, merchant_id, merchant_operating_city_id, "order",is_image_validation_required,is_default_enabled_on_manual_verification, do_strict_verifcation , title, created_at, updated_at, role)
-- VALUES
-- (false,
-- false,
-- '{}',
-- null,
-- '',
-- 'PanCard',
-- false,
-- false,
-- true,
-- 4,
-- (select id from atlas_driver_offer_bpp.merchant where short_id = 'MSIL_PARTNER'),
-- (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Delhi' and merchant_short_id = 'MSIL_PARTNER'),
-- 2,
-- true,
-- false,
-- false,
-- 'Business Pan Card', -- check if different
-- now(),
-- now(),
-- 'FLEET_BUSINESS');

-- INSERT INTO atlas_driver_offer_bpp.fleet_owner_document_verification_config
-- (check_expiry, check_extraction, dependency_document_type, description, disable_warning, document_type, is_disabled, is_hidden, is_mandatory, max_retry_count, merchant_id, merchant_operating_city_id, "order",is_image_validation_required,is_default_enabled_on_manual_verification, do_strict_verifcation , title, created_at, updated_at, role)
-- VALUES
-- (false,
-- false,
-- '{}',
-- null,
-- '',
-- 'BusinessLicense',
-- false,
-- false,
-- true,
-- 4,
-- (select id from atlas_driver_offer_bpp.merchant where short_id = 'MSIL_PARTNER'),
-- (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Delhi' and merchant_short_id = 'MSIL_PARTNER'),
-- 3,
-- true,
-- false,
-- false,
-- 'Business License',
-- now(),
-- now(),
-- 'FLEET_BUSINESS');

-- INSERT INTO atlas_driver_offer_bpp.fleet_owner_document_verification_config
-- (check_expiry, check_extraction, dependency_document_type, description, disable_warning, document_type, is_disabled, is_hidden, is_mandatory, max_retry_count, merchant_id, merchant_operating_city_id, "order",is_image_validation_required,is_default_enabled_on_manual_verification, do_strict_verifcation , title, created_at, updated_at, role)
-- VALUES
-- (false,
-- false,
-- '{}',
-- null,
-- '',
-- 'GSTCertificate',
-- false,
-- false,
-- true,
-- 4,
-- (select id from atlas_driver_offer_bpp.merchant where short_id = 'MSIL_PARTNER'),
-- (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Delhi' and merchant_short_id = 'MSIL_PARTNER'),
-- 4,
-- true,
-- false,
-- false,
-- 'GST Certificate',
-- now(),
-- now(),
-- 'FLEET_BUSINESS');

-- Fleet Business Onboarding Documents (LOCAL) --

INSERT INTO atlas_driver_offer_bpp.fleet_owner_document_verification_config
(check_expiry, check_extraction, dependency_document_type, description, disable_warning, document_type, is_disabled, is_hidden, is_mandatory, max_retry_count, merchant_id, merchant_operating_city_id, "order",is_image_validation_required,is_default_enabled_on_manual_verification, do_strict_verifcation , title, created_at, updated_at, role)
VALUES
(false,
false,
'{}',
null,
'',
'AadhaarCard',
false,
false,
true,
4,
(select id from atlas_driver_offer_bpp.merchant where short_id = 'NAMMA_YATRI_PARTNER'),
(select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kochi' and merchant_short_id = 'NAMMA_YATRI_PARTNER'),
1,
true,
false,
false,
'Aadhaar Card',
now(),
now(),
'FLEET_BUSINESS') ON CONFLICT DO NOTHING;;

INSERT INTO atlas_driver_offer_bpp.fleet_owner_document_verification_config
(check_expiry, check_extraction, dependency_document_type, description, disable_warning, document_type, is_disabled, is_hidden, is_mandatory, max_retry_count, merchant_id, merchant_operating_city_id, "order",is_image_validation_required,is_default_enabled_on_manual_verification, do_strict_verifcation , title, created_at, updated_at, role)
VALUES
(false,
false,
'{}',
null,
'',
'PanCard',
false,
false,
true,
4,
(select id from atlas_driver_offer_bpp.merchant where short_id = 'NAMMA_YATRI_PARTNER'),
(select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kochi' and merchant_short_id = 'NAMMA_YATRI_PARTNER'),
2,
true,
false,
false,
'Business Pan Card', -- check if different
now(),
now(),
'FLEET_BUSINESS') ON CONFLICT DO NOTHING;;

INSERT INTO atlas_driver_offer_bpp.fleet_owner_document_verification_config
(check_expiry, check_extraction, dependency_document_type, description, disable_warning, document_type, is_disabled, is_hidden, is_mandatory, max_retry_count, merchant_id, merchant_operating_city_id, "order",is_image_validation_required,is_default_enabled_on_manual_verification, do_strict_verifcation , title, created_at, updated_at, role)
VALUES
(false,
false,
'{}',
null,
'',
'BusinessLicense',
false,
false,
true,
4,
(select id from atlas_driver_offer_bpp.merchant where short_id = 'NAMMA_YATRI_PARTNER'),
(select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kochi' and merchant_short_id = 'NAMMA_YATRI_PARTNER'),
3,
true,
false,
false,
'Business License',
now(),
now(),
'FLEET_BUSINESS') ON CONFLICT DO NOTHING;;

INSERT INTO atlas_driver_offer_bpp.fleet_owner_document_verification_config
(check_expiry, check_extraction, dependency_document_type, description, disable_warning, document_type, is_disabled, is_hidden, is_mandatory, max_retry_count, merchant_id, merchant_operating_city_id, "order",is_image_validation_required,is_default_enabled_on_manual_verification, do_strict_verifcation , title, created_at, updated_at, role)
VALUES
(false,
false,
'{}',
null,
'',
'GSTCertificate',
false,
false,
true,
4,
(select id from atlas_driver_offer_bpp.merchant where short_id = 'NAMMA_YATRI_PARTNER'),
(select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kochi' and merchant_short_id = 'NAMMA_YATRI_PARTNER'),
4,
true,
false,
false,
'GST Certificate',
now(),
now(),
'FLEET_BUSINESS') ON CONFLICT DO NOTHING;;

-- Updating new columns --

UPDATE atlas_driver_offer_bpp.fleet_owner_document_verification_config set document_category = 'Driver' where document_type in ('BackgroundVerification','DriverLicense','SubscriptionPlan','ProfilePhoto','AadhaarCard','SocialSecurityNumber','ProfileDetails','PanCard','GSTCertificate','BusinessLicense');
UPDATE atlas_driver_offer_bpp.fleet_owner_document_verification_config set document_category = 'Vehicle' where document_type in ('VehicleInsurance','VehiclePermit','VehicleRegistrationCertificate','VehicleInspectionForm','VehiclePUC','VehicleFitnessCertificate');
UPDATE atlas_driver_offer_bpp.fleet_owner_document_verification_config set document_category = 'Permission' where document_type in ('Permissions');
UPDATE atlas_driver_offer_bpp.fleet_owner_document_verification_config set document_category = 'Training' where document_type in ('Training');

-- Run in master only --

-- update atlas_driver_offer_bpp.document_verification_config set dependency_document_type = '{}' where document_type in ('AadhaarCard', 'PanCard') AND merchant_id = (select id from atlas_driver_offer_bpp.merchant where short_id = 'MSIL_PARTNER');

-- run in master (even if commented) --

-- INSERT INTO atlas_driver_offer_bpp.document_verification_config (
--     check_expiry,
--     check_extraction,
--     dependency_document_type,
--     description,
--     disable_warning,
--     document_type,
--     is_disabled,
--     is_hidden,
--     is_mandatory,
--     max_retry_count,
--     merchant_id,
--     merchant_operating_city_id,
--     rc_number_prefix_list,
--     supported_vehicle_classes_json,
--     title,
--     vehicle_category,
--     vehicle_class_check_type,
--     created_at,
--     updated_at,
--     is_default_enabled_on_manual_verification,
--     is_image_validation_required,
--     "order"
-- )
-- SELECT
--     check_expiry,
--     check_extraction,
--     dependency_document_type,
--     description,
--     disable_warning,
--     'InspectionHub',
--     is_disabled,
--     is_hidden,
--     is_mandatory,
--     max_retry_count,
--     (select id from atlas_driver_offer_bpp.merchant where short_id = 'MSIL_PARTNER') as merchant_id,
--     (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Delhi' and merchant_short_id = 'MSIL_PARTNER') as merchant_operating_city_id,
--     '{DL}',
--     supported_vehicle_classes_json,
--     title,
--     vehicle_category,
--     vehicle_class_check_type,
--     CURRENT_TIMESTAMP as created_at,
--     CURRENT_TIMESTAMP as updated_at,
--     is_default_enabled_on_manual_verification,
--     is_image_validation_required,
--     "order"
-- FROM
--     atlas_driver_offer_bpp.document_verification_config
-- WHERE
--     vehicle_category = 'CAR' AND merchant_operating_city_id = (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Delhi' and merchant_short_id = 'MSIL_PARTNER') AND document_type in ('VehicleInspectionForm') ON CONFLICT DO NOTHING;


-- update atlas_driver_offer_bpp.document_verification_config set document_category = 'Vehicle', description = 'Operation Hub Flow', title = 'Operation Hub Flow' where document_type = 'InspectionHub';

-- Run in master even if commented --

-- UPDATE atlas_driver_offer_bpp.transporter_config
-- SET
--   safety_team_numbers = ARRAY['6666666666'],
--   local_police_numbers = ARRAY['6666666666'],
--   local_ambulance_numbers = ARRAY['6666666666']
-- WHERE merchant_operating_city_id IN (
--   SELECT id
--   FROM atlas_driver_offer_bpp.merchant_operating_city
--   WHERE city = 'Delhi'
--     AND merchant_short_id = 'MSIL_PARTNER'
-- );
