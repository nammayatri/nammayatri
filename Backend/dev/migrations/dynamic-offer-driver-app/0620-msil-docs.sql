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

-- update atlas_driver_offer_bpp.transporter_config set onboarding_retry_time_in_hours = 48 where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where merchant_short_id = 'MSIL_PARTNER');

-- UPDATE atlas_driver_offer_bpp.document_verification_config SET check_extraction = false where merchant_operating_city_id in (SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city WHERE city = 'Delhi' and merchant_short_id = 'MSIL_PARTNER') and document_type = 'VehicleRegistrationCertificate';

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

-- Run in master even if commented --
-- update atlas_driver_offer_bpp.rc_validation_rules set vehicle_class = null;

-- INSERT INTO atlas_driver_offer_bpp.rc_validation_rules (
--   id,
--   max_vehicle_age,
--   fuel_type,
--   vehicle_class,
--   vehicle_oem,
--   merchant_id,
--   merchant_operating_city_id
-- )
-- SELECT
--   atlas_driver_offer_bpp.uuid_generate_v4(),
--   36,
--   ARRAY['Petrol', 'Diesel', 'CNG', 'LPG', 'EV', 'Electric'],
--   ARRAY['SUV', 'SEDAN'],
--   ARRAY['MARUTI'],
--   (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER'),
--   (SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city WHERE merchant_short_id = 'MSIL_PARTNER' AND city = 'Delhi');

-- update atlas_driver_offer_bpp.document_verification_config set supported_vehicle_classes_json = '[
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "brezza",
--         "priority": 151,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI BREZZA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "brezza",
--         "priority": 152,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI BREZZA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "brezza",
--         "priority": 153,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI BREZZA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "brezza",
--         "priority": 154,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI BREZZA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "brezza",
--         "priority": 155,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI BREZZA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "brezza",
--         "manufacturerModel": null,
--         "priority": 156,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI BREZZA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "brezza",
--         "manufacturerModel": null,
--         "priority": 157,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI BREZZA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "brezza",
--         "manufacturerModel": null,
--         "priority": 158,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI BREZZA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "brezza",
--         "manufacturerModel": null,
--         "priority": 159,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI BREZZA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "brezza",
--         "manufacturerModel": null,
--         "priority": 160,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI BREZZA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "ciaz",
--         "priority": 241,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI CIAZ",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "ciaz",
--         "priority": 242,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI CIAZ",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "ciaz",
--         "priority": 243,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI CIAZ",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "ciaz",
--         "priority": 244,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI CIAZ",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "ciaz",
--         "priority": 245,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI CIAZ",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "ciaz",
--         "manufacturerModel": null,
--         "priority": 246,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI CIAZ",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "ciaz",
--         "manufacturerModel": null,
--         "priority": 247,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI CIAZ",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "ciaz",
--         "manufacturerModel": null,
--         "priority": 248,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI CIAZ",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "ciaz",
--         "manufacturerModel": null,
--         "priority": 249,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI CIAZ",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "ciaz",
--         "manufacturerModel": null,
--         "priority": 250,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI CIAZ",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "dezire",
--         "priority": 341,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "dezire",
--         "priority": 342,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "dezire",
--         "priority": 343,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "dezire",
--         "priority": 344,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "dezire",
--         "priority": 345,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "dezire",
--         "manufacturerModel": null,
--         "priority": 346,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "dezire",
--         "manufacturerModel": null,
--         "priority": 347,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "dezire",
--         "manufacturerModel": null,
--         "priority": 348,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "dezire",
--         "manufacturerModel": null,
--         "priority": 349,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "dezire",
--         "manufacturerModel": null,
--         "priority": 350,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "dzire",
--         "priority": 351,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "dzire",
--         "priority": 352,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "dzire",
--         "priority": 353,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "dzire",
--         "priority": 354,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "dzire",
--         "priority": 355,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "dzire",
--         "manufacturerModel": null,
--         "priority": 356,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "dzire",
--         "manufacturerModel": null,
--         "priority": 357,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "dzire",
--         "manufacturerModel": null,
--         "priority": 358,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "dzire",
--         "manufacturerModel": null,
--         "priority": 359,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "dzire",
--         "manufacturerModel": null,
--         "priority": 360,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "d zire",
--         "priority": 361,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "d zire",
--         "priority": 362,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "d zire",
--         "priority": 363,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "d zire",
--         "priority": 364,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "d zire",
--         "priority": 365,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "d zire",
--         "manufacturerModel": null,
--         "priority": 366,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "d zire",
--         "manufacturerModel": null,
--         "priority": 367,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "d zire",
--         "manufacturerModel": null,
--         "priority": 368,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "d zire",
--         "manufacturerModel": null,
--         "priority": 369,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "d zire",
--         "manufacturerModel": null,
--         "priority": 370,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "dizire",
--         "priority": 371,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "dizire",
--         "priority": 372,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "dizire",
--         "priority": 373,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "dizire",
--         "priority": 374,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "dizire",
--         "priority": 375,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "dizire",
--         "manufacturerModel": null,
--         "priority": 376,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "dizire",
--         "manufacturerModel": null,
--         "priority": 377,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "dizire",
--         "manufacturerModel": null,
--         "priority": 378,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "dizire",
--         "manufacturerModel": null,
--         "priority": 379,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "dizire",
--         "manufacturerModel": null,
--         "priority": 380,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "desire",
--         "priority": 381,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "desire",
--         "priority": 382,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "desire",
--         "priority": 383,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "desire",
--         "priority": 384,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "desire",
--         "priority": 385,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "desire",
--         "manufacturerModel": null,
--         "priority": 386,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "desire",
--         "manufacturerModel": null,
--         "priority": 387,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "desire",
--         "manufacturerModel": null,
--         "priority": 388,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "desire",
--         "manufacturerModel": null,
--         "priority": 389,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "desire",
--         "manufacturerModel": null,
--         "priority": 390,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "eeco",
--         "priority": 491,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI EECO",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "eeco",
--         "priority": 492,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI EECO",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "eeco",
--         "priority": 493,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI EECO",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "eeco",
--         "priority": 494,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI EECO",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "eeco",
--         "priority": 495,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI EECO",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "eeco",
--         "manufacturerModel": null,
--         "priority": 496,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI EECO",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "eeco",
--         "manufacturerModel": null,
--         "priority": 497,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI EECO",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "eeco",
--         "manufacturerModel": null,
--         "priority": 498,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI EECO",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "eeco",
--         "manufacturerModel": null,
--         "priority": 499,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI EECO",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "eeco",
--         "manufacturerModel": null,
--         "priority": 500,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI EECO",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "ertiga",
--         "priority": 551,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "ertiga",
--         "priority": 552,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "ertiga",
--         "priority": 553,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "ertiga",
--         "priority": 554,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "ertiga",
--         "priority": 555,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "ertiga",
--         "manufacturerModel": null,
--         "priority": 556,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "ertiga",
--         "manufacturerModel": null,
--         "priority": 557,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "ertiga",
--         "manufacturerModel": null,
--         "priority": 558,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "ertiga",
--         "manufacturerModel": null,
--         "priority": 559,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "ertiga",
--         "manufacturerModel": null,
--         "priority": 560,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "esteem",
--         "priority": 561,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI ESTEEM",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "esteem",
--         "priority": 562,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI ESTEEM",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "esteem",
--         "priority": 563,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI ESTEEM",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "esteem",
--         "priority": 564,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI ESTEEM",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "esteem",
--         "priority": 565,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI ESTEEM",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "esteem",
--         "manufacturerModel": null,
--         "priority": 566,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI ESTEEM",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "esteem",
--         "manufacturerModel": null,
--         "priority": 567,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI ESTEEM",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "esteem",
--         "manufacturerModel": null,
--         "priority": 568,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI ESTEEM",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "esteem",
--         "manufacturerModel": null,
--         "priority": 569,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI ESTEEM",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "esteem",
--         "manufacturerModel": null,
--         "priority": 570,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI ESTEEM",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "maruthi 1000",
--         "priority": 911,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI MARUTHI 1000",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "maruthi 1000",
--         "priority": 912,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI MARUTHI 1000",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "maruthi 1000",
--         "priority": 913,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI MARUTHI 1000",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "maruthi 1000",
--         "priority": 914,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI MARUTHI 1000",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "maruthi 1000",
--         "priority": 915,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI MARUTHI 1000",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "maruthi 1000",
--         "manufacturerModel": null,
--         "priority": 916,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI MARUTHI 1000",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "maruthi 1000",
--         "manufacturerModel": null,
--         "priority": 917,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI MARUTHI 1000",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "maruthi 1000",
--         "manufacturerModel": null,
--         "priority": 918,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI MARUTHI 1000",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "maruthi 1000",
--         "manufacturerModel": null,
--         "priority": 919,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI MARUTHI 1000",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "maruthi 1000",
--         "manufacturerModel": null,
--         "priority": 920,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI MARUTHI 1000",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "sx4",
--         "priority": 1171,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI SX4",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "sx4",
--         "priority": 1172,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI SX4",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "sx4",
--         "priority": 1173,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI SX4",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "sx4",
--         "priority": 1174,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI SX4",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "sx4",
--         "priority": 1175,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI SX4",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "sx4",
--         "manufacturerModel": null,
--         "priority": 1176,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI SX4",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "sx4",
--         "manufacturerModel": null,
--         "priority": 1177,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI SX4",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "sx4",
--         "manufacturerModel": null,
--         "priority": 1178,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI SX4",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "sx4",
--         "manufacturerModel": null,
--         "priority": 1179,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI SX4",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "sx4",
--         "manufacturerModel": null,
--         "priority": 1180,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI SX4",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "sx 4",
--         "priority": 1181,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI SX4",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "sx 4",
--         "priority": 1182,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI SX4",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "sx 4",
--         "priority": 1183,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI SX4",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "sx 4",
--         "priority": 1184,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI SX4",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "sx 4",
--         "priority": 1185,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI SX4",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "sx 4",
--         "manufacturerModel": null,
--         "priority": 1186,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI SX4",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "sx 4",
--         "manufacturerModel": null,
--         "priority": 1187,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI SX4",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "sx 4",
--         "manufacturerModel": null,
--         "priority": 1188,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI SX4",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "sx 4",
--         "manufacturerModel": null,
--         "priority": 1189,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI SX4",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "sx 4",
--         "manufacturerModel": null,
--         "priority": 1190,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI SX4",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "vitara",
--         "priority": 1321,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI VITARA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "vitara",
--         "priority": 1322,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI VITARA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "vitara",
--         "priority": 1323,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI VITARA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "vitara",
--         "priority": 1324,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI VITARA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "vitara",
--         "priority": 1325,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI VITARA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "vitara",
--         "manufacturerModel": null,
--         "priority": 1326,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI VITARA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "vitara",
--         "manufacturerModel": null,
--         "priority": 1327,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI VITARA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "vitara",
--         "manufacturerModel": null,
--         "priority": 1328,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI VITARA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "vitara",
--         "manufacturerModel": null,
--         "priority": 1329,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI VITARA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "vitara",
--         "manufacturerModel": null,
--         "priority": 1330,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI VITARA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "xl6",
--         "priority": 1381,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI XL6",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "xl6",
--         "priority": 1382,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI XL6",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "xl6",
--         "priority": 1383,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI XL6",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "xl6",
--         "priority": 1384,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI XL6",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "xl6",
--         "priority": 1385,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI XL6",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "xl6",
--         "manufacturerModel": null,
--         "priority": 1386,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI XL6",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "xl6",
--         "manufacturerModel": null,
--         "priority": 1387,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI XL6",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "xl6",
--         "manufacturerModel": null,
--         "priority": 1388,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI XL6",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "xl6",
--         "manufacturerModel": null,
--         "priority": 1389,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI XL6",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "xl6",
--         "manufacturerModel": null,
--         "priority": 1390,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI XL6",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "xl7",
--         "priority": 1391,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI XL7",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "xl7",
--         "priority": 1392,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI XL7",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "xl7",
--         "priority": 1393,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI XL7",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "xl7",
--         "priority": 1394,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI XL7",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "xl7",
--         "priority": 1395,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI XL7",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "xl7",
--         "manufacturerModel": null,
--         "priority": 1396,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI XL7",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "xl7",
--         "manufacturerModel": null,
--         "priority": 1397,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI XL7",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "xl7",
--         "manufacturerModel": null,
--         "priority": 1398,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI XL7",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "xl7",
--         "manufacturerModel": null,
--         "priority": 1399,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI XL7",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "xl7",
--         "manufacturerModel": null,
--         "priority": 1400,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI XL7",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "zeta",
--         "priority": 1421,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI ZETA",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "zeta",
--         "priority": 1422,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI ZETA",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "zeta",
--         "priority": 1423,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI ZETA",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "zeta",
--         "priority": 1424,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI ZETA",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "zeta",
--         "priority": 1425,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI ZETA",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "zeta",
--         "manufacturerModel": null,
--         "priority": 1426,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI ZETA",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "zeta",
--         "manufacturerModel": null,
--         "priority": 1427,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI ZETA",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "zeta",
--         "manufacturerModel": null,
--         "priority": 1428,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI ZETA",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "zeta",
--         "manufacturerModel": null,
--         "priority": 1429,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI ZETA",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "zeta",
--         "manufacturerModel": null,
--         "priority": 1430,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI ZETA",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tours",
--         "priority": 1501,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tours",
--         "priority": 1502,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tours",
--         "priority": 1503,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tours",
--         "priority": 1504,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tours",
--         "priority": 1505,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tours",
--         "manufacturerModel": null,
--         "priority": 1506,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tours",
--         "manufacturerModel": null,
--         "priority": 1507,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tours",
--         "manufacturerModel": null,
--         "priority": 1508,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tours",
--         "manufacturerModel": null,
--         "priority": 1509,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tours",
--         "manufacturerModel": null,
--         "priority": 1510,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour s",
--         "priority": 1511,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour s",
--         "priority": 1512,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour s",
--         "priority": 1513,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour s",
--         "priority": 1514,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour s",
--         "priority": 1515,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour s",
--         "manufacturerModel": null,
--         "priority": 1516,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour s",
--         "manufacturerModel": null,
--         "priority": 1517,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour s",
--         "manufacturerModel": null,
--         "priority": 1518,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour s",
--         "manufacturerModel": null,
--         "priority": 1519,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour s",
--         "manufacturerModel": null,
--         "priority": 1520,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour m",
--         "priority": 1521,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour m",
--         "priority": 1522,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour m",
--         "priority": 1523,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour m",
--         "priority": 1524,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour m",
--         "priority": 1525,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour m",
--         "manufacturerModel": null,
--         "priority": 1526,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour m",
--         "manufacturerModel": null,
--         "priority": 1527,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour m",
--         "manufacturerModel": null,
--         "priority": 1528,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour m",
--         "manufacturerModel": null,
--         "priority": 1529,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour m",
--         "manufacturerModel": null,
--         "priority": 1530,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tourm",
--         "priority": 1531,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tourm",
--         "priority": 1532,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tourm",
--         "priority": 1533,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tourm",
--         "priority": 1534,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tourm",
--         "priority": 1535,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tourm",
--         "manufacturerModel": null,
--         "priority": 1536,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tourm",
--         "manufacturerModel": null,
--         "priority": 1537,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tourm",
--         "manufacturerModel": null,
--         "priority": 1538,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tourm",
--         "manufacturerModel": null,
--         "priority": 1539,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tourm",
--         "manufacturerModel": null,
--         "priority": 1540,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour v",
--         "priority": 1541,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI EECO",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour v",
--         "priority": 1542,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI EECO",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour v",
--         "priority": 1543,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI EECO",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour v",
--         "priority": 1544,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI EECO",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour v",
--         "priority": 1545,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI EECO",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour v",
--         "manufacturerModel": null,
--         "priority": 1546,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI EECO",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour v",
--         "manufacturerModel": null,
--         "priority": 1547,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI EECO",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour v",
--         "manufacturerModel": null,
--         "priority": 1548,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI EECO",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour v",
--         "manufacturerModel": null,
--         "priority": 1549,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI EECO",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour v",
--         "manufacturerModel": null,
--         "priority": 1550,
--         "reviewRequired": false,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI EECO",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour",
--         "priority": 1551,
--         "reviewRequired": true,
--         "vehicleCapacity": 8,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour",
--         "priority": 1552,
--         "reviewRequired": true,
--         "vehicleCapacity": 8,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour",
--         "priority": 1553,
--         "reviewRequired": true,
--         "vehicleCapacity": 8,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour",
--         "priority": 1554,
--         "reviewRequired": true,
--         "vehicleCapacity": 8,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour",
--         "priority": 1555,
--         "reviewRequired": true,
--         "vehicleCapacity": 8,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour",
--         "manufacturerModel": null,
--         "priority": 1556,
--         "reviewRequired": true,
--         "vehicleCapacity": 8,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour",
--         "manufacturerModel": null,
--         "priority": 1557,
--         "reviewRequired": true,
--         "vehicleCapacity": 8,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour",
--         "manufacturerModel": null,
--         "priority": 1558,
--         "reviewRequired": true,
--         "vehicleCapacity": 8,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour",
--         "manufacturerModel": null,
--         "priority": 1559,
--         "reviewRequired": true,
--         "vehicleCapacity": 8,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour",
--         "manufacturerModel": null,
--         "priority": 1560,
--         "reviewRequired": true,
--         "vehicleCapacity": 8,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour",
--         "priority": 1561,
--         "reviewRequired": true,
--         "vehicleCapacity": 7,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour",
--         "priority": 1562,
--         "reviewRequired": true,
--         "vehicleCapacity": 7,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour",
--         "priority": 1563,
--         "reviewRequired": true,
--         "vehicleCapacity": 7,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour",
--         "priority": 1564,
--         "reviewRequired": true,
--         "vehicleCapacity": 7,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour",
--         "priority": 1565,
--         "reviewRequired": true,
--         "vehicleCapacity": 7,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour",
--         "manufacturerModel": null,
--         "priority": 1566,
--         "reviewRequired": true,
--         "vehicleCapacity": 7,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour",
--         "manufacturerModel": null,
--         "priority": 1567,
--         "reviewRequired": true,
--         "vehicleCapacity": 7,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour",
--         "manufacturerModel": null,
--         "priority": 1568,
--         "reviewRequired": true,
--         "vehicleCapacity": 7,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour",
--         "manufacturerModel": null,
--         "priority": 1569,
--         "reviewRequired": true,
--         "vehicleCapacity": 7,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour",
--         "manufacturerModel": null,
--         "priority": 1570,
--         "reviewRequired": true,
--         "vehicleCapacity": 7,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour",
--         "priority": 1571,
--         "reviewRequired": true,
--         "vehicleCapacity": 5,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour",
--         "priority": 1572,
--         "reviewRequired": true,
--         "vehicleCapacity": 5,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour",
--         "priority": 1573,
--         "reviewRequired": true,
--         "vehicleCapacity": 5,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour",
--         "priority": 1574,
--         "reviewRequired": true,
--         "vehicleCapacity": 5,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour",
--         "manufacturerModel": null,
--         "priority": 1576,
--         "reviewRequired": true,
--         "vehicleCapacity": 5,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour",
--         "manufacturerModel": null,
--         "priority": 1577,
--         "reviewRequired": true,
--         "vehicleCapacity": 5,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour",
--         "manufacturerModel": null,
--         "priority": 1578,
--         "reviewRequired": true,
--         "vehicleCapacity": 5,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour",
--         "manufacturerModel": null,
--         "priority": 1579,
--         "reviewRequired": true,
--         "vehicleCapacity": 5,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour",
--         "priority": 1581,
--         "reviewRequired": true,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour",
--         "priority": 1582,
--         "reviewRequired": true,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour",
--         "priority": 1583,
--         "reviewRequired": true,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": "tour",
--         "priority": 1584,
--         "reviewRequired": true,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour",
--         "manufacturerModel": null,
--         "priority": 1586,
--         "reviewRequired": true,
--         "vehicleCapacity": null,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour",
--         "manufacturerModel": null,
--         "priority": 1587,
--         "reviewRequired": true,
--         "vehicleCapacity": null,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour",
--         "manufacturerModel": null,
--         "priority": 1588,
--         "reviewRequired": true,
--         "vehicleCapacity": null,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": "tour",
--         "manufacturerModel": null,
--         "priority": 1589,
--         "reviewRequired": true,
--         "vehicleCapacity": null,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": null,
--         "priority": 1591,
--         "reviewRequired": true,
--         "vehicleCapacity": 8,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": null,
--         "priority": 1592,
--         "reviewRequired": true,
--         "vehicleCapacity": 8,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": null,
--         "priority": 1593,
--         "reviewRequired": true,
--         "vehicleCapacity": 8,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": null,
--         "priority": 1594,
--         "reviewRequired": true,
--         "vehicleCapacity": 8,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": null,
--         "priority": 1595,
--         "reviewRequired": true,
--         "vehicleCapacity": 8,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": null,
--         "priority": 1596,
--         "reviewRequired": true,
--         "vehicleCapacity": 7,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": null,
--         "priority": 1597,
--         "reviewRequired": true,
--         "vehicleCapacity": 7,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": null,
--         "priority": 1598,
--         "reviewRequired": true,
--         "vehicleCapacity": 7,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": null,
--         "priority": 1599,
--         "reviewRequired": true,
--         "vehicleCapacity": 7,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": null,
--         "priority": 1600,
--         "reviewRequired": true,
--         "vehicleCapacity": 7,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": null,
--         "priority": 1601,
--         "reviewRequired": true,
--         "vehicleCapacity": 6,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": null,
--         "priority": 1602,
--         "reviewRequired": true,
--         "vehicleCapacity": 6,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": null,
--         "priority": 1603,
--         "reviewRequired": true,
--         "vehicleCapacity": 6,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": null,
--         "priority": 1604,
--         "reviewRequired": true,
--         "vehicleCapacity": 6,
--         "vehicleClass": "pcv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": null,
--         "priority": 1605,
--         "reviewRequired": true,
--         "vehicleCapacity": 6,
--         "vehicleClass": "lmv",
--         "vehicleModel": "MARUTI ERTIGA",
--         "vehicleVariant": "SUV"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": null,
--         "priority": 1606,
--         "reviewRequired": true,
--         "vehicleCapacity": 5,
--         "vehicleClass": "cab",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": null,
--         "priority": 1607,
--         "reviewRequired": true,
--         "vehicleCapacity": 5,
--         "vehicleClass": "light passenger vehicle",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     },
--     {
--         "bodyType": null,
--         "manufacturer": null,
--         "manufacturerModel": null,
--         "priority": 1608,
--         "reviewRequired": true,
--         "vehicleCapacity": 5,
--         "vehicleClass": "lpv",
--         "vehicleModel": "MARUTI DZIRE",
--         "vehicleVariant": "SEDAN"
--     }
-- ]'
-- where document_type = 'VehicleRegistrationCertificate' and vehicle_category = 'CAR' and merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where
-- merchant_short_id = 'MSIL_PARTNER' and city = 'Delhi');


-- Subscriptions --

-- Master -- (Don't run again if entries are present already)
INSERT INTO atlas_driver_offer_bpp.plan (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, cgst_percentage, max_mandate_amount, merchant_op_city_id, sgst_percentage, billing_type, validity_in_days, service_name, vehicle_category, allow_strike_off)
SELECT 'a35ffc7c-qe0d-4dcc-83a8-e36a5a29cc1c', m.merchant_id, 'MANUAL', 'FLEXIBLE', 'RECHARGE_5000.0', 'STANDARD' , 'Enjoy rides worth 5K in 500!', 0, 500, true, 0, 0, 'SUBSCRIPTION', 0.09, 0.0, m.id, 0.09, 'PREPAID', 365, 'PREPAID_SUBSCRIPTION', 'CAR', true FROM atlas_driver_offer_bpp.merchant_operating_city AS m where m.city = 'Delhi' and m.merchant_short_id = 'MSIL_PARTNER';

INSERT INTO atlas_driver_offer_bpp.plan (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, cgst_percentage, max_mandate_amount, merchant_op_city_id, sgst_percentage, billing_type, validity_in_days, service_name, vehicle_category, allow_strike_off)
SELECT '28911beb-38ba-456d-8cca-4d019461d2b1', m.merchant_id, 'MANUAL', 'FLEXIBLE', 'RECHARGE_500.0', 'ECONOMY' , 'Enjoy rides worth 500 in 50!', 0, 50, true, 0, 0, 'SUBSCRIPTION', 0.09, 0.0, m.id, 0.09, 'PREPAID', 365, 'PREPAID_SUBSCRIPTION', 'CAR', true FROM atlas_driver_offer_bpp.merchant_operating_city AS m where m.city = 'Delhi' and m.merchant_short_id = 'MSIL_PARTNER';

-- Prod --
INSERT INTO atlas_driver_offer_bpp.plan (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, cgst_percentage, max_mandate_amount, merchant_op_city_id, sgst_percentage, billing_type, validity_in_days, service_name, vehicle_category, allow_strike_off)
SELECT 'a35ffc7c-qe0d-4dcc-83a8-e36a5a29cc1c', m.merchant_id, 'MANUAL', 'FLEXIBLE', 'RECHARGE_5000.0', 'STANDARD' , 'Enjoy rides worth 5K in 500!', 0, 500, true, 0, 0, 'SUBSCRIPTION', 0.09, 0.0, m.id, 0.09, 'PREPAID', 365, 'PREPAID_SUBSCRIPTION', 'CAR', true FROM atlas_driver_offer_bpp.merchant_operating_city AS m where m.city = 'Delhi' and m.merchant_short_id = 'MSIL_PARTNER';

INSERT INTO atlas_driver_offer_bpp.plan (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, cgst_percentage, max_mandate_amount, merchant_op_city_id, sgst_percentage, billing_type, validity_in_days, service_name, vehicle_category, allow_strike_off)
SELECT '28911beb-38ba-456d-8cca-4d019461d2b1', m.merchant_id, 'MANUAL', 'FLEXIBLE', 'RECHARGE_500.0', 'ECONOMY' , 'Enjoy rides worth 500 in 50!', 0, 50, true, 0, 0, 'SUBSCRIPTION', 0.09, 0.0, m.id, 0.09, 'PREPAID', 365, 'PREPAID_SUBSCRIPTION', 'CAR', true FROM atlas_driver_offer_bpp.merchant_operating_city AS m where m.city = 'Delhi' and m.merchant_short_id = 'MSIL_PARTNER';

-- Local --
INSERT INTO atlas_driver_offer_bpp.plan (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, cgst_percentage, max_mandate_amount, merchant_op_city_id, sgst_percentage, billing_type, validity_in_days, service_name, vehicle_category, allow_strike_off)
SELECT 'a35ffc7c-qe0d-4dcc-83a8-e36a5a29cc1c', m.merchant_id, 'MANUAL', 'FLEXIBLE', 'RECHARGE_5000.0', 'STANDARD' , 'Enjoy rides worth 5K in 500!', 0, 500, true, 0, 0, 'SUBSCRIPTION', 0.09, 0.0, m.id, 0.09, 'PREPAID', 365, 'PREPAID_SUBSCRIPTION', 'CAR', true FROM atlas_driver_offer_bpp.merchant_operating_city AS m where m.city = 'Kochi' and m.merchant_short_id = 'NAMMA_YATRI_PARTNER';

INSERT INTO atlas_driver_offer_bpp.plan (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, cgst_percentage, max_mandate_amount, merchant_op_city_id, sgst_percentage, billing_type, validity_in_days, service_name, vehicle_category, allow_strike_off)
SELECT '28911beb-38ba-456d-8cca-4d019461d2b1', m.merchant_id, 'MANUAL', 'FLEXIBLE', 'RECHARGE_500.0', 'ECONOMY' , 'Enjoy rides worth 500 in 50!', 0, 50, true, 0, 0, 'SUBSCRIPTION', 0.09, 0.0, m.id, 0.09, 'PREPAID', 365, 'PREPAID_SUBSCRIPTION', 'CAR', true FROM atlas_driver_offer_bpp.merchant_operating_city AS m where m.city = 'Kochi' and m.merchant_short_id = 'NAMMA_YATRI_PARTNER';

-- Transporter Config --

-- Prod --
UPDATE atlas_driver_offer_bpp.transporter_config
SET subscription = false
WHERE merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where merchant_short_id = 'MSIL_PARTNER' and city = 'Delhi');

-- Master --
UPDATE atlas_driver_offer_bpp.transporter_config
SET subscription = false
WHERE merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where merchant_short_id = 'MSIL_PARTNER' and city = 'Delhi');

-- Local --
UPDATE atlas_driver_offer_bpp.transporter_config
SET subscription = false
WHERE merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where merchant_short_id = 'NAMMA_YATRI_PARTNER' and city = 'Kochi');

-- Fare Policy --

-- Prod --
UPDATE atlas_driver_offer_bpp.fare_policy set platform_fee_charges_by = 'None' where id in (
select fare_policy_id from atlas_driver_offer_bpp.fare_product where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Delhi' and merchant_short_id = 'MSIL_PARTNER'));

-- Master --
UPDATE atlas_driver_offer_bpp.fare_policy set platform_fee_charges_by = 'None' where id in (
select fare_policy_id from atlas_driver_offer_bpp.fare_product where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Delhi' and merchant_short_id = 'MSIL_PARTNER'));

-- Local --
UPDATE atlas_driver_offer_bpp.fare_policy set platform_fee_charges_by = 'None' where id in (
select fare_policy_id from atlas_driver_offer_bpp.fare_product where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kochi' and merchant_short_id = 'NAMMA_YATRI_PARTNER'));

-- Subscription Config --

-- Local --
insert into atlas_driver_offer_bpp.subscription_config (subscription_enabled_for_vehicle_categories, autopay_enabled, free_trial_rides_applicable, number_of_free_trial_rides, default_city_vehicle_category, show_manual_plans_in_ui, allow_manual_payment_links, payment_link_channel, payment_link_job_time, generic_job_reschedule_time, use_overlay_service, generic_batch_size_for_jobs, deep_link_expiry_time_in_minutes, max_retry_count, service_name,send_in_app_fcm_notifications , allow_due_addition, payment_service_name,send_deep_link, merchant_id, merchant_operating_city_id, allow_driver_fee_calc_schedule, is_triggered_at_end_ride , created_at, updated_at)
select '{CAR}', false, false, 0, 'CAR', true, false, 'WHATSAPP', 21600, 60, true, 30, 15, 4, 'PREPAID_SUBSCRIPTION', true, false, 'Payment_Juspay' , false, m.merchant_id, m.id, false, true,now(), now()
from atlas_driver_offer_bpp.merchant_operating_city as m where m.city = 'Kochi' and m.merchant_short_id = 'NAMMA_YATRI_PARTNER';

-- Master --
insert into atlas_driver_offer_bpp.subscription_config (subscription_enabled_for_vehicle_categories, autopay_enabled, free_trial_rides_applicable, number_of_free_trial_rides, default_city_vehicle_category, show_manual_plans_in_ui, allow_manual_payment_links, payment_link_channel, payment_link_job_time, generic_job_reschedule_time, use_overlay_service, generic_batch_size_for_jobs, deep_link_expiry_time_in_minutes, max_retry_count, service_name,send_in_app_fcm_notifications , allow_due_addition, payment_service_name,send_deep_link, merchant_id, merchant_operating_city_id, allow_driver_fee_calc_schedule, is_triggered_at_end_ride , created_at, updated_at)
select '{CAR}', false, false, 0, 'CAR', true, false, 'WHATSAPP', 21600, 60, true, 30, 15, 4, 'PREPAID_SUBSCRIPTION', true, false, 'Payment_Juspay' , false, m.merchant_id, m.id, false, true,now(), now()
from atlas_driver_offer_bpp.merchant_operating_city as m where m.city = 'Delhi' and m.merchant_short_id = 'MSIL_PARTNER';

-- Prod --
insert into atlas_driver_offer_bpp.subscription_config (subscription_enabled_for_vehicle_categories, autopay_enabled, free_trial_rides_applicable, number_of_free_trial_rides, default_city_vehicle_category, show_manual_plans_in_ui, allow_manual_payment_links, payment_link_channel, payment_link_job_time, generic_job_reschedule_time, use_overlay_service, generic_batch_size_for_jobs, deep_link_expiry_time_in_minutes, max_retry_count, service_name,send_in_app_fcm_notifications , allow_due_addition, payment_service_name,send_deep_link, merchant_id, merchant_operating_city_id, allow_driver_fee_calc_schedule, is_triggered_at_end_ride , created_at, updated_at)
select '{CAR}', false, false, 0, 'CAR', true, false, 'WHATSAPP', 21600, 60, true, 30, 15, 4, 'PREPAID_SUBSCRIPTION', true, false, 'Payment_Juspay' , false, m.merchant_id, m.id, false, true,now(), now()
from atlas_driver_offer_bpp.merchant_operating_city as m where m.city = 'Delhi' and m.merchant_short_id = 'MSIL_PARTNER';

update atlas_driver_offer_bpp.merchant set prepaid_subscription_and_wallet_enabled = true where short_id = 'MSIL_PARTNER';


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
    'Local Residence Proof is required for the driver to start driving.',
    disable_warning,
    'LocalResidenceProof',
    is_disabled,
    is_hidden,
    true,
    max_retry_count,
    (select id from atlas_driver_offer_bpp.merchant where short_id = 'MSIL_PARTNER') as merchant_id,
   (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Delhi' and merchant_short_id = 'MSIL_PARTNER') as merchant_operating_city_id,
    '{DL}',
    supported_vehicle_classes_json,
    'Local Residence Proof',
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
    'Police Verification Certificate is required for the driver to start driving.',
    disable_warning,
    'PoliceVerificationCertificate',
    is_disabled,
    is_hidden,
    true,
    max_retry_count,
    (select id from atlas_driver_offer_bpp.merchant where short_id = 'MSIL_PARTNER') as merchant_id,
   (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Delhi' and merchant_short_id = 'MSIL_PARTNER') as merchant_operating_city_id,
    '{DL}',
    supported_vehicle_classes_json,
    'Police Verification Certificate',
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
    'MSDS Certificate is required for the driver to start driving.',
    disable_warning,
    'DrivingSchoolCertificate',
    is_disabled,
    true,
    true,
    max_retry_count,
    (select id from atlas_driver_offer_bpp.merchant where short_id = 'MSIL_PARTNER') as merchant_id,
   (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Delhi' and merchant_short_id = 'MSIL_PARTNER') as merchant_operating_city_id,
    '{DL}',
    supported_vehicle_classes_json,
    'Driving School Certificate',
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

update atlas_driver_offer_bpp.transporter_config set requires_driver_onboarding_inspection = true, separate_driver_vehicle_enablement = true, reminder_system_enabled = true where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where merchant_short_id = 'MSIL_PARTNER');