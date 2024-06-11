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
    is_mandatory,
    max_retry_count,
    merchant_id,
    (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri') as merchant_operating_city_id,
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
    vehicle_category = 'CAR' AND merchant_id = (select merchant_id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri');

UPDATE atlas_driver_offer_bpp.document_verification_config
SET supported_vehicle_classes_json = json_build_array('MCWG', 'MCWOG')
WHERE document_type = 'DriverLicense' AND vehicle_category = 'MOTORCYCLE';

update atlas_driver_offer_bpp.document_verification_config set supported_vehicle_classes_json =
    json_build_array(json_build_object('vehicleClass', '2WN', 'vehicleVariant', 'BIKE'),
    json_build_object('vehicleClass', '2W', 'vehicleVariant', 'BIKE'),
    json_build_object('vehicleClass', '2WIC', 'vehicleVariant', 'BIKE'),
    json_build_object('vehicleClass', '2WT', 'vehicleVariant', 'BIKE'),
    json_build_object('vehicleClass', 'M-Cycle/Scooter', 'vehicleVariant', 'BIKE'),
    json_build_object('vehicleClass', 'M-Cycle/Scooter-With Side Car(2WN)', 'vehicleVariant', 'BIKE'),
    json_build_object('vehicleClass', 'Moped(2WN)', 'vehicleVariant', 'BIKE'),
    json_build_object('vehicleClass', 'Mopeds and Motorised Cycle', 'vehicleVariant', 'BIKE'),
    json_build_object('vehicleClass', 'MOTOR CYCLE', 'vehicleVariant', 'BIKE'),
    json_build_object('vehicleClass', 'Motor Cycle for Hire', 'vehicleVariant', 'BIKE'),
    json_build_object('vehicleClass', 'MOTOR CYCLE FOR HIRE', 'vehicleVariant', 'BIKE'),
    json_build_object('vehicleClass', 'Motor Cycle/Scooter-Used For Hire', 'vehicleVariant', 'BIKE'),
    json_build_object('vehicleClass', 'Motor Cycle/Scooter-Used For Hire(2WT)', 'vehicleVariant', 'BIKE'),
    json_build_object('vehicleClass', 'Motor Cycle/Scooter-Used For Hire(OTH)', 'vehicleVariant', 'BIKE'),
    json_build_object('vehicleClass', 'Motorised Cycle (CC > 25cc)', 'vehicleVariant', 'BIKE')
    ) where document_type = 'VehicleRegistrationCertificate' and vehicle_category = 'MOTORCYCLE';

-- SUBSCRIPTION PLAN FOR BIKE SPECIFIC IN YS WITH MANDATORY
insert into atlas_driver_offer_bpp.document_verification_config (check_expiry, check_extraction, dependency_document_type, description, disable_warning, document_type, is_disabled, is_hidden, is_mandatory, max_retry_count, merchant_id, merchant_operating_city_id, rc_number_prefix_list, supported_vehicle_classes_json, title, vehicle_category, vehicle_class_check_type, created_at, updated_at, is_default_enabled_on_manual_verification, is_image_validation_required)
    select false, false, '{VehicleRegistrationCertificate}', null, null, 'SubscriptionPlan', false, false, false, 3, m.merchant_id, m.id, '{WB,JH,BR,OR}', '[]', 'Yatri Sathi Subscription Plan', 'MOTORCYCLE', 'Infix', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, false, true
    from atlas_driver_offer_bpp.merchant_operating_city as m where city = 'Siliguri';

-- RUN THIS ONLY AFTER CHANGE IN plan.sql FILE: ADDITION OF NULLABLE vehicle_variant COLUMN
INSERT INTO atlas_driver_offer_bpp.plan
	(id, merchant_id, payment_mode, plan_type, name, description, max_amount, registration_amount, plan_base_amount, is_offer_applicable, max_credit_limit, free_ride_count, frequency, cgst_percentage, sgst_percentage, max_mandate_amount, subscribed_flag_toggle_allowed, is_deprecated, eligible_for_coin_discount, merchant_op_city_id, service_name, based_on_entity, vehicle_variant)
	select md5('6d49ec05-74f8-40a5-b038-503af38763fc'|| CURRENT_TIMESTAMP || m.id) :: uuid, m.merchant_id, 'AUTOPAY', 'SUBSCRIPTION', 'DAILY PER RIDE', 'Unlimited number of rides per day', 5000, 1, 'PERRIDE_2', true, 350, 0, 'DAILY', 0.09, 0.09, 150.0, true, false, true, m.id, 'YATRI_SUBSCRIPTION', 'RIDE', 'BIKE'
	from atlas_driver_offer_bpp.merchant_operating_city as m where city = 'Siliguri';

INSERT INTO atlas_driver_offer_bpp.plan
	(id, merchant_id, payment_mode, plan_type, name, description, max_amount, registration_amount, plan_base_amount, is_offer_applicable, max_credit_limit, free_ride_count, frequency, cgst_percentage, sgst_percentage, max_mandate_amount, subscribed_flag_toggle_allowed, is_deprecated, eligible_for_coin_discount, merchant_op_city_id, service_name, based_on_entity, vehicle_variant)
	select md5('6d49ec05-74f8-40a5-b038-503af38763fc'|| CURRENT_TIMESTAMP || m.id) :: uuid, m.merchant_id, 'MANUAL', 'SUBSCRIPTION', 'DAILY PER RIDE', 'Unlimited number of rides per day', 5000, 1, 'PERRIDE_2', true, 350, 0, 'DAILY', 0.09, 0.09, 150.0, true, false, true, m.id, 'YATRI_SUBSCRIPTION', 'RIDE', 'BIKE'
	from atlas_driver_offer_bpp.merchant_operating_city as m where city = 'Siliguri';

-- INSERT INTO vehicle_service_tier
insert into atlas_driver_offer_bpp.vehicle_service_tier (id, name, merchant_id, merchant_operating_city_id, seating_capacity, air_conditioned, driver_rating, vehicle_rating, luggage_capacity, short_description, long_description, allowed_vehicle_variant, default_for_vehicle_variant, service_tier_type, created_at, updated_at, auto_selected_vehicle_variant)
(select
    md5(random()::text || clock_timestamp()::text)::uuid,
    'Bike Taxi',
    m.merchant_id,
    m.id,
    null,
    null,
    null,
    null,
    null,
    'Commute friendly',
    null,
    '{BIKE}',
    '{BIKE}',
    'BIKE',
    now(),
    now(),
    '{BIKE}'
    from atlas_driver_offer_bpp.merchant_operating_city as m where city = 'Siliguri');

-- DONT RUN IN MASTER
update atlas_driver_offer_bpp.document_verification_config
set is_default_enabled_on_manual_verification = 'true' where document_type = 'AadhaarCard' and merchant_operating_city_id = (select id from atlas_driver_offer_bpp.merchant_operating_city
where city = 'Siliguri') and vehicle_category = 'MOTORCYCLE';

-- RUN IN MASTER and PROD
INSERT INTO atlas_driver_offer_bpp.beckn_config (Collected_By, Domain, Gateway_URL, ID, Payment_Params_Json, Registry_URL, Subscriber_ID, Subscriber_URL, Unique_Key_ID, Vehicle_Category, Merchant_ID, Created_At, Updated_At, On_Select_Ttl_Sec, On_Search_Ttl_Sec, On_Init_Ttl_Sec, On_Confirm_Ttl_Sec, On_Track_Ttl_Sec, On_Status_Ttl_Sec, On_Cancel_Ttl_Sec, On_Update_Ttl_Sec)
SELECT 'BPP', 'MOBILITY', 'http://localhost:8015/v1', md5('caa19ac8-2e34-410e-ab9a-76bdf3504761' || CURRENT_TIMESTAMP || m.id) :: uuid, '{"bankAccNumber": "xyz@upi","bankCode": "xyz"}', 'https://beta.beckn.uat.juspay.net/dev/registry', 'api.sandbox.beckn.juspay.in/dev/dobpp/beckn/96dd7f78-787e-4a0b-8675-e9e6fe93bb8f', 'https://api.sandbox.beckn.juspay.in/dev/dobpp/beckn/96dd7f78-787e-4a0b-8675-e9e6fe93bb8f', 41, 'MOTORCYCLE', m.merchant_id, '2024-02-23T11:44:55.861452Z', '2024-02-23T11:44:55.861452Z', 120, 120, 120, 120, 120, 120, 120, 120
FROM atlas_driver_offer_bpp.merchant_operating_city as m where city = 'Siliguri';
