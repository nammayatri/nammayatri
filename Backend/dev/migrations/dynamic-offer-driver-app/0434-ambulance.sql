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
    (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata') as merchant_operating_city_id,
    '{WB}',
    supported_vehicle_classes_json,
    title,
    'AMBULANCE' AS vehicle_category,
    vehicle_class_check_type,
    CURRENT_TIMESTAMP as created_at,
    CURRENT_TIMESTAMP as updated_at,
    is_default_enabled_on_manual_verification,
    is_image_validation_required
FROM
    atlas_driver_offer_bpp.document_verification_config
WHERE
    vehicle_category = 'CAR' AND rc_number_prefix_list = '{WB}' AND document_type != 'SubscriptionPlan';

update atlas_driver_offer_bpp.document_verification_config set supported_vehicle_classes_json =
    json_build_array(json_build_object('vehicleClass', 'AMBULANCE', 'vehicleVariant', 'AMBULANCE_TAXI')) where document_type = 'VehicleRegistrationCertificate' and vehicle_category = 'AMBULANCE';

insert into atlas_driver_offer_bpp.vehicle_service_tier (id, name, merchant_id, merchant_operating_city_id, seating_capacity, air_conditioned, driver_rating, vehicle_rating, luggage_capacity, short_description, long_description, allowed_vehicle_variant, default_for_vehicle_variant, service_tier_type, created_at, updated_at, auto_selected_vehicle_variant, oxygen, ventilator)
(select
    md5(random()::text || clock_timestamp()::text)::uuid,
    'Ambulance Eco',
    m.merchant_id,
    m.id,
    null,
    null,
    null,
    null,
    null,
    'Ambulance Taxi', -- change this to eco?
    null,
    '{AMBULANCE_TAXI}',
    '{AMBULANCE_TAXI, AMBULANCE_TAXI_OXY, AMBULANCE_AC, AMBULANCE_AC_OXY, AMBULANCE_VENTILATOR}',
    'AMBULANCE_TAXI',
    now(),
    now(),
    '{AMBULANCE_TAXI}',
    null,
    null
    from atlas_driver_offer_bpp.merchant_operating_city as m where city = 'Kolkata');

insert into atlas_driver_offer_bpp.vehicle_service_tier (id, name, merchant_id, merchant_operating_city_id, seating_capacity, air_conditioned, driver_rating, vehicle_rating, luggage_capacity, short_description, long_description, allowed_vehicle_variant, default_for_vehicle_variant, service_tier_type, created_at, updated_at, auto_selected_vehicle_variant, oxygen, ventilator)
(select
    md5(random()::text || clock_timestamp()::text)::uuid,
    'Ambulance Taxi with Oxygen',
    m.merchant_id,
    m.id,
    null,
    null,
    null,
    null,
    null,
    'Ambulance Eco with O2',
    null,
    '{AMBULANCE_TAXI_OXY, AMBULANCE_AC, AMBULANCE_AC_OXY, AMBULANCE_VENTILATOR}',
    '{AMBULANCE_TAXI_OXY}',
    'AMBULANCE_TAXI_OXY',
    now(),
    now(),
    '{AMBULANCE_TAXI_OXY}',
    1,
    null
    from atlas_driver_offer_bpp.merchant_operating_city as m where city = 'Kolkata');

insert into atlas_driver_offer_bpp.vehicle_service_tier (id, name, merchant_id, merchant_operating_city_id, seating_capacity, air_conditioned, driver_rating, vehicle_rating, luggage_capacity, short_description, long_description, allowed_vehicle_variant, default_for_vehicle_variant, service_tier_type, created_at, updated_at, auto_selected_vehicle_variant, oxygen, ventilator)
(select
    md5(random()::text || clock_timestamp()::text)::uuid,
    'Ambulance with AC',
    m.merchant_id,
    m.id,
    null,
    1,
    null,
    null,
    null,
    'Ambulance',
    null,
    '{AMBULANCE_AC, AMBULANCE_AC_OXY, AMBULANCE_VENTILATOR}',
    '{AMBULANCE_AC}',
    'AMBULANCE_AC',
    now(),
    now(),
    '{AMBULANCE_AC}',
    null,
    null
    from atlas_driver_offer_bpp.merchant_operating_city as m where city = 'Kolkata');

insert into atlas_driver_offer_bpp.vehicle_service_tier (id, name, merchant_id, merchant_operating_city_id, seating_capacity, air_conditioned, driver_rating, vehicle_rating, luggage_capacity, short_description, long_description, allowed_vehicle_variant, default_for_vehicle_variant, service_tier_type, created_at, updated_at, auto_selected_vehicle_variant, oxygen, ventilator)
(select
    md5(random()::text || clock_timestamp()::text)::uuid,
    'Ambulance with AC and Oxygen',
    m.merchant_id,
    m.id,
    null,
    1,
    null,
    null,
    null,
    'Ambulance with AC and O2',
    null,
    '{AMBULANCE_AC_OXY, AMBULANCE_VENTILATOR}',
    '{AMBULANCE_AC_OXY}',
    'AMBULANCE_AC_OXY',
    now(),
    now(),
    '{AMBULANCE_AC_OXY}',
    1,
    null
    from atlas_driver_offer_bpp.merchant_operating_city as m where city = 'Kolkata');

insert into atlas_driver_offer_bpp.vehicle_service_tier (id, name, merchant_id, merchant_operating_city_id, seating_capacity, air_conditioned, driver_rating, vehicle_rating, luggage_capacity, short_description, long_description, allowed_vehicle_variant, default_for_vehicle_variant, service_tier_type, created_at, updated_at, auto_selected_vehicle_variant, oxygen, ventilator)
(select
    md5(random()::text || clock_timestamp()::text)::uuid,
    'Ambulance with Ventilator',
    m.merchant_id,
    m.id,
    null,
    1,
    null,
    null,
    null,
    'Ambulance with ventilator',
    null,
    '{AMBULANCE_VENTILATOR}',
    '{AMBULANCE_VENTILATOR}',
    'AMBULANCE_VENTILATOR',
    now(),
    now(),
    '{AMBULANCE_VENTILATOR}',
    1,
    1
    from atlas_driver_offer_bpp.merchant_operating_city as m where city = 'Kolkata');