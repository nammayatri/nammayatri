-- RUN IN MASTER
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
    'TRUCK' AS vehicle_category,
    vehicle_class_check_type,
    CURRENT_TIMESTAMP as created_at,
    CURRENT_TIMESTAMP as updated_at,
    is_default_enabled_on_manual_verification,
    is_image_validation_required
FROM
    atlas_driver_offer_bpp.document_verification_config
WHERE
    vehicle_category = 'CAR' AND merchant_id = (select merchant_id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata')
ON CONFLICT DO NOTHING;

-- RUN IN MASTER
UPDATE atlas_driver_offer_bpp.document_verification_config
SET supported_vehicle_classes_json = json_build_array('MGV', 'HMV', 'HGMV', 'HTV', 'LGV', 'HGV', 'HGV-TR', 'HGV-TRANS', 'HGVTR', 'HGVTRANS')
WHERE document_type = 'DriverLicense' AND vehicle_category = 'TRUCK' AND merchant_id = (select merchant_id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata');

-- ONLY IN MASTER
update atlas_driver_offer_bpp.document_verification_config
set supported_vehicle_classes_json =
    json_build_array(
        json_build_object('vehicleClass', 'MGV', 'vehicleVariant', 'DELIVERY_LIGHT_GOODS_VEHICLE'),
        json_build_object('vehicleClass', 'HMV', 'vehicleVariant', 'DELIVERY_LIGHT_GOODS_VEHICLE'),
        json_build_object('vehicleClass', '3WT', 'vehicleVariant', 'DELIVERY_LIGHT_GOODS_VEHICLE'),
        json_build_object('vehicleClass', 'HTV', 'vehicleVariant', 'DELIVERY_LIGHT_GOODS_VEHICLE'),
        json_build_object('vehicleClass', 'LGV', 'vehicleVariant', 'DELIVERY_LIGHT_GOODS_VEHICLE'),
        json_build_object('vehicleClass', 'HGV', 'vehicleVariant', 'DELIVERY_LIGHT_GOODS_VEHICLE'),
        json_build_object('vehicleClass', 'Three Wheeler (Goods)(3WT)', 'vehicleVariant', 'DELIVERY_LIGHT_GOODS_VEHICLE'),
        json_build_object('vehicleClass', 'Goods Carrier(HGV)', 'vehicleVariant', 'DELIVERY_LIGHT_GOODS_VEHICLE'),
        json_build_object('vehicleClass', 'Goods Carrier(LGV)', 'vehicleVariant', 'DELIVERY_LIGHT_GOODS_VEHICLE'),
        json_build_object('vehicleClass', 'Goods', 'vehicleVariant', 'DELIVERY_LIGHT_GOODS_VEHICLE'),
        json_build_object('vehicleClass', 'Three Wheeled Goods Vehicle', 'vehicleVariant', 'DELIVERY_LIGHT_GOODS_VEHICLE'),
        json_build_object('vehicleClass', 'e-Rickshaw with Cart (G)(3WT)', 'vehicleVariant', 'DELIVERY_LIGHT_GOODS_VEHICLE'),
        json_build_object('vehicleClass', 'Goods Carrier(MGV)', 'vehicleVariant', 'DELIVERY_LIGHT_GOODS_VEHICLE'),
        json_build_object('vehicleClass', 'Articulated Vehicle(HGV)', 'vehicleVariant', 'DELIVERY_LIGHT_GOODS_VEHICLE'),
        json_build_object('vehicleClass', 'Goods Carrier', 'vehicleVariant', 'DELIVERY_LIGHT_GOODS_VEHICLE')
    )
where document_type = 'VehicleRegistrationCertificate'
and vehicle_category = 'TRUCK' AND merchant_id = (select merchant_id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata');

-- INSERT INTO vehicle_service_tier
-- RUN IN MASTER
insert into atlas_driver_offer_bpp.vehicle_service_tier (id, name, merchant_id, merchant_operating_city_id, seating_capacity, air_conditioned, driver_rating, vehicle_rating, luggage_capacity, short_description, long_description, allowed_vehicle_variant, default_for_vehicle_variant, service_tier_type, created_at, updated_at, auto_selected_vehicle_variant)
(select
    md5(random()::text || clock_timestamp()::text)::uuid,
    'Truck',
    m.merchant_id,
    m.id,
    null,
    null,
    null,
    null,
    null,
    'Deliver Everything',
    null,
    '{DELIVERY_LIGHT_GOODS_VEHICLE}',
    '{DELIVERY_LIGHT_GOODS_VEHICLE}',
    'DELIVERY_LIGHT_GOODS_VEHICLE',
    now(),
    now(),
    '{DELIVERY_LIGHT_GOODS_VEHICLE}'
    from atlas_driver_offer_bpp.merchant_operating_city as m where city = 'Kolkata');
