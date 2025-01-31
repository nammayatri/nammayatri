UPDATE atlas_driver_offer_bpp.transporter_config
SET avg_speed_of_vehicle = (
    avg_speed_of_vehicle::jsonb || '{"heritagecab": 35, "evautorickshaw":25}'::jsonb
);

UPDATE atlas_driver_offer_bpp.document_verification_config
SET supported_vehicle_classes_json = (supported_vehicle_classes_json::jsonb || '[
    {   "bodyType": null,
        "manufacturer": null,
        "manufacturerModel": null,
        "priority": 5,
        "reviewRequired": false,
        "vehicleCapacity": null,
        "vehicleClass": "cab",
        "vehicleModel": "WagonR",
        "vehicleVariant": "HERITAGE_CAB"
    },
    {   "bodyType": null,
        "manufacturer": null,
        "manufacturerModel": null,
        "priority": 5,
        "reviewRequired": false,
        "vehicleCapacity": null,
        "vehicleClass": "lmv",
        "vehicleModel": "WagonR",
        "vehicleVariant": "HERITAGE_CAB"
    },
    {   "bodyType": null,
        "manufacturer": null,
        "manufacturerModel": null,
        "priority": 5,
        "reviewRequired": false,
        "vehicleCapacity": null,
        "vehicleClass": "lpv",
        "vehicleModel": "WagonR",
        "vehicleVariant": "HERITAGE_CAB"
    },
    {   "bodyType": null,
        "manufacturer": null,
        "manufacturerModel": null,
        "priority": 5,
        "reviewRequired": false,
        "vehicleCapacity": null,
        "vehicleClass": "light passenger vehicle",
        "vehicleModel": "WagonR",
        "vehicleVariant": "HERITAGE_CAB"
    },
    {   "bodyType": null,
        "manufacturer": null,
        "manufacturerModel": null,
        "priority": 5,
        "reviewRequired": false,
        "vehicleCapacity": null,
        "vehicleClass": "pcv",
        "vehicleModel": "WagonR",
        "vehicleVariant": "HERITAGE_CAB"
    }
    ]')::jsonb
WHERE document_type = 'VehicleRegistrationCertificate' and vehicle_category = 'CAR' and merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata');


-- Do not run in master --
insert into atlas_driver_offer_bpp.vehicle_service_tier (id, name, merchant_id, merchant_operating_city_id, seating_capacity, air_conditioned, driver_rating, vehicle_rating, luggage_capacity, short_description, long_description, allowed_vehicle_variant, default_for_vehicle_variant, service_tier_type, created_at, updated_at, auto_selected_vehicle_variant, oxygen, ventilator, is_air_conditioned, air_conditioned_threshold)
(select
    atlas_driver_offer_bpp.uuid_generate_v4(),
    'Heritage Cab',
    m.merchant_id,
    m.id,
    4,
    2,
    null,
    null,
    null,
    'Signature Cabs',
    null,
    '{TAXI, HATCHBACK, HERITAGE_CAB}',
    '{HERITAGE_CAB}',
    'HERITAGE_CAB',
    now(),
    now(),
    '{HATCHBACK, HERITAGE_CAB}',
    null,
    null,
    true,
    2
    from atlas_driver_offer_bpp.merchant_operating_city as m where city = 'Kolkata');