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
