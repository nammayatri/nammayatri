UPDATE atlas_driver_offer_bpp.document_verification_config
SET supported_vehicle_classes_json = '[
  {
    "bodyType": null,
    "manufacturer": null,
    "manufacturerModel": null,
    "reviewRequired": false,
    "vehicleCapacity": null,
    "vehicleClass": "TOTO",
    "vehicleModel": null,
    "vehicleVariant": "E_RICKSHAW"
  },
  {
    "bodyType": null,
    "manufacturer": null,
    "manufacturerModel": null,
    "reviewRequired": false,
    "vehicleCapacity": null,
    "vehicleClass": "3WT",
    "vehicleModel": null,
    "vehicleVariant": "E_RICKSHAW"
  }
]'::jsonb,
check_expiry = false
WHERE document_type = 'VehicleRegistrationCertificate'
  AND vehicle_category = 'TOTO';