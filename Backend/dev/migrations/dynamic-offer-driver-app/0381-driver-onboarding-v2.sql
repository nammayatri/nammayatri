update atlas_driver_offer_bpp.document_verification_config set supported_vehicle_classes_json = json_build_array() where document_type not in ('VehicleRegistrationCertificate', 'DriverLicense');
