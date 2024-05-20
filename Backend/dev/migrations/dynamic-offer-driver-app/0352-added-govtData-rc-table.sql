CREATE TABLE atlas_driver_offer_bpp.govt_data_r_c  (
    id VARCHAR(36) PRIMARY KEY,
    merchant_operating_city_id VARCHAR(36) NOT NULL,
    owner_serial_number TEXT,
    registration_number TEXT,
    manufacturer_model TEXT,
    permit_validity_from TEXT,
    permit_validity_upto TEXT,
    manufacturer TEXT,
    body_type TEXT,
    number_of_cylinder INT,
    fuel_type TEXT,
    seating_capacity INT,
    from_date TEXT,
    to_date TEXT,
    created_at timestamp with time zone NOT NULL DEFAULT now()
);

UPDATE atlas_driver_offer_bpp.onboarding_document_configs SET rc_number_prefix_list = '{"TS", "KA"}' ; -- need to check and run in master and prod accordingly

UPDATE atlas_driver_offer_bpp.onboarding_document_configs SET supported_vehicle_classes_json =
    json_build_array(json_build_object('vehicleClass', 'Hackney', 'vehicleCapacity', 4, 'vehicleVariant', 'AUTO_RICKSHAW', 'bodyType', 'Hackney'),
    json_build_object('vehicleClass', 'Monocoque', 'vehicleCapacity', 4, 'vehicleVariant', 'AUTO_RICKSHAW', 'bodyType', 'Monocoque')) where document_type = 'RC';   -- append this