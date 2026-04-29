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
);   -- append this