ALTER TABLE atlas_driver_offer_bpp.driver_license ADD CONSTRAINT unique_number UNIQUE (license_number_hash);
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate ADD CONSTRAINT unique_rc_id UNIQUE (certificate_number_hash, fitness_expiry);
