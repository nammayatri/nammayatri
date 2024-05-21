UPDATE atlas_driver_offer_bpp.image set verification_status = 'VALID' where is_valid = true and (image_type = 'DriverLicense' OR image_type = 'VehicleRegistrationCertificate');

UPDATE atlas_driver_offer_bpp.image set verification_status = 'MANUAL_VERIFICATION_REQUIRED' where is_valid = true and image_type != 'DriverLicense' and image_type != 'VehicleRegistrationCertificate';

UPDATE atlas_driver_offer_bpp.image set verification_status = 'INVALID' where is_valid = false;