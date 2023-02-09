
ALTER TABLE atlas_driver_offer_bpp._driver_driving_license_t RENAME TO driver_license;
ALTER TABLE atlas_driver_offer_bpp._vehicle_registration_cert_t RENAME TO vehicle_registration_certificate;


ALTER TABLE atlas_driver_offer_bpp.driver_license RENAME COLUMN driver_license_number to license_number;
ALTER TABLE atlas_driver_offer_bpp.driver_license RENAME COLUMN driver_license_start to license_start;
ALTER TABLE atlas_driver_offer_bpp.driver_license RENAME COLUMN driver_license_expiry to license_expiry;
ALTER TABLE atlas_driver_offer_bpp.driver_license RENAME COLUMN class_of_vehicle to class_of_vehicles;
ALTER TABLE atlas_driver_offer_bpp.driver_license RENAME COLUMN request_id to idfy_request_id;

ALTER TABLE atlas_driver_offer_bpp.driver_license ALTER COLUMN idfy_request_id DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_license ALTER COLUMN driver_dob DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_license DROP COLUMN idfy_status;
ALTER TABLE atlas_driver_offer_bpp.driver_license DROP COLUMN driver_verification_status;

ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN image_s3_path character (255) DEFAULT '' NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN idfy_response_dump character (255);
ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN verification_try_count int8;

ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate RENAME COLUMN vehicle_registration_cert_number to certificate_number;
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate RENAME COLUMN fitness_cert_expiry to fitness_expiry;
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate RENAME COLUMN request_id to idfy_request_id;

ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate ALTER COLUMN idfy_request_id DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate DROP COLUMN idfy_status;
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate DROP COLUMN vehicle_number;

ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate ADD COLUMN puc_expiry timestamp with time zone DEFAULT CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate ADD COLUMN vehicle_color character (36);
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate ADD COLUMN vehicle_model character (36);
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate ADD COLUMN vehicle_manufacturer character (36);
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate ADD COLUMN image_s3_path character (255) DEFAULT '' NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate ADD COLUMN idfy_response_dump character (255);
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate ADD COLUMN verification_try_count int8;

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN verified boolean DEFAULT false NOT NULL;