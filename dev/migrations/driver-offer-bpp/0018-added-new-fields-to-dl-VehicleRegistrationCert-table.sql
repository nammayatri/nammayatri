ALTER TABLE atlas_driver_offer_bpp._driver_driving_license_t RENAME COLUMN request_id to idfy_request_id;
ALTER TABLE atlas_driver_offer_bpp._driver_driving_license_t ADD COLUMN dl_image1s3_path character (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp._driver_driving_license_t ADD COLUMN dl_image2s3_path character (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp._driver_driving_license_t ADD COLUMN verification_resp_dump character (36) NOT NULL;

ALTER TABLE atlas_driver_offer_bpp._vehicle_registration_cert_t RENAME COLUMN request_id to idfy_request_id;
ALTER TABLE atlas_driver_offer_bpp._vehicle_registration_cert_t ADD COLUMN puc_expiry timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp._vehicle_registration_cert_t ADD COLUMN vehicle_color character (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp._vehicle_registration_cert_t ADD COLUMN vehicle_model character (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp._vehicle_registration_cert_t ADD COLUMN vehicle_manufacturer character (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp._vehicle_registration_cert_t ADD COLUMN rc_images3_path character (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp._vehicle_registration_cert_t ADD COLUMN verification_resp_dump character (36) NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN verified boolean DEFAULT false NOT NULL;