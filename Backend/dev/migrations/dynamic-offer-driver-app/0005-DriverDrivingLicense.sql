CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp._driver_driving_license_t
(
    id character(36) NOT NULL,
    driver_id character varying(36) NOT NULL,
    driver_dob timestamp with time zone NOT NULL,
    driver_license_number character varying(255),
    driver_license_start timestamp with time zone,
    driver_license_expiry timestamp with time zone,
    class_of_vehicle text[][],
    idfy_status character varying(20) NOT NULL,
    verification_status character varying(10) NOT NULL,
    driver_verification_status character varying(10) NOT NULL,
    request_id character(36) NOT NULL,
    consent boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    consent_timestamp timestamp with time zone NOT NULL
    ,CONSTRAINT  DriverDrivingLicense_driver_id_fkey FOREIGN KEY (driver_id) REFERENCES atlas_driver_offer_bpp.person(id)
);

