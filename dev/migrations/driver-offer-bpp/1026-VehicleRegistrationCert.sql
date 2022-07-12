CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.VehicleRegistrationCert
(
    id character(36) COLLATE pg_catalog."default" NOT NULL,
    driver_id character varying(255) COLLATE pg_catalog."default" NOT NULL,
    vehicle_registration_certNumber character varying(255) COLLATE pg_catalog."default",
    fitness_cert_expiry  timestamp with time zone NOT NULL,
    permit_number character varying(255) COLLATE pg_catalog."default",
    permit_start timestamp with time zone NOT NULL,
    permit_expiry timestamp with time zone NOT NULL,
    vehicle_class character(36) COLLATE pg_catalog."default",
    vehicle_cumber character(36) COLLATE pg_catalog."default",
    insurance_validity timestamp with time zone NOT NULL,
    request_id character(36) COLLATE pg_catalog."default" NOT NULL,
    created_at timestamp with time zone NOT NULL,
    rc_status character varying(10) NOT NULL,
    updated_at timestamp with time zone NOT NULL
    ,CONSTRAINT  VehicleRegistrationCert_driver_id_fkey FOREIGN KEY (driver_id) REFERENCES atlas_driver_offer_bpp.person(id)
);

-- data VehicleRegistrationCert = VehicleRegistrationCert {
--     id :: Id VehicleRegistrationCert,
--     driverId :: Id Person,
--     vehicleRegistrationCertNumber :: Maybe Text,
--     fitnessCertExpiry :: Maybe UTCTime,
--     permitNumber :: Maybe Text,
--     permitStart :: Maybe UTCTime,
--     permitExpiry :: Maybe UTCTime,
--     vehicleClass :: Maybe COV,
--     vehicleNumber :: Maybe Text,
--     insuranceValidity :: Maybe UTCTime,
--     request_id :: Text,
--     createdAt :: UTCTime,
--     rcStatus :: IdfyStatus,
--     updatedAt :: UTCTime
-- }
