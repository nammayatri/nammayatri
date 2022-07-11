CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.VehicleRegistrationCert
(
    id character(36) COLLATE pg_catalog."default" NOT NULL,
    driverId character varying(255) COLLATE pg_catalog."default" NOT NULL,
    vehicleRegistrationCertNumber character varying(255) COLLATE pg_catalog."default",
    fitnessCertExpiry  timestamp with time zone NOT NULL,
    permitNumber character varying(255) COLLATE pg_catalog."default",
    permitStart timestamp with time zone NOT NULL,
    permitExpiry timestamp with time zone NOT NULL,
    vehicleClass character(36) COLLATE pg_catalog."default",
    vehicleNumber character(36) COLLATE pg_catalog."default",
    insuranceValidity timestamp with time zone NOT NULL,
    request_id character(36) COLLATE pg_catalog."default" NOT NULL,
    createdAt timestamp with time zone NOT NULL,
    rcStatus character varying(10) NOT NULL,
    updatedAt timestamp with time zone NOT NULL
    ,CONSTRAINT  VehicleRegistrationCert_driver_id_fkey FOREIGN KEY (driverId) REFERENCES atlas_driver_offer_bpp.person(id)
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
