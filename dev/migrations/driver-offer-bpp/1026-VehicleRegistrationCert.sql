CREATE TABLE IF NOT EXISTS atlas_transporter.VehicleRegistrationCert
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
    request_id character(36) COLLATE pg_catalog."default" NOT NULL,
    createdAt timestamp with time zone NOT NULL,
    updatedAt timestamp with time zone NOT NULL
    ,CONSTRAINT  VehicleRegistrationCert_org_id_fkey FOREIGN KEY (driverId) REFERENCES atlas_transporter.organization(id)
);

-- data VehicleRegistrationCert = VehicleRegistrationCert {
--     id :: Id VehicleRegistrationCert,
--     driverId :: Id Person,
--     vehicleRegistrationCertNumber :: Maybe Text,
--     fitnessCertExpiry :: Maybe UTCTime,
--     permitNumber :: Maybe Text,
--     permitStart :: Maybe UTCTime,
--     permitExpiry :: Maybe UTCTime,
--     vehicleClass :: Maybe VehicleClass,
--     vehicleNumber :: Maybe Text,
--     request_id :: Text,
--     createdAt :: UTCTime,
--     updatedAt :: UTCTime
-- }

