CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.DriverDrivingLicense
(
    id character(36) COLLATE pg_catalog."default" NOT NULL,
    driverId character varying(36) COLLATE pg_catalog."default" NOT NULL,
    driverLicenseNumber character varying(255) COLLATE pg_catalog."default",
    driverLicenseStart timestamp with time zone NOT NULL,
    driverLicenseStatus character varying(10) NOT NULL,
    driverVerificationStatus character varying(10) NOT NULL,
    driverLicenseExpiry timestamp with time zone NOT NULL,
    classOfVehicle text[][] COLLATE pg_catalog."default",
    request_id character(36) COLLATE pg_catalog."default" NOT NULL,
    createdAt timestamp with time zone NOT NULL,
    updatedAt timestamp with time zone NOT NULL
    ,CONSTRAINT  DriverDrivingLicense_driver_id_fkey FOREIGN KEY (driverId) REFERENCES atlas_driver_offer_bpp.person(id)
);


-- data DriverDrivingLicense = DriverDrivingLicense {
--     id :: Id DriverDrivingLicense,
--     driverId :: Id Person,
--     driverLicenseNumber :: Maybe Text,
--     driverLicenseStart :: Maybe UTCTime,
--     driverLicenseStatus :: IdfyStatus,
--     driverVerificationStatus :: Maybe IdfyStatus,
--     driverLicenseExpiry :: Maybe UTCTime,
--     classOfVehicle :: [COV],
--     request_id :: Text,
--     createdAt :: UTCTime,
--     updatedAt :: UTCTime
-- }
--     deriving (Generic)