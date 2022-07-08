CREATE TABLE IF NOT EXISTS atlas_transporter.DriverDrivingLicense
(
    id character(36) COLLATE pg_catalog."default" NOT NULL,
    driverId character varying(36) COLLATE pg_catalog."default" NOT NULL,
    driverLicenseNumber character varying(255) COLLATE pg_catalog."default",
    driverLicenseStart timestamp with time zone NOT NULL,
    driverLicenseStatus character varying(10) NOT NULL,
    driverVerificationStatus character varying(10) NOT NULL,
    driverLicenseExpiry timestamp with time zone NOT NULL,
    classOfVehicle text[][] COLLATE pg_catalog."default",
    createdAt timestamp with time zone NOT NULL,
    updatedAt timestamp with time zone NOT NULL
);


-- data DriverDrivingLicense = DriverDrivingLicense {
--     id :: Id DriverDrivingLicense,
--     driverId :: Id Person,
--     driverLicenseNumber :: Maybe Text,
--     driverLicenseStart :: Maybe UTCTime,
--     driverLicenseStatus :: DLStatus,
--     driverVerificationStatus :: Maybe DriverLicenseStatus,
--     driverLicenseExpiry :: Maybe UTCTime,
--     classOfVehicle :: [VehicleClass],
--     request_id :: Text,
--     createdAt :: UTCTime,
--     updatedAt :: UTCTime
-- }
--     deriving (Generic)