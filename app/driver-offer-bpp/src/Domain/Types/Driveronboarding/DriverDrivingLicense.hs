module Domain.Types.Driveronboarding.DriverDrivingLicense where
import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.Person (Person)
import Domain.Types.Driveronboarding.VehicleRegistrationCert


data Verification1 = PENDINGVERIFICATION | VERIFIED | FAILEDVERIFICATION | WAITINGINPUT deriving
    ( Show,
      Eq,
      Read,
      Generic,
      ToJSON,
      FromJSON,
      ToSchema,
      ToParamSchema,
      Enum,
      Bounded
    )


-- 3-T , - _



data DriverDrivingLicense = DriverDrivingLicense {
    id :: Id DriverDrivingLicense,
    driverId :: Id Person,
    driverLicenseNumber :: Maybe Text,
    driverLicenseStart :: Maybe UTCTime,
    driverLicenseStatus :: Verification1,
    driverVerificationStatus :: Maybe Verification1,
    driverLicenseExpiry :: Maybe UTCTime,
    classOfVehicle :: [VehicleClass],
    request_id :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
}
    deriving (Generic,ToSchema,ToJSON,FromJSON)
