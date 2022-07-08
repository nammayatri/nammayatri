module Domain.Types.Driveronboarding.DriverDrivingLicense where
import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.Person (Person)
import Domain.Types.Driveronboarding.VehicleRegistrationCert


data DLStatus = COMPLETED | IN_PROGRESS | FAILED  deriving (Show,Eq,Read,Generic,ToJSON,FromJSON,Enum,Bounded,ToSchema)


-- 3-T , - _

data DriverLicenseStatus = Completed | In_Progress | Failed deriving (Show,Eq,Read,Generic,ToJSON,FromJSON,Enum,Bounded,ToSchema)


data DriverDrivingLicense = DriverDrivingLicense {
    id :: Id DriverDrivingLicense,
    driverId :: Id Person,
    driverLicenseNumber :: Maybe Text,
    driverLicenseStart :: Maybe UTCTime,
    driverLicenseStatus :: DLStatus,
    driverVerificationStatus :: Maybe DriverLicenseStatus,
    driverLicenseExpiry :: Maybe UTCTime,
    classOfVehicle :: [VehicleClass],
    request_id :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
}
    deriving (Generic)
