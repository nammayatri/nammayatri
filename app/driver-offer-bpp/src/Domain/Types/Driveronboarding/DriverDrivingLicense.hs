module Domain.Types.Driveronboarding.DriverDrivingLicense where
import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.Person (Person)
import Domain.Types.Driveronboarding.VehicleRegistrationCert

data DriverDrivingLicense = DriverDrivingLicense {
    id :: Id DriverDrivingLicense,
    driverId :: Id Person,
    driverLicenseNumber :: Maybe Text,
    driverLicenseStart :: Maybe UTCTime,
    driverLicenseStatus :: IdfyStatus,
    driverVerificationStatus :: Maybe IdfyStatus,
    driverLicenseExpiry :: Maybe UTCTime,
    classOfVehicle :: [COV],
    request_id :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
}
    deriving (Generic,ToSchema,ToJSON,FromJSON)
