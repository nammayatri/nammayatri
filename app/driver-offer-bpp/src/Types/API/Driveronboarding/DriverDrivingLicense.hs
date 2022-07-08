module Types.API.Driveronboarding.DriverDrivingLicense where
import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess)



 

data DriverDrivingLicenseReq = DriverDrivingLicenseReq {   
    organization_id :: Text,
    driverLicenseNumber :: Maybe Text,
    operatingCity :: Text
}
    deriving (Generic)

type DriverDrivingLicenseRes = APISuccess  





