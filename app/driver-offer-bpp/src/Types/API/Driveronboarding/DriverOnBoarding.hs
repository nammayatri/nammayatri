
module Types.API.Driveronboarding.DriverOnBoarding where
import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess)
data DriverOnBoardingReq = DriverOnBoardingReq {
    organizationId :: Text,
    driverLicenseNumber :: Maybe Text,
    operatingCity :: Text,
    driverDateOfBirth :: UTCTime,
    vehicleRegistrationCertNumber :: Maybe Text
}

    deriving (Generic,ToSchema,ToJSON,FromJSON)

type DriverOnBoardingRes = APISuccess  




