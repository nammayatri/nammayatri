
module Types.API.Driveronboarding.DriverOnBoarding where
import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess)
data DriverOnBoardingReq = DriverOnBoardingReq {
    organizationId :: Text,
    driverLicenseNumber :: Text,
    operatingCity :: Text,
    driverDateOfBirth :: UTCTime, 
    vehicleRegistrationCertNumber :: Text, 
    driverConsent :: Bool
}

    deriving (Generic,ToSchema,ToJSON,FromJSON)

type DriverOnBoardingRes = APISuccess  

-- add consentTimestamp