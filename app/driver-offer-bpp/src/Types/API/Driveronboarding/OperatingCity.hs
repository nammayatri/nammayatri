
module Types.API.Driveronboarding.OperatingCity where
import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess)



data OperatingCityReq = OperatingCityReq{
    organization_id :: Text,
    vehicleRegistrationCertNumber :: Maybe Text,
    driverLicenseNumber :: Maybe Text,
    operatingCity :: Text
}   
    deriving (Generic,ToSchema,ToJSON,FromJSON)
type OperatingCityRes = APISuccess