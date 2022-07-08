
module Types.API.Driveronboarding.VehicleRegistrationCert where
import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess)




data VehicleRegistrationCertReq = VehicleRegistrationCert {   
    organization_id :: Text,
    vehicleRegistrationCertNumber :: Maybe Text,
    operatingCity :: Text
}
    deriving (Generic)

type VehicleRegistrationCertRes = APISuccess  




