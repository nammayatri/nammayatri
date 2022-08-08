module Types.API.DriverOnboarding.VehicleRegistrationCertificate where

import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Predicate
import Beckn.Types.Validation
import Beckn.Utils.Predicates
import Beckn.Utils.Validation

newtype DriverRCImageReq = DriverRCImageReq
  {image :: Text}
  deriving (Generic, ToSchema, ToJSON, FromJSON)

data DriverRCReq = DriverRCReq
  { vehicleRegistrationCertNumber :: Text,
    operatingCity :: Text
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

validateDriverRCReq :: Validate DriverRCReq
validateDriverRCReq DriverRCReq {..} =
  sequenceA_
    [validateField "vehicleRegistrationCertNumber" vehicleRegistrationCertNumber certNum]
  where
    certNum = LengthInRange 5 12 `And` star (latinUC \/ digit \/ ",")

type DriverRCRes = APISuccess
