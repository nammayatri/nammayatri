module Types.API.DriverOnboarding.DriverLicense where

import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Predicate
import Beckn.Types.Validation
import Beckn.Utils.Predicates
import Beckn.Utils.Validation
import Data.Time

newtype DriverDLImageReq = DriverDLImageReq
  {image :: Text}
  deriving (Generic, ToSchema, ToJSON, FromJSON)

data DriverDLReq = DriverDLReq
  { driverLicenseNumber :: Text,
    operatingCity :: Text,
    driverDateOfBirth :: UTCTime
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

validateDriverDLReq :: UTCTime -> Validate DriverDLReq
validateDriverDLReq now DriverDLReq {..} =
  sequenceA_
    [ validateField "driverLicenseNumber" driverLicenseNumber licenseNum,
      validateField "driverDateOfBirth" driverDateOfBirth $ InRange @UTCTime t100YearsAgo t16YearsAgo
    ]
  where
    licenseNum = MinLength 5 `And` star (latinUC \/ digit)
    t16YearsAgo = yearsAgo 16
    t100YearsAgo = yearsAgo 100
    yearsAgo i = negate (nominalDay * 365 * i) `addUTCTime` now

type DriverDLRes = APISuccess
