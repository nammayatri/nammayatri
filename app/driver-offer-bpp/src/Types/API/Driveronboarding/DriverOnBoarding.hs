module Types.API.Driveronboarding.DriverOnBoarding where

import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Predicate
import Beckn.Types.Validation
import Beckn.Utils.Predicates
import Beckn.Utils.Validation
import Data.Time

data DriverOnBoardingReq = DriverOnBoardingReq
  { driverLicenseNumber :: Text,
    operatingCity :: Text,
    driverDateOfBirth :: UTCTime,
    vehicleRegistrationCertNumber :: Text
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

validateDriverOnBoardingReq :: UTCTime -> Validate DriverOnBoardingReq
validateDriverOnBoardingReq now DriverOnBoardingReq {..} =
  sequenceA_
    [ validateField "driverLicenseNumber" driverLicenseNumber licenseNum,
      validateField "driverDateOfBirth" driverDateOfBirth $ InRange @UTCTime t100YearsAgo t16YearsAgo,
      validateField "vehicleRegistrationCertNumber" vehicleRegistrationCertNumber certNum
    ]
  where
    licenseNum = MinLength 5 `And` star (latinUC \/ digit)
    certNum = LengthInRange 5 12 `And` star (latinUC \/ digit \/ ",")
    t16YearsAgo = yearsAgo 16
    t100YearsAgo = yearsAgo 100
    yearsAgo i = negate (nominalDay * 365 * i) `addUTCTime` now

type DriverOnBoardingRes = APISuccess
