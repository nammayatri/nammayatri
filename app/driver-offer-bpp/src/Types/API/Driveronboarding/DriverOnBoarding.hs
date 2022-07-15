module Types.API.Driveronboarding.DriverOnBoarding where

import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess)

data DriverOnBoardingReq = DriverOnBoardingReq
  { driverLicenseNumber :: Text,
    operatingCity :: Text,
    driverDateOfBirth :: UTCTime,
    vehicleRegistrationCertNumber :: Text
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

type DriverOnBoardingRes = APISuccess
