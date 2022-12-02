module Domain.Types.DriverOnboarding.OperatingCity where

import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.Merchant

data OperatingCity = OperatingCity
  { id :: Id OperatingCity,
    merchantId :: Id Merchant,
    cityName :: Text,
    enabled :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)
