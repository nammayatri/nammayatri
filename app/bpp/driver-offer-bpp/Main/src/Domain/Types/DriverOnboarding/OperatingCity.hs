module Domain.Types.DriverOnboarding.OperatingCity where

import Domain.Types.Merchant
import Kernel.Prelude
import Kernel.Types.Id

data OperatingCity = OperatingCity
  { id :: Id OperatingCity,
    merchantId :: Id Merchant,
    cityName :: Text,
    enabled :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)
