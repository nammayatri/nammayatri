module Domain.Types.Driveronboarding.OperatingCity where

import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.Organization

data OperatingCity = OperatingCity
  { id :: Id OperatingCity,
    organizationId :: Id Organization,
    cityName :: Text,
    enabled :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)
