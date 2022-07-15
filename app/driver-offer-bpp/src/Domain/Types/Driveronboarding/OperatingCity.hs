module Domain.Types.Driveronboarding.OperatingCity where

import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.Organization

data OperatingCityVerification = VALID | INPUT
  deriving
    ( Show,
      Eq,
      Read,
      Generic,
      ToJSON,
      FromJSON,
      ToSchema,
      ToParamSchema,
      Enum,
      Bounded
    )

data OperatingCity = OperatingCity
  { id :: Id OperatingCity,
    organizationId :: Id Organization,
    cityName :: Text,
    enabled :: OperatingCityVerification,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)
