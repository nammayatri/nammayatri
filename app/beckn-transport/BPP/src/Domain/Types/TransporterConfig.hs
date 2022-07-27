module Domain.Types.TransporterConfig where

import Beckn.Types.Id
import Data.Time (UTCTime)
import Domain.Types.Organization (Organization)
import EulerHS.Prelude hiding (id)

data TransporterConfig = TransporterConfig
  { id :: Id TransporterParameter,
    transporterId :: Id Organization,
    key :: ConfigKey,
    value :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

data TransporterParameter

newtype ConfigKey = ConfigKey
  { getConfigKey :: Text
  }
  deriving (Generic, Show, Read)
