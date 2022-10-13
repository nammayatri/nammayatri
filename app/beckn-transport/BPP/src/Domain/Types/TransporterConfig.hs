{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Domain.Types.TransporterConfig where

import Beckn.Types.Id
import Data.Time (UTCTime)
import Domain.Types.Common
import Domain.Types.Organization (Organization)
import EulerHS.Prelude hiding (id)

data TransporterConfigD u = TransporterConfig
  { id :: Id TransporterParameter,
    transporterId :: Id Organization,
    key :: ConfigKey,
    value :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

type TransporterConfig = TransporterConfigD 'Safe

instance FromJSON (TransporterConfigD 'Unsafe)

instance ToJSON (TransporterConfigD 'Unsafe)

data TransporterParameter

newtype ConfigKey = ConfigKey
  { getConfigKey :: Text
  }
  deriving stock (Generic)
  deriving newtype (Show, Read, FromJSON, ToJSON)
