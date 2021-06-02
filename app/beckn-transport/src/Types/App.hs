{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.App where

import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.TH
import EulerHS.Prelude
import Servant

newtype ConfigKey = ConfigKey
  { getConfigKey :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''ConfigKey

data Ride -- = ProductInstance

data Driver -- = Person

type Limit = Int

type Offset = Int

type MandatoryQueryParam name a = QueryParam' '[Required, Strict] name a

data SortMode
  = ETA
  | IdleTime
  deriving (Eq, Generic, FromDhall)
