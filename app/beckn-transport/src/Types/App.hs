{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.App where

import Beckn.Types.Storage.Person (Person)
import Beckn.Types.Storage.ProductInstance (ProductInstance)
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.TH
import EulerHS.Prelude
import Servant

newtype ConfigKey = ConfigKey
  { _getConfigKey :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''ConfigKey

type Ride = ProductInstance

type Driver = Person

type Limit = Int

type Offset = Int

type MandatoryQueryParam name a = QueryParam' '[Required, Strict] name a

data SortMode
  = ETA
  | IdleTime
  deriving (Eq, Generic, FromDhall)
