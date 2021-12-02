{-# LANGUAGE DerivingStrategies #-}

module Types.App where

import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude
import Servant

data Driver -- = Person

type Limit = Int

type Offset = Int

type MandatoryQueryParam name a = QueryParam' '[Required, Strict] name a

data SortMode
  = ETA
  | IdleTime
  deriving (Eq, Generic, FromDhall)

data Person