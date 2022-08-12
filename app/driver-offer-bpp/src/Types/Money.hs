{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Money where

import Beckn.Prelude
import Beckn.Utils.GenericPretty
import Database.Persist.Class
import Database.Persist.Sql

newtype RoundedMoney = RoundedMoney Int
  deriving newtype (Show, PrettyShow, Enum, Eq, Ord, Num, Real, Integral, PersistField, PersistFieldSql, ToJSON, FromJSON, ToSchema)
