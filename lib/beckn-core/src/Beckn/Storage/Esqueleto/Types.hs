{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Beckn.Storage.Esqueleto.Types where

import Data.OpenApi (ToSchema)
import Database.Esqueleto.Experimental
import EulerHS.Prelude hiding (Key)

data Point = Point
  deriving (Generic, Show, Read, Eq, ToSchema)

instance PersistField Point where
  toPersistValue _ = error "This value should not be used in queries directly."
  fromPersistValue _ = return Point

instance PersistFieldSql Point where
  sqlType _ = SqlOther "geography"
