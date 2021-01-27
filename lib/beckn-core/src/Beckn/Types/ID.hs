{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Beckn.Types.ID where

import qualified Data.Text as Text
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Query
import EulerHS.Prelude

newtype ID domain = ID Text
  deriving stock (Show, Eq)
  deriving newtype (ToJSON, FromJSON)

getId :: ID a -> Text
getId (ID a) = a

instance IsString (ID d) where
  fromString = ID . Text.pack

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be (ID a) where
  sqlValueSyntax = sqlValueSyntax . getId

instance FromBackendRow Postgres (ID a) where
  fromBackendRow = ID <$> fromBackendRow

instance BeamSqlBackend be => HasSqlEqualityCheck be (ID a)
