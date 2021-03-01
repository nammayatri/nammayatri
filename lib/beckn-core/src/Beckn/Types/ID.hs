{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Beckn.Types.ID where

import Beckn.Types.Common (GuidLike (..))
import Data.Swagger
import qualified Data.Text as Text
import Database.Beam.Backend.SQL
  ( BeamSqlBackend,
    FromBackendRow (fromBackendRow),
    HasSqlValueSyntax (..),
  )
import Database.Beam.Postgres (Postgres)
import Database.Beam.Query (HasSqlEqualityCheck)
import EulerHS.Prelude
import Servant (FromHttpApiData (parseUrlPiece))

newtype ID domain = ID
  {getId :: Text}
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON, ToSchema)

cast :: ID a -> ID b
cast = ID . getId

instance IsString (ID d) where
  fromString = ID . Text.pack

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be (ID a) where
  sqlValueSyntax = sqlValueSyntax . getId

instance FromBackendRow Postgres (ID a) where
  fromBackendRow = ID <$> fromBackendRow

instance BeamSqlBackend be => HasSqlEqualityCheck be (ID a)

instance FromHttpApiData (ID a) where
  parseUrlPiece = pure . ID

instance GuidLike (ID a) where
  generateGUID = ID <$> generateGUID
