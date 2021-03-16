{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Beckn.Types.Id where

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
import Servant (FromHttpApiData (parseUrlPiece), ToHttpApiData)

newtype Id domain = Id
  {getId :: Text}
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON, ToSchema, ToHttpApiData)

cast :: Id a -> Id b
cast = Id . getId

instance IsString (Id d) where
  fromString = Id . Text.pack

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be (Id a) where
  sqlValueSyntax = sqlValueSyntax . getId

instance FromBackendRow Postgres (Id a) where
  fromBackendRow = Id <$> fromBackendRow

instance BeamSqlBackend be => HasSqlEqualityCheck be (Id a)

instance FromHttpApiData (Id a) where
  parseUrlPiece = pure . Id

instance GuidLike (Id a) where
  generateGUID = Id <$> generateGUID

newtype ShortId domain = ShortId
  { getShortId :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving newtype (ToJSON, FromJSON, ToSchema, ToHttpApiData)

instance IsString (ShortId d) where
  fromString = ShortId . Text.pack

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be (ShortId a) where
  sqlValueSyntax = sqlValueSyntax . getShortId

instance FromBackendRow Postgres (ShortId a) where
  fromBackendRow = ShortId <$> fromBackendRow

instance BeamSqlBackend be => HasSqlEqualityCheck be (ShortId a)

instance FromHttpApiData (ShortId a) where
  parseUrlPiece = pure . ShortId

instance GuidLike (ShortId a) where
  generateGUID = ShortId <$> generateGUID