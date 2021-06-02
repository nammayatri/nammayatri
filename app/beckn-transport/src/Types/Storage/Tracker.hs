{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Types.Storage.Tracker where

import Beckn.Types.Id
import Beckn.Utils.JSON
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import EulerHS.Prelude hiding (Type, id)
import Servant.API

data Type = DRIVER | CUSTOMER | TRIP
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Type where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres Type where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToParamSchema Type

instance FromHttpApiData Type where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data TrackerT f = Tracker
  { id :: B.C f (Id Tracker),
    _type :: B.C f Type,
    referenceId :: B.C f Text,
    long :: B.C f Text,
    lat :: B.C f Text,
    gps :: B.C f Text,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type Tracker = TrackerT Identity

type TrackerPrimaryKey = B.PrimaryKey TrackerT Identity

instance B.Table TrackerT where
  data PrimaryKey TrackerT f = TrackerPrimaryKey (B.C f (Id Tracker))
    deriving (Generic, B.Beamable)
  primaryKey = TrackerPrimaryKey . id

deriving instance Show Tracker

deriving instance Eq Tracker

instance ToJSON Tracker where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON Tracker where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToSchema Tracker

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity TrackerT)
fieldEMod =
  B.setEntityName "trip_reference"
    <> B.modifyTableFields
      B.tableModification
        { createdAt = "created_at",
          updatedAt = "updated_at",
          referenceId = "reference_id"
        }
