{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Types.Storage.TripReference where

import Beckn.Types.Id
import Beckn.Utils.JSON
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import EulerHS.Prelude hiding (id)
import Servant.API

data Status = NOT_STARTED | WAITING | ON_GOING | COMPLETED | CANCELLED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Status where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres Status where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToParamSchema Status

instance FromHttpApiData Status where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data TripReferenceT f = TripReference
  { id :: B.C f (Id TripReference),
    customerId :: B.C f Text,
    quotationId :: B.C f Text,
    driverId :: B.C f (Maybe Text),
    leadsId :: B.C f Text,
    vehicleId :: B.C f (Maybe Text),
    shortId :: B.C f Text,
    status :: B.C f Status,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type TripReference = TripReferenceT Identity

type TripReferencePrimaryKey = B.PrimaryKey TripReferenceT Identity

instance B.Table TripReferenceT where
  data PrimaryKey TripReferenceT f = TripReferencePrimaryKey (B.C f (Id TripReference))
    deriving (Generic, B.Beamable)
  primaryKey = TripReferencePrimaryKey . id

deriving instance Show TripReference

deriving instance Eq TripReference

instance ToJSON TripReference where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON TripReference where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToSchema TripReference

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity TripReferenceT)
fieldEMod =
  B.setEntityName "trip_reference"
    <> B.modifyTableFields
      B.tableModification
        { createdAt = "created_at",
          updatedAt = "updated_at",
          customerId = "customer_id",
          quotationId = "quotation_id",
          driverId = "driver_id",
          leadsId = "_booking_reference_id",
          vehicleId = "vehicle_id",
          shortId = "short_id"
        }
