{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.TripReference where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time.LocalTime
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import EulerHS.Prelude
import Servant.API
import Servant.Swagger
import Types.App

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
  { _id :: B.C f TripReferenceId,
    _customerId :: B.C f Text,
    _quotationId :: B.C f Text,
    _driverId :: B.C f (Maybe Text),
    _leadsId :: B.C f Text,
    _vehicleId :: B.C f (Maybe Text),
    _shortId :: B.C f Text,
    _status :: B.C f Status,
    _createdAt :: B.C f LocalTime,
    _updatedAt :: B.C f LocalTime
  }
  deriving (Generic, B.Beamable)

type TripReference = TripReferenceT Identity

type TripReferencePrimaryKey = B.PrimaryKey TripReferenceT Identity

instance B.Table TripReferenceT where
  data PrimaryKey TripReferenceT f = TripReferencePrimaryKey (B.C f TripReferenceId)
    deriving (Generic, B.Beamable)
  primaryKey = TripReferencePrimaryKey . _id

deriving instance Show TripReference

deriving instance Eq TripReference

instance ToJSON TripReference where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON TripReference where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToSchema TripReference

insertExpression org = B.insertValues [org]

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity TripReferenceT)
fieldEMod =
  B.setEntityName "trip_reference"
    <> B.modifyTableFields
      B.tableModification
        { _createdAt = "created_at",
          _updatedAt = "updated_at",
          _customerId = "customer_id",
          _quotationId = "quotation_id",
          _driverId = "driver_id",
          _leadsId = "_booking_reference_id",
          _vehicleId = "vehicle_id",
          _shortId = "short_id"
        }
