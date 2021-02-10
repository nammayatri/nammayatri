{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.RideRequest where

import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Database.Beam as B
import Database.Beam.Backend.SQL (BeamSqlBackend, FromBackendRow, HasSqlValueSyntax (..), autoSqlValueSyntax, fromBackendRow)
import Database.Beam.Postgres (Postgres)
import EulerHS.Prelude
import Types.App (RideId, RideRequestId)

data AllocationStatus = NEW | COMPLETED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be AllocationStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres AllocationStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance BeamSqlBackend be => B.HasSqlEqualityCheck be AllocationStatus

data RideRequestType = ALLOCATION | CANCELLATION
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be RideRequestType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres RideRequestType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance BeamSqlBackend be => B.HasSqlEqualityCheck be RideRequestType

data RideRequestT f = RideRequest
  { _id :: B.C f RideRequestId,
    _rideId :: B.C f RideId,
    _orderedAt :: B.C f (Maybe UTCTime),
    _requestTime :: B.C f UTCTime,
    _type :: B.C f RideRequestType,
    _status :: B.C f AllocationStatus
  }
  deriving (Generic, B.Beamable)

type RideRequest = RideRequestT Identity

type RideRequestPrimaryKey = B.PrimaryKey RideRequestT Identity

instance B.Table RideRequestT where
  data PrimaryKey RideRequestT f = RideRequestPrimaryKey (B.C f RideId)
    deriving (Generic, B.Beamable)
  primaryKey = RideRequestPrimaryKey . _rideId

instance ToJSON RideRequest where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON RideRequest where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity RideRequestT)
fieldEMod =
  B.setEntityName "ride_request"
    <> B.modifyTableFields
      B.tableModification
        { _rideId = "ride_id",
          _orderedAt = "ordered_at",
          _requestTime = "request_time"
        }
