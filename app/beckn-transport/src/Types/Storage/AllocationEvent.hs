{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.AllocationEvent where

import Beckn.Types.Id (Id)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Database.Beam as B
import Database.Beam.Backend.SQL (BeamSqlBackend, FromBackendRow, HasSqlValueSyntax (..), autoSqlValueSyntax, fromBackendRow)
import Database.Beam.Postgres (Postgres)
import EulerHS.Prelude
import Types.App (Ride)

data AllocationEventT f = AllocationEvent
  { _id :: B.C f (Id AllocationEvent),
    _eventType :: B.C f AllocationEventType,
    _timestamp :: B.C f UTCTime,
    _rideId :: B.C f (Id Ride)
  }
  deriving (Generic, B.Beamable)

type AllocationEvent = AllocationEventT Identity

data AllocationEventType
  = NotificationSent
  | AcceptedByDriver
  | RejectedByDriver
  | IgnoredByDriver
  | ConsumerCancelled
  | EmptyDriverPool
  | AllocationTimeFinished
  deriving (Show, Eq, Read, Generic, FromJSON, ToJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be AllocationEventType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres AllocationEventType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance BeamSqlBackend be => B.HasSqlEqualityCheck be AllocationEventType

type AllocationEventPrimaryKey = B.PrimaryKey AllocationEventT Identity

instance B.Table AllocationEventT where
  data PrimaryKey AllocationEventT f = AllocationEventPrimaryKey (B.C f (Id AllocationEvent))
    deriving (Generic, B.Beamable)
  primaryKey = AllocationEventPrimaryKey . _id

instance ToJSON AllocationEvent where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON AllocationEvent where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity AllocationEventT)
fieldEMod =
  B.setEntityName "allocation_event"
    <> B.modifyTableFields
      B.tableModification
        { _id = "id",
          _eventType = "event_type",
          _timestamp = "timestamp",
          _rideId = "ride_id"
        }
