{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.AllocationEvent where

import Beckn.Types.Id (Id)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Database.Beam as B
import Database.Beam.Backend.SQL (BeamSqlBackend, FromBackendRow, HasSqlValueSyntax (..), autoSqlValueSyntax, fromBackendRow)
import Database.Beam.Postgres (Postgres)
import EulerHS.Prelude hiding (id)
import Types.App (Driver)
import qualified Types.Storage.RideBooking as SRB

data AllocationEventT f = AllocationEvent
  { id :: B.C f (Id AllocationEvent),
    driverId :: B.C f (Maybe (Id Driver)),
    eventType :: B.C f AllocationEventType,
    timestamp :: B.C f UTCTime,
    rideBookingId :: B.C f (Id SRB.RideBooking)
  }
  deriving (Generic, B.Beamable)

type AllocationEvent = AllocationEventT Identity

data AllocationEventType
  = NotificationSent
  | MarkedAsAccepted
  | MarkedAsRejected
  | MarkedAsIgnored
  | AcceptedByDriver
  | RejectedByDriver
  | ConsumerCancelled
  | EmptyDriverPool
  | AllocationTimeFinished
  deriving (Show, Eq, Read, Generic, FromJSON, ToJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be AllocationEventType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres AllocationEventType where
  fromBackendRow = do
    str <- T.unpack <$> fromBackendRow
    case readMaybe str of
      Nothing -> fail $ "failed to parse AllocationEventType; invalid value: " ++ str
      Just val -> pure val

instance BeamSqlBackend be => B.HasSqlEqualityCheck be AllocationEventType

type AllocationEventPrimaryKey = B.PrimaryKey AllocationEventT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table AllocationEventT where
  data PrimaryKey AllocationEventT f = AllocationEventPrimaryKey (B.C f (Id AllocationEvent))
    deriving (Generic, B.Beamable)
  primaryKey = AllocationEventPrimaryKey . id

deriving instance FromJSON AllocationEvent

deriving instance ToJSON AllocationEvent

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity AllocationEventT)
fieldEMod =
  B.setEntityName "allocation_event"
    <> B.modifyTableFields
      B.tableModification
        { id = "id",
          driverId = "driver_id",
          eventType = "event_type",
          timestamp = "timestamp",
          rideBookingId = "ride_booking_id"
        }
