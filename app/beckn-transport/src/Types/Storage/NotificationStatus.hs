{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.NotificationStatus where

import Beckn.Storage.DB.Utils (fromBackendRowEnum)
import Beckn.Types.Id
import Data.Time (UTCTime)
import qualified Database.Beam as B
import Database.Beam.Backend.SQL (BeamSqlBackend, FromBackendRow, HasSqlValueSyntax (..), autoSqlValueSyntax, fromBackendRow)
import Database.Beam.Postgres (Postgres)
import EulerHS.Prelude hiding (id)
import Types.App
import qualified Types.Storage.RideBooking as SRB

data AnswerStatus = NOTIFIED | REJECTED | IGNORED | ACCEPTED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be AnswerStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres AnswerStatus where
  fromBackendRow = fromBackendRowEnum "AnswerStatus"

instance BeamSqlBackend be => B.HasSqlEqualityCheck be AnswerStatus

data NotificationStatusT f = NotificationStatus
  { id :: B.C f (Id NotificationStatus),
    rideBookingId :: B.C f (Id SRB.RideBooking),
    driverId :: B.C f (Id Driver),
    status :: B.C f AnswerStatus,
    expiresAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type NotificationStatus = NotificationStatusT Identity

type NotificationStatusPrimaryKey = B.PrimaryKey NotificationStatusT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table NotificationStatusT where
  data PrimaryKey NotificationStatusT f = NotificationStatusPrimaryKey (B.C f (Id NotificationStatus))
    deriving (Generic, B.Beamable)
  primaryKey = NotificationStatusPrimaryKey . id

deriving instance FromJSON NotificationStatus

deriving instance ToJSON NotificationStatus

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity NotificationStatusT)
fieldEMod =
  B.setEntityName "notification_status"
    <> B.modifyTableFields
      B.tableModification
        { rideBookingId = "ride_booking_id",
          driverId = "driver_id",
          expiresAt = "expires_at"
        }
