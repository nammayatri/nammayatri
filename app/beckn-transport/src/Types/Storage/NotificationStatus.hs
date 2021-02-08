{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.NotificationStatus where

import Beckn.Types.ID (ID)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Database.Beam as B
import Database.Beam.Backend.SQL (BeamSqlBackend, FromBackendRow, HasSqlValueSyntax (..), autoSqlValueSyntax, fromBackendRow)
import Database.Beam.Postgres (Postgres)
import EulerHS.Prelude
import Types.App (DriverId, RideId, RideRequestId)

data AnswerStatus = ACCEPTED | REJECTED | NOTIFIED | IGNORED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be AnswerStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres AnswerStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance BeamSqlBackend be => B.HasSqlEqualityCheck be AnswerStatus

data NotificationStatusT f = NotificationStatus
  { _id :: B.C f (ID NotificationStatus),
    _rideId :: B.C f RideId,
    _driverId :: B.C f DriverId,
    _rideRequestId :: B.C f RideRequestId,
    _status :: B.C f AnswerStatus,
    _notifiedAt :: B.C f (Maybe UTCTime)
  }
  deriving (Generic, B.Beamable)

type NotificationStatus = NotificationStatusT Identity

type NotificationStatusPrimaryKey = B.PrimaryKey NotificationStatusT Identity

instance B.Table NotificationStatusT where
  data PrimaryKey NotificationStatusT f = NotificationStatusPrimaryKey (B.C f (ID NotificationStatus))
    deriving (Generic, B.Beamable)
  primaryKey = NotificationStatusPrimaryKey . _id

instance ToJSON NotificationStatus where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON NotificationStatus where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity NotificationStatusT)
fieldEMod =
  B.setEntityName "notification_status"
    <> B.modifyTableFields
      B.tableModification
        { _rideId = "ride_id",
          _driverId = "driver_id",
          _rideRequestId = "ride_request_id",
          _notifiedAt = "notified_at"
        }
