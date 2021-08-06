{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.NotificationStatus where

import Beckn.Types.Id
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Database.Beam as B
import Database.Beam.Backend.SQL (BeamSqlBackend, FromBackendRow, HasSqlValueSyntax (..), autoSqlValueSyntax, fromBackendRow)
import Database.Beam.Postgres (Postgres)
import EulerHS.Prelude hiding (id)
import Types.App

data AnswerStatus = NOTIFIED | REJECTED | IGNORED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be AnswerStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres AnswerStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance BeamSqlBackend be => B.HasSqlEqualityCheck be AnswerStatus

data NotificationStatusT f = NotificationStatus
  { id :: B.C f (Id NotificationStatus),
    rideId :: B.C f (Id Ride),
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
        { rideId = "ride_id",
          driverId = "driver_id",
          expiresAt = "expires_at"
        }
