{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Notification where

import qualified Database.Beam as B
import Database.Beam.Backend (BeamSqlBackend, FromBackendRow, HasSqlValueSyntax (sqlValueSyntax), autoSqlValueSyntax)
import Database.Beam.MySQL ()
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import GHC.Generics (Generic)
import qualified Kernel.External.Payment.Juspay.Types as Payment
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Payment.NotificationStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Payment.NotificationStatus

instance FromField Payment.NotificationStatus where
  fromField = fromFieldEnum

instance FromBackendRow Postgres Payment.NotificationStatus

deriving instance Ord Payment.NotificationStatus

data NotificationT f = NotificationT
  { id :: B.C f Text,
    shortId :: B.C f Text,
    sourceAmount :: B.C f HighPrecMoney,
    mandateId :: B.C f Text,
    driverFeeId :: B.C f Text,
    txnDate :: B.C f UTCTime,
    juspayProvidedId :: B.C f Text,
    providerName :: B.C f (Maybe Text),
    notificationType :: B.C f (Maybe Text),
    description :: B.C f Text,
    status :: B.C f Payment.NotificationStatus,
    dateCreated :: B.C f UTCTime,
    lastUpdated :: B.C f UTCTime,
    lastStatusCheckedAt :: B.C f (Maybe UTCTime),
    responseCode :: B.C f (Maybe Text),
    responseMessage :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table NotificationT where
  data PrimaryKey NotificationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Notification = NotificationT Identity

$(enableKVPG ''NotificationT ['id] []) -- DON'T Enable for KV

$(mkTableInstances ''NotificationT "notification")
