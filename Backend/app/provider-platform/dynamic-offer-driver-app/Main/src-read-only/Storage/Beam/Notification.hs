{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Notification where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Extra.Notification
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data NotificationT f = NotificationT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    dateCreated :: B.C f Kernel.Prelude.UTCTime,
    description :: B.C f Kernel.Prelude.Text,
    driverFeeId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    juspayProvidedId :: B.C f Kernel.Prelude.Text,
    lastStatusCheckedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    lastUpdated :: B.C f Kernel.Prelude.UTCTime,
    mandateId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    notificationType :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    providerName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    responseCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    responseMessage :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    shortId :: B.C f Kernel.Prelude.Text,
    sourceAmount :: B.C f Kernel.Types.Common.HighPrecMoney,
    status :: B.C f Domain.Types.Extra.Notification.NotificationStatus,
    txnDate :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table NotificationT where
  data PrimaryKey NotificationT f = NotificationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = NotificationId . id

type Notification = NotificationT Identity

$(enableKVPG ''NotificationT ['id] [])

$(mkTableInstances ''NotificationT "notification")
