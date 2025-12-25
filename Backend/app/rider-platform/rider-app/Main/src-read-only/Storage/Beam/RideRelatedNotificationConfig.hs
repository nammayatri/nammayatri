{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RideRelatedNotificationConfig where

import qualified Database.Beam as B
import qualified Domain.Types.BookingStatus
import Domain.Types.Common ()
import qualified Domain.Types.RideRelatedNotificationConfig
import qualified Domain.Types.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data RideRelatedNotificationConfigT f = RideRelatedNotificationConfigT
  { eventTime :: B.C f Domain.Types.RideRelatedNotificationConfig.EventTime,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    notificationKey :: B.C f Kernel.Prelude.Text,
    notificationType :: B.C f Domain.Types.RideRelatedNotificationConfig.NotificationType,
    onBookingStatus :: B.C f Domain.Types.BookingStatus.BookingStatus,
    onScheduledBooking :: B.C f Kernel.Prelude.Bool,
    timeDiff :: B.C f Kernel.Types.Common.Seconds,
    timeDiffEvent :: B.C f Domain.Types.RideRelatedNotificationConfig.TimeDiffEvent,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RideRelatedNotificationConfigT where
  data PrimaryKey RideRelatedNotificationConfigT f = RideRelatedNotificationConfigId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RideRelatedNotificationConfigId <$> id <*> merchantOperatingCityId

type RideRelatedNotificationConfig = RideRelatedNotificationConfigT Identity

$(enableKVPG ''RideRelatedNotificationConfigT ['id, 'merchantOperatingCityId] [])

$(mkTableInstances ''RideRelatedNotificationConfigT "ride_related_notification_config")

$(Domain.Types.UtilsTH.mkCacParseInstance ''RideRelatedNotificationConfigT)
