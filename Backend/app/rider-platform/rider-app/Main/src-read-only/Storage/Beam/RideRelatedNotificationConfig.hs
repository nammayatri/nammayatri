{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RideRelatedNotificationConfig where

import qualified Database.Beam as B
import qualified Domain.Types.Extra.Booking
import qualified Domain.Types.RideRelatedNotificationConfig
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data RideRelatedNotificationConfigT f = RideRelatedNotificationConfigT
  { id :: B.C f Kernel.Prelude.Text,
    timeDiff :: B.C f Kernel.Types.Common.Seconds,
    timeDiffEvent :: B.C f Domain.Types.RideRelatedNotificationConfig.TimeDiffEvent,
    onBookingStatus :: B.C f Domain.Types.Extra.Booking.BookingStatus,
    notificationType :: B.C f Domain.Types.RideRelatedNotificationConfig.NotificationType,
    notificationKey :: B.C f Kernel.Prelude.Text,
    onScheduledBooking :: B.C f Kernel.Prelude.Bool,
    eventTime :: B.C f Domain.Types.RideRelatedNotificationConfig.EventTime,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
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
