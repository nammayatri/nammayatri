{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.RideRelatedNotificationConfig where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Domain.Types.RideRelatedNotificationConfig
import qualified Kernel.Prelude
import qualified Domain.Types.BookingStatus
import qualified Kernel.Types.Common
import qualified Domain.Types.UtilsTH
import qualified Database.Beam as B



data RideRelatedNotificationConfigT f
    = RideRelatedNotificationConfigT {eventTime :: (B.C f Domain.Types.RideRelatedNotificationConfig.EventTime),
                                      id :: (B.C f Kernel.Prelude.Text),
                                      merchantId :: (B.C f Kernel.Prelude.Text),
                                      merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                                      notificationKey :: (B.C f Kernel.Prelude.Text),
                                      notificationType :: (B.C f Domain.Types.RideRelatedNotificationConfig.NotificationType),
                                      onBookingStatus :: (B.C f Domain.Types.BookingStatus.BookingStatus),
                                      onScheduledBooking :: (B.C f Kernel.Prelude.Bool),
                                      timeDiff :: (B.C f Kernel.Types.Common.Seconds),
                                      timeDiffEvent :: (B.C f Domain.Types.RideRelatedNotificationConfig.TimeDiffEvent),
                                      createdAt :: (B.C f Kernel.Prelude.UTCTime),
                                      updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table RideRelatedNotificationConfigT
    where data PrimaryKey RideRelatedNotificationConfigT f = RideRelatedNotificationConfigId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = RideRelatedNotificationConfigId <$> id <*> merchantOperatingCityId
type RideRelatedNotificationConfig = RideRelatedNotificationConfigT Identity

$(enableKVPG (''RideRelatedNotificationConfigT) [('id), ('merchantOperatingCityId)] [])

$(mkTableInstances (''RideRelatedNotificationConfigT) "ride_related_notification_config")

$(Domain.Types.UtilsTH.mkCacParseInstance (''RideRelatedNotificationConfigT))

