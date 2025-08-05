{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RideRelatedNotificationConfig where

import Data.Aeson
import qualified Domain.Types.Booking
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RideRelatedNotificationConfig = RideRelatedNotificationConfig
  { eventTime :: Domain.Types.RideRelatedNotificationConfig.EventTime,
    id :: Kernel.Types.Id.Id Domain.Types.RideRelatedNotificationConfig.RideRelatedNotificationConfig,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    notificationKey :: Kernel.Prelude.Text,
    notificationType :: Domain.Types.RideRelatedNotificationConfig.NotificationType,
    onBookingStatus :: Domain.Types.Booking.BookingStatus,
    onScheduledBooking :: Kernel.Prelude.Bool,
    onlyIfOffline :: Kernel.Prelude.Bool,
    timeDiff :: Kernel.Prelude.NominalDiffTime,
    timeDiffEvent :: Domain.Types.RideRelatedNotificationConfig.TimeDiffEvent,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, Eq)

data EventTime = PreEvent | PostEvent | OnEvent deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data NotificationType = SMS | PN | WHATSAPP | OVERLAY | CALL deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data TimeDiffEvent = RIDE_ASSIGNED | PICKUP_TIME | START_TIME | END_TIME deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''EventTime)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''NotificationType)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''TimeDiffEvent)
