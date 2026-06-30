{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MerchantPushNotification where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Trip
import qualified Kernel.External.Notification.Interface.Types
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data MerchantPushNotification = MerchantPushNotification
  { body :: Kernel.Prelude.Text,
    channels :: Kernel.Prelude.Maybe [Domain.Types.MerchantPushNotification.NotificationChannel],
    fcmNotificationType :: Kernel.External.Notification.Interface.Types.Category,
    fcmSubCategory :: Kernel.Prelude.Maybe Kernel.External.Notification.Interface.Types.SubCategory,
    id :: Kernel.Types.Id.Id Domain.Types.MerchantPushNotification.MerchantPushNotification,
    isCritical :: Kernel.Prelude.Bool,
    key :: Kernel.Prelude.Text,
    language :: Kernel.External.Types.Language,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    shouldTrigger :: Kernel.Prelude.Bool,
    title :: Kernel.Prelude.Text,
    tripCategory :: Kernel.Prelude.Maybe Domain.Types.Trip.TripCategory,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, Eq)

data NotificationChannel = FCM | GRPC | SMS | WHATSAPP | OVERLAY | WEB deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''NotificationChannel)

$(mkHttpInstancesForEnum ''NotificationChannel)
