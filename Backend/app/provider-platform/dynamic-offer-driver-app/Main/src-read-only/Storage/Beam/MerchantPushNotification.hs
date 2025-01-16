{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MerchantPushNotification where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Trip
import Kernel.External.Encryption
import qualified Kernel.External.Notification.FCM.Types
import qualified Kernel.External.Notification.Interface.Types
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MerchantPushNotificationT f = MerchantPushNotificationT
  { body :: B.C f Kernel.Prelude.Text,
    fcmNotificationType :: B.C f Kernel.External.Notification.FCM.Types.FCMNotificationType,
    fcmSubCategory :: B.C f (Kernel.Prelude.Maybe Kernel.External.Notification.Interface.Types.SubCategory),
    id :: B.C f Kernel.Prelude.Text,
    key :: B.C f Kernel.Prelude.Text,
    language :: B.C f Kernel.External.Types.Language,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    title :: B.C f Kernel.Prelude.Text,
    tripCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.Trip.TripCategory),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantPushNotificationT where
  data PrimaryKey MerchantPushNotificationT f = MerchantPushNotificationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MerchantPushNotificationId . id

type MerchantPushNotification = MerchantPushNotificationT Identity

$(enableKVPG ''MerchantPushNotificationT ['id] [])

$(mkTableInstances ''MerchantPushNotificationT "merchant_push_notification")
