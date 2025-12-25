{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Types.TagActionNotificationConfig where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Yudhishthira.Types
import qualified Tools.Beam.UtilsTH

data TagActionNotificationConfig = TagActionNotificationConfig
  { id :: Kernel.Types.Id.Id Lib.Yudhishthira.Types.TagActionNotificationConfig.TagActionNotificationConfig,
    merchantId :: Kernel.Types.Id.Id Lib.Yudhishthira.Types.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity,
    notificationKey :: Kernel.Prelude.Text,
    notificationType :: Lib.Yudhishthira.Types.TagActionNotificationConfig.NotificationType,
    notifyAt :: Kernel.Prelude.TimeOfDay,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data NotificationType = SMS | PN | WHATSAPP | OVERLAY | CALL deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''NotificationType)
