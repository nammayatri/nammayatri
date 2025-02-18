{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.NotificationSoundsConfig where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.External.Notification.Interface.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data NotificationSoundsConfig = NotificationSoundsConfig
  { blindSound :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    defaultSound :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    notificationType :: Kernel.External.Notification.Interface.Types.Category,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
