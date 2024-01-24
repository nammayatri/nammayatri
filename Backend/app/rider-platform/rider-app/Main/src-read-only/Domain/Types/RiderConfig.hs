{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RiderConfig where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RiderConfig = RiderConfig
  { appUrl :: Kernel.Prelude.Text,
    enableEmergencyContactAddedMessage :: Kernel.Prelude.Bool,
    enableLocalPoliceSupport :: Kernel.Prelude.Bool,
    enableSupportForSafety :: Kernel.Prelude.Bool,
    localPoliceNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    safetyCheckEndTime :: Kernel.Types.Common.Seconds,
    safetyCheckStartTime :: Kernel.Types.Common.Seconds,
    timeDiffFromUtc :: Kernel.Types.Common.Seconds,
    trackingShortUrlPattern :: Kernel.Prelude.Text,
    videoFileSizeUpperLimit :: Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
