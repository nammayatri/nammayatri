{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SubscriptionConfig where

import qualified Data.Time
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantMessage
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Merchant.MerchantServiceConfig
import qualified Domain.Types.Plan
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data SubscriptionConfig = SubscriptionConfig
  { allowDriverFeeCalcSchedule :: Kernel.Prelude.Bool,
    allowDueAddition :: Kernel.Prelude.Bool,
    allowManualPaymentLinks :: Kernel.Prelude.Bool,
    deepLinkExpiryTimeInMinutes :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    genericBatchSizeForJobs :: Kernel.Prelude.Int,
    genericJobRescheduleTime :: Data.Time.NominalDiffTime,
    isTriggeredAtEndRide :: Kernel.Prelude.Bool,
    maxRetryCount :: Kernel.Prelude.Int,
    paymentLinkChannel :: Domain.Types.Merchant.MerchantMessage.MediaChannel,
    paymentLinkJobTime :: Data.Time.NominalDiffTime,
    paymentServiceName :: Domain.Types.Merchant.MerchantServiceConfig.ServiceName,
    sendDeepLink :: Kernel.Prelude.Bool,
    sendInAppFcmNotifications :: Kernel.Prelude.Bool,
    serviceName :: Domain.Types.Plan.ServiceNames,
    useOverlayService :: Kernel.Prelude.Bool,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)
