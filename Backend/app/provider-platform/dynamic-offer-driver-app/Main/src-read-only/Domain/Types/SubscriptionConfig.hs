{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SubscriptionConfig where

import Data.Aeson
import qualified Data.Time
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantMessage
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantServiceConfig
import qualified Domain.Types.Plan
import Kernel.Prelude
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
    paymentLinkChannel :: Domain.Types.MerchantMessage.MediaChannel,
    paymentLinkJobTime :: Data.Time.NominalDiffTime,
    paymentServiceName :: Domain.Types.MerchantServiceConfig.ServiceName,
    sendDeepLink :: Kernel.Prelude.Bool,
    sendInAppFcmNotifications :: Kernel.Prelude.Bool,
    serviceName :: Domain.Types.Plan.ServiceNames,
    useOverlayService :: Kernel.Prelude.Bool,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

{-
	DSL Source Link: file://./../../../spec/Storage/SubscriptionConfig.yaml
-}
