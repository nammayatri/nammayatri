{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.SubscriptionConfig where

import qualified Data.Time
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantMessage
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Merchant.MerchantServiceConfig
import qualified Domain.Types.Plan
import Kernel.Prelude
import qualified Kernel.Types.Id

data SubscriptionConfig = SubscriptionConfig
  { allowManualPaymentLinks :: Kernel.Prelude.Bool,
    useOverlayService :: Kernel.Prelude.Bool,
    paymentLinkChannel :: Domain.Types.Merchant.MerchantMessage.MediaChannel,
    paymentLinkJobTime :: Data.Time.NominalDiffTime,
    genericBatchSizeForJobs :: Kernel.Prelude.Int,
    genericJobRescheduleTime :: Data.Time.NominalDiffTime,
    maxRetryCount :: Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Domain.Types.SubscriptionConfig.SubscriptionConfig,
    sendInAppFcmNotifications :: Kernel.Prelude.Bool,
    paymentServiceName :: Domain.Types.Merchant.MerchantServiceConfig.ServiceName,
    allowDueAddition :: Kernel.Prelude.Bool,
    serviceName :: Domain.Types.Plan.ServiceNames,
    sendDeepLink :: Kernel.Prelude.Bool,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity),
    deepLinkExpiryTimeInMinutes :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)
