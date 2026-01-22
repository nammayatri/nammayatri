{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SubscriptionConfig where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.MerchantMessage
import qualified Domain.Types.MerchantServiceConfig
import qualified Domain.Types.Plan
import qualified Domain.Types.SubscriptionConfig
import qualified Domain.Types.VehicleCategory
import qualified Domain.Types.VehicleVariant
import qualified Domain.Types.WebhookExtra
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data SubscriptionConfigT f = SubscriptionConfigT
  { allowDriverFeeCalcSchedule :: (B.C f Kernel.Prelude.Bool),
    allowDueAddition :: (B.C f Kernel.Prelude.Bool),
    allowManualPaymentLinks :: (B.C f Kernel.Prelude.Bool),
    autopayEnabled :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    cgstPercentageOneTimeSecurityDeposit :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    dataEntityToSend :: (B.C f (Kernel.Prelude.Maybe [Domain.Types.SubscriptionConfig.CurrentPlanEntites])),
    deepLinkExpiryTimeInMinutes :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    defaultCityVehicleCategory :: (B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory)),
    disabledVariantsForSubscription :: (B.C f (Kernel.Prelude.Maybe [Domain.Types.VehicleVariant.VehicleVariant])),
    enableCityBasedFeeSwitch :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    enablePayoutSettlement :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    enableServiceUsageChargeDefault :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    eventsEnabledForWebhook :: (B.C f (Kernel.Prelude.Maybe [Domain.Types.WebhookExtra.WebhookEvent])),
    executionEnabledForVehicleCategories :: (B.C f (Kernel.Prelude.Maybe [Domain.Types.VehicleCategory.VehicleCategory])),
    extWebhookConfigs :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    freeTrialRidesApplicable :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    genericBatchSizeForJobs :: (B.C f Kernel.Prelude.Int),
    genericJobRescheduleTime :: (B.C f Kernel.Types.Common.Seconds),
    genericNextJobScheduleTimeThreshold :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds)),
    isFreeTrialDaysApplicable :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    isSubscriptionEnabledAtCategoryLevel :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    isTriggeredAtEndRide :: (B.C f Kernel.Prelude.Bool),
    isUIEnabled :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    isVendorSplitEnabled :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    manualPaymentLinkRateLimitExpirySeconds :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds)),
    maxRetryCount :: (B.C f Kernel.Prelude.Int),
    numberOfFreeTrialRides :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    partialDueClearanceMessageKey :: (B.C f (Kernel.Prelude.Maybe Domain.Types.MerchantMessage.MessageKey)),
    paymentLinkChannel :: (B.C f Domain.Types.MerchantMessage.MediaChannel),
    paymentLinkJobTime :: (B.C f Kernel.Types.Common.Seconds),
    paymentServiceName :: (B.C f Domain.Types.MerchantServiceConfig.ServiceName),
    payoutServiceName :: (B.C f (Kernel.Prelude.Maybe Domain.Types.MerchantServiceConfig.ServiceName)),
    payoutSettlementJobScheduleTime :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds)),
    payoutSettlementWeekStartDay :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    payoutSettlementWeekStartTime :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds)),
    sendDeepLink :: (B.C f Kernel.Prelude.Bool),
    sendInAppFcmNotifications :: (B.C f Kernel.Prelude.Bool),
    sendManualPaymentLinkJobMaxDelay :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds)),
    serviceName :: (B.C f Domain.Types.Plan.ServiceNames),
    sgstPercentageOneTimeSecurityDeposit :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    showManualPlansInUI :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    subscriptionDown :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    subscriptionEnabledForVehicleCategories :: (B.C f (Kernel.Prelude.Maybe [Domain.Types.VehicleCategory.VehicleCategory])),
    useOverlayService :: (B.C f Kernel.Prelude.Bool),
    waiveOffOfferDescription :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    waiveOffOfferTitle :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    webhookConfig :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table SubscriptionConfigT where
  data PrimaryKey SubscriptionConfigT f = SubscriptionConfigId (B.C f Domain.Types.Plan.ServiceNames) (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))) deriving (Generic, B.Beamable)
  primaryKey = SubscriptionConfigId <$> serviceName <*> merchantOperatingCityId

type SubscriptionConfig = SubscriptionConfigT Identity

$(enableKVPG (''SubscriptionConfigT) [('serviceName), ('merchantOperatingCityId)] [])

$(mkTableInstances (''SubscriptionConfigT) "subscription_config")
