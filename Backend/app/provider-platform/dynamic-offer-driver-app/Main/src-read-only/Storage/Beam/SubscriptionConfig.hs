{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SubscriptionConfig where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.MerchantMessage
import qualified Domain.Types.MerchantServiceConfig
import qualified Domain.Types.Plan
import qualified Domain.Types.VehicleCategory
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data SubscriptionConfigT f = SubscriptionConfigT
  { allowDriverFeeCalcSchedule :: B.C f Kernel.Prelude.Bool,
    allowDueAddition :: B.C f Kernel.Prelude.Bool,
    allowManualPaymentLinks :: B.C f Kernel.Prelude.Bool,
    cgstPercentageOneTimeSecurityDeposit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    deepLinkExpiryTimeInMinutes :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    defaultCityVehicleCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory),
    enableCityBasedFeeSwitch :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    executionEnabledForVehicleCategories :: B.C f (Kernel.Prelude.Maybe [Domain.Types.VehicleCategory.VehicleCategory]),
    freeTrialRidesApplicable :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    genericBatchSizeForJobs :: B.C f Kernel.Prelude.Int,
    genericJobRescheduleTime :: B.C f Kernel.Types.Common.Seconds,
    isSubscriptionEnabledAtCategoryLevel :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isTriggeredAtEndRide :: B.C f Kernel.Prelude.Bool,
    isVendorSplitEnabled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    maxRetryCount :: B.C f Kernel.Prelude.Int,
    numberOfFreeTrialRides :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    partialDueClearanceMessageKey :: B.C f (Kernel.Prelude.Maybe Domain.Types.MerchantMessage.MessageKey),
    paymentLinkChannel :: B.C f Domain.Types.MerchantMessage.MediaChannel,
    paymentLinkJobTime :: B.C f Kernel.Types.Common.Seconds,
    paymentServiceName :: B.C f Domain.Types.MerchantServiceConfig.ServiceName,
    payoutServiceName :: B.C f (Kernel.Prelude.Maybe Domain.Types.MerchantServiceConfig.ServiceName),
    sendDeepLink :: B.C f Kernel.Prelude.Bool,
    sendInAppFcmNotifications :: B.C f Kernel.Prelude.Bool,
    serviceName :: B.C f Domain.Types.Plan.ServiceNames,
    sgstPercentageOneTimeSecurityDeposit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    subscriptionDown :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    subscriptionEnabledForVehicleCategories :: B.C f (Kernel.Prelude.Maybe [Domain.Types.VehicleCategory.VehicleCategory]),
    useOverlayService :: B.C f Kernel.Prelude.Bool,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SubscriptionConfigT where
  data PrimaryKey SubscriptionConfigT f = SubscriptionConfigId (B.C f Domain.Types.Plan.ServiceNames) (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)) deriving (Generic, B.Beamable)
  primaryKey = SubscriptionConfigId <$> serviceName <*> merchantOperatingCityId

type SubscriptionConfig = SubscriptionConfigT Identity

$(enableKVPG ''SubscriptionConfigT ['serviceName, 'merchantOperatingCityId] [])

$(mkTableInstances ''SubscriptionConfigT "subscription_config")
