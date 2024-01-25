{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SubscriptionConfig where

import qualified Database.Beam as B
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantMessage
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Merchant.MerchantServiceConfig
import qualified Domain.Types.Plan
import qualified Domain.Types.SubscriptionConfig
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data SubscriptionConfigT f = SubscriptionConfigT
  { allowManualPaymentLinks :: B.C f Kernel.Prelude.Bool,
    paymentLinkChannel :: B.C f Domain.Types.Merchant.MerchantMessage.MediaChannel,
    paymentLinkJobTime :: B.C f Kernel.Types.Common.Seconds,
    genericJobRescheduleTime :: B.C f Kernel.Types.Common.Seconds,
    useOverlayService :: B.C f Kernel.Prelude.Bool,
    genericBatchSizeForJobs :: B.C f Kernel.Prelude.Int,
    deepLinkExpiryTimeInMinutes :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    maxRetryCount :: B.C f Kernel.Prelude.Int,
    id :: B.C f Kernel.Prelude.Text,
    sendInAppFcmNotifications :: B.C f Kernel.Prelude.Bool,
    allowDueAddition :: B.C f Kernel.Prelude.Bool,
    paymentServiceName :: B.C f Domain.Types.Merchant.MerchantServiceConfig.ServiceName,
    sendDeepLink :: B.C f Kernel.Prelude.Bool,
    serviceName :: B.C f Domain.Types.Plan.ServiceNames,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SubscriptionConfigT where
  data PrimaryKey SubscriptionConfigT f = SubscriptionConfigId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = SubscriptionConfigId . id

type SubscriptionConfig = SubscriptionConfigT Identity

$(enableKVPG ''SubscriptionConfigT ['id] [])

$(mkTableInstances ''SubscriptionConfigT "subscription_config")
