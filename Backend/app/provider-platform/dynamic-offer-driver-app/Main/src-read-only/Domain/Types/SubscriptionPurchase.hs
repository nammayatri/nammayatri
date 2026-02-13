{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SubscriptionPurchase where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Plan
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Tools.Beam.UtilsTH

data SubscriptionPurchase = SubscriptionPurchase
  { expiryDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.SubscriptionPurchase.SubscriptionPurchase,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    ownerId :: Kernel.Prelude.Text,
    ownerType :: Domain.Types.SubscriptionPurchase.SubscriptionOwnerType,
    paymentOrderId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder,
    planFee :: Kernel.Types.Common.HighPrecMoney,
    planFrequency :: Domain.Types.Plan.Frequency,
    planId :: Kernel.Types.Id.Id Domain.Types.Plan.Plan,
    planRideCredit :: Kernel.Types.Common.HighPrecMoney,
    purchaseTimestamp :: Kernel.Prelude.UTCTime,
    status :: Domain.Types.SubscriptionPurchase.SubscriptionPurchaseStatus,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Eq)

data SubscriptionOwnerType = DRIVER | FLEET_OWNER deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Ord, Eq, Read, ToParamSchema)

data SubscriptionPurchaseStatus = PENDING | ACTIVE | EXPIRED | FAILED deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Ord, Eq, Read, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''SubscriptionPurchaseStatus)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''SubscriptionOwnerType)
