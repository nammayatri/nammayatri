{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SubscriptionPurchase where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DriverPlan
import qualified Domain.Types.Extra.Plan
import qualified Domain.Types.Plan
import qualified Domain.Types.SubscriptionPurchase
import qualified Domain.Types.VehicleCategory
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data SubscriptionPurchaseT f = SubscriptionPurchaseT
  { enableServiceUsageCharge :: B.C f Kernel.Prelude.Bool,
    expiryDate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    ownerId :: B.C f Kernel.Prelude.Text,
    ownerType :: B.C f Domain.Types.SubscriptionPurchase.SubscriptionOwnerType,
    paymentOrderId :: B.C f Kernel.Prelude.Text,
    planFee :: B.C f Kernel.Types.Common.HighPrecMoney,
    planFrequency :: B.C f Domain.Types.Plan.Frequency,
    planId :: B.C f Kernel.Prelude.Text,
    planRideCredit :: B.C f Kernel.Types.Common.HighPrecMoney,
    purchaseTimestamp :: B.C f Kernel.Prelude.UTCTime,
    serviceName :: B.C f Domain.Types.Extra.Plan.ServiceNames,
    status :: B.C f Domain.Types.SubscriptionPurchase.SubscriptionPurchaseStatus,
    vehicleCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory),
    waiveOfMode :: B.C f Domain.Types.DriverPlan.WaiveOffMode,
    waiveOffEnabledOn :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    waiveOffValidTill :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    waiverOffPercentage :: B.C f Kernel.Types.Common.HighPrecMoney,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SubscriptionPurchaseT where
  data PrimaryKey SubscriptionPurchaseT f = SubscriptionPurchaseId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SubscriptionPurchaseId . id

type SubscriptionPurchase = SubscriptionPurchaseT Identity

$(enableKVPG ''SubscriptionPurchaseT ['id] [['ownerId], ['paymentOrderId]])

$(mkTableInstances ''SubscriptionPurchaseT "subscription_purchase")
