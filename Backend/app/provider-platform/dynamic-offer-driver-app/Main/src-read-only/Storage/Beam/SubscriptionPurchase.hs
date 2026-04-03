{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.SubscriptionPurchase where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.SubscriptionPurchase
import qualified Kernel.Types.Common
import qualified Domain.Types.Plan
import qualified Data.Aeson
import qualified Domain.Types.Extra.Plan
import qualified Domain.Types.VehicleCategory
import qualified Domain.Types.DriverPlan
import qualified Database.Beam as B



data SubscriptionPurchaseT f
    = SubscriptionPurchaseT {enableServiceUsageCharge :: (B.C f Kernel.Prelude.Bool),
                             expiryDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                             financeInvoiceId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                             id :: (B.C f Kernel.Prelude.Text),
                             merchantId :: (B.C f Kernel.Prelude.Text),
                             merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                             ownerId :: (B.C f Kernel.Prelude.Text),
                             ownerType :: (B.C f Domain.Types.SubscriptionPurchase.SubscriptionOwnerType),
                             paymentOrderId :: (B.C f Kernel.Prelude.Text),
                             planFee :: (B.C f Kernel.Types.Common.HighPrecMoney),
                             planFrequency :: (B.C f Domain.Types.Plan.Frequency),
                             planId :: (B.C f Kernel.Prelude.Text),
                             planRideCredit :: (B.C f Kernel.Types.Common.HighPrecMoney),
                             purchaseTimestamp :: (B.C f Kernel.Prelude.UTCTime),
                             reconciliationStatus :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
                             serviceName :: (B.C f Domain.Types.Extra.Plan.ServiceNames),
                             startDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                             status :: (B.C f Domain.Types.SubscriptionPurchase.SubscriptionPurchaseStatus),
                             vehicleCategory :: (B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory)),
                             waiveOfMode :: (B.C f Domain.Types.DriverPlan.WaiveOffMode),
                             waiveOffEnabledOn :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                             waiveOffValidTill :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                             waiverOffPercentage :: (B.C f Kernel.Types.Common.HighPrecMoney),
                             createdAt :: (B.C f Kernel.Prelude.UTCTime),
                             updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table SubscriptionPurchaseT
    where data PrimaryKey SubscriptionPurchaseT f = SubscriptionPurchaseId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = SubscriptionPurchaseId . id
type SubscriptionPurchase = SubscriptionPurchaseT Identity

$(enableKVPG (''SubscriptionPurchaseT) [('id)] [[('financeInvoiceId)], [('ownerId)], [('paymentOrderId)]])

$(mkTableInstances (''SubscriptionPurchaseT) "subscription_purchase")

