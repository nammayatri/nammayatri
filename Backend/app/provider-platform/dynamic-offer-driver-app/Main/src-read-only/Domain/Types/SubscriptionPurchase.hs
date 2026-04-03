{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.SubscriptionPurchase where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Lib.Finance.Domain.Types.Invoice
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Kernel.Types.Common
import qualified Domain.Types.Plan
import qualified Domain.Types.Extra.Plan
import qualified Domain.Types.VehicleCategory
import qualified Domain.Types.DriverPlan
import qualified Kernel.Beam.Lib.UtilsTH
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH



data SubscriptionPurchase
    = SubscriptionPurchase {enableServiceUsageCharge :: Kernel.Prelude.Bool,
                            expiryDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
                            financeInvoiceId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Finance.Domain.Types.Invoice.Invoice),
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
                            reconciliationStatus :: Kernel.Prelude.Maybe Data.Aeson.Value,
                            serviceName :: Domain.Types.Extra.Plan.ServiceNames,
                            startDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
                            status :: Domain.Types.SubscriptionPurchase.SubscriptionPurchaseStatus,
                            vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
                            waiveOfMode :: Domain.Types.DriverPlan.WaiveOffMode,
                            waiveOffEnabledOn :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
                            waiveOffValidTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
                            waiverOffPercentage :: Kernel.Types.Common.HighPrecMoney,
                            createdAt :: Kernel.Prelude.UTCTime,
                            updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, ( Show), ( Eq))
data SubscriptionOwnerType = DRIVER | FLEET_OWNER deriving (Generic, ( Show), ( ToJSON), ( FromJSON), ( ToSchema), ( Ord), ( Eq), ( Read), ( ToParamSchema))
data SubscriptionPurchaseStatus = PENDING | ACTIVE | EXPIRED | FAILED | EXHAUSTED deriving (Generic, ( Show), ( ToJSON), ( FromJSON), ( ToSchema), ( Ord), ( Eq), ( Read), ( ToParamSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''SubscriptionPurchaseStatus))

$(Kernel.Utils.TH.mkHttpInstancesForEnum (''SubscriptionPurchaseStatus))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''SubscriptionOwnerType))

