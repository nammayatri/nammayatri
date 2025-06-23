{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverPlan (module Domain.Types.DriverPlan, module ReExport) where

import Data.Aeson
import qualified Domain.Types.DriverInformation
import Domain.Types.Extra.DriverPlan as ReExport
import qualified Domain.Types.Extra.DriverPlan
import qualified Domain.Types.Extra.Plan
import qualified Domain.Types.Mandate
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Plan
import qualified Domain.Types.VehicleCategory
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DriverPlan = DriverPlan
  { autoPayStatus :: Kernel.Prelude.Maybe Domain.Types.DriverInformation.DriverAutoPayStatus,
    coinCovertedToCashLeft :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    enableServiceUsageCharge :: Kernel.Prelude.Bool,
    isCategoryLevelSubscriptionEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isOnFreeTrial :: Kernel.Prelude.Bool,
    lastBillGeneratedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    lastPaymentLinkSentAtIstDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    mandateId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Mandate.Mandate),
    mandateSetupDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOpCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    payerVpa :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    planId :: Kernel.Types.Id.Id Domain.Types.Plan.Plan,
    planType :: Domain.Types.Plan.PaymentMode,
    serviceName :: Domain.Types.Extra.Plan.ServiceNames,
    subscriptionServiceRelatedData :: Domain.Types.Extra.DriverPlan.SubscriptionServiceRelatedData,
    totalAmountChargedForService :: Kernel.Prelude.Int,
    totalCoinsConvertedCash :: Kernel.Types.Common.HighPrecMoney,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
    waiveOfMode :: Domain.Types.DriverPlan.WaiveOffMode,
    waiveOffEnabledOn :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    waiveOffValidTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    waiverOffPercentage :: Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic, Show, Eq, Ord)

data WaiveOffEntity = WaiveOffEntity
  { daysValidFor :: Kernel.Prelude.Integer,
    driverId :: Kernel.Prelude.Text,
    percentage :: Kernel.Types.Common.HighPrecMoney,
    serviceName :: Domain.Types.Extra.Plan.ServiceNames,
    waiveOfMode :: Domain.Types.DriverPlan.WaiveOffMode
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data WaiveOffMode = WITH_OFFER | WITHOUT_OFFER | NO_WAIVE_OFF deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''WaiveOffMode)
