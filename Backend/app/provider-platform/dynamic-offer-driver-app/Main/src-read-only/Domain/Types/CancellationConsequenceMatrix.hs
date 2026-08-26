{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CancellationConsequenceMatrix where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.Extra.CancellationConsequenceMatrix
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantPaymentMethod
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.DriverCoins.Types
import qualified Lib.Types.SpecialLocation
import qualified Tools.Beam.UtilsTH

data CancellationConsequenceMatrix = CancellationConsequenceMatrix
  { active :: Kernel.Prelude.Bool,
    area :: Kernel.Prelude.Maybe Lib.Types.SpecialLocation.Area,
    blacklistDriverForRiderSeconds :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    cancelledBy :: Kernel.Prelude.Maybe Lib.DriverCoins.Types.CancellationType,
    collectionMode :: Kernel.Prelude.Maybe Domain.Types.CancellationConsequenceMatrix.ConsequenceCollectionMode,
    countsTowardCustomerCancellationStats :: Kernel.Prelude.Bool,
    countsTowardDriverCancellationRate :: Kernel.Prelude.Bool,
    customerCommissionAndTax :: Kernel.Prelude.Maybe Domain.Types.Extra.CancellationConsequenceMatrix.CommissionAndTax,
    customerDeduction :: Kernel.Prelude.Maybe Domain.Types.Extra.CancellationConsequenceMatrix.ConsequenceDeduction,
    customerNotificationKey :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverDeduction :: Kernel.Prelude.Maybe Domain.Types.Extra.CancellationConsequenceMatrix.ConsequenceDeduction,
    driverNotificationKey :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    exemptDashboardBookings :: Kernel.Prelude.Bool,
    faultRule :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    faultVerdict :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.CancellationConsequenceMatrix.CancellationConsequenceMatrix,
    maxWaiveOffsPerPeriod :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    paymentInstrument :: Kernel.Prelude.Maybe Domain.Types.MerchantPaymentMethod.PaymentInstrument,
    tripCategory :: Kernel.Prelude.Maybe Domain.Types.Common.TripCategory,
    vehicleServiceTier :: Kernel.Prelude.Maybe Domain.Types.Common.ServiceTierType,
    waiveOffAllowed :: Kernel.Prelude.Bool,
    waiveOffPeriodDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, Eq)

data ConsequenceCollectionMode = NextRideDues | ImmediateCapture deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''ConsequenceCollectionMode))
