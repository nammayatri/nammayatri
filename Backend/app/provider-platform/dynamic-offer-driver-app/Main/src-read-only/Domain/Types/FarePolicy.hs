{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FarePolicy (module Domain.Types.FarePolicy, module ReExport) where

import qualified API.Types.ProviderPlatform.Management.Merchant
import Data.Aeson
import qualified Data.List.NonEmpty
import qualified Domain.Types.CancellationFarePolicy
import Domain.Types.Common (UsageSafety (..))
import qualified Domain.Types.ConditionalCharges
import Domain.Types.Extra.FarePolicy as ReExport
import qualified Domain.Types.Extra.FarePolicy
import qualified Domain.Types.FarePolicy.DriverExtraFeeBounds
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FarePolicyD (s :: UsageSafety) = FarePolicy
  { additionalCongestionCharge :: Kernel.Types.Common.HighPrecMoney,
    airportConvenienceFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    allowedTripDistanceBounds :: Kernel.Prelude.Maybe Domain.Types.Extra.FarePolicy.AllowedTripDistanceBounds,
    boothCharges :: Kernel.Prelude.Maybe Domain.Types.Extra.FarePolicy.BoothCharge,
    businessDiscountPercentage :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    cancellationCommissionChargeConfig :: Kernel.Prelude.Maybe Domain.Types.Extra.FarePolicy.FareChargeConfig,
    cancellationFarePolicyId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.CancellationFarePolicy.CancellationFarePolicy),
    cardCharge :: Kernel.Prelude.Maybe Domain.Types.Extra.FarePolicy.CardCharge,
    cgst :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    commissionChargeConfig :: Kernel.Prelude.Maybe Domain.Types.Extra.FarePolicy.FareChargeConfig,
    conditionalCharges :: [Domain.Types.ConditionalCharges.ConditionalCharges],
    congestionChargeMultiplier :: Kernel.Prelude.Maybe Domain.Types.Extra.FarePolicy.CongestionChargeMultiplier,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    driverAllowance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    driverCancellationNotAllowed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    driverExtraFeeBounds :: Kernel.Prelude.Maybe (Data.List.NonEmpty.NonEmpty Domain.Types.FarePolicy.DriverExtraFeeBounds.DriverExtraFeeBounds),
    farePolicyDetails :: Domain.Types.Extra.FarePolicy.FarePolicyDetailsD s,
    govtCharges :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    id :: Kernel.Types.Id.Id Domain.Types.FarePolicy.FarePolicy,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    nightShiftBounds :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Merchant.NightShiftBounds,
    parkingCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    perDistanceUnitInsuranceCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    perLuggageCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    perMinuteRideExtraTimeCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    perStopCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    personalDiscountPercentage :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    petCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    pickupBufferInSecsForNightShiftCal :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    platformFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    platformFeeChargesBy :: Domain.Types.Extra.FarePolicy.PlatformFeeMethods,
    priorityCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    returnFee :: Kernel.Prelude.Maybe Domain.Types.Extra.FarePolicy.ReturnFee,
    rideExtraTimeChargeGracePeriod :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    schedulingCharge :: Kernel.Prelude.Maybe Domain.Types.Extra.FarePolicy.SchedulingCharge,
    serviceCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    sgst :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    tipOptions :: Kernel.Prelude.Maybe [Kernel.Prelude.Int],
    tollCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    tollTaxChargeConfig :: Kernel.Prelude.Maybe Domain.Types.Extra.FarePolicy.FareChargeConfig,
    updatedAt :: Kernel.Prelude.UTCTime,
    vatChargeConfig :: Kernel.Prelude.Maybe Domain.Types.Extra.FarePolicy.FareChargeConfig
  }
  deriving (Generic, Show, ToSchema)

type FarePolicy = FarePolicyD ('Safe)

instance FromJSON (FarePolicyD 'Unsafe)

instance ToJSON (FarePolicyD 'Unsafe)

instance FromJSON (FarePolicyD 'Safe)

instance ToJSON (FarePolicyD 'Safe)
