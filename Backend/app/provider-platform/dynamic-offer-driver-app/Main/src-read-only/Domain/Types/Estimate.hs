{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Estimate where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.FareParameters
import qualified Domain.Types.FarePolicy
import qualified Domain.Types.FarePolicy.DriverExtraFeeBounds
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.SearchRequest
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Estimate = Estimate
  { businessDiscount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    congestionMultiplier :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    dpVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverExtraFeeBounds :: Kernel.Prelude.Maybe Domain.Types.FarePolicy.DriverExtraFeeBounds.DriverExtraFeeBounds,
    eligibleForUpgrade :: Kernel.Prelude.Bool,
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    fareParams :: Kernel.Prelude.Maybe Domain.Types.FareParameters.FareParameters,
    farePolicy :: Kernel.Prelude.Maybe Domain.Types.FarePolicy.FarePolicy,
    fromLocGeohash :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.Estimate.Estimate,
    isBlockedRoute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isCustomerPrefferedSearchRoute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isScheduled :: Kernel.Prelude.Bool,
    maxFare :: Kernel.Types.Common.HighPrecMoney,
    mbActualQARCity :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    mbActualQARCityPast :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    mbActualQARFromLocGeohash :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    mbActualQARFromLocGeohashDistance :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    mbActualQARFromLocGeohashDistancePast :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    mbActualQARFromLocGeohashPast :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    mbCongestionCity :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    mbCongestionCityPast :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    mbCongestionFromLocGeohash :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    mbCongestionFromLocGeohashDistance :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    mbCongestionFromLocGeohashDistancePast :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    mbCongestionFromLocGeohashPast :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    minFare :: Kernel.Types.Common.HighPrecMoney,
    personalDiscount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    requestId :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
    smartTipReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    smartTipSuggestion :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    specialLocationTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    supplyDemandRatioFromLoc :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    supplyDemandRatioToLoc :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    tipOptions :: Kernel.Prelude.Maybe [Kernel.Prelude.Int],
    tollIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    tollNames :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    tripCategory :: Domain.Types.Common.TripCategory,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleServiceTier :: Domain.Types.Common.ServiceTierType,
    vehicleServiceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
