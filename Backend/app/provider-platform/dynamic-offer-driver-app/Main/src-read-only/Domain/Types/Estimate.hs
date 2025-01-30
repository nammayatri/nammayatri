{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Estimate where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.FareParameters
import qualified Domain.Types.FarePolicy
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.SearchRequest
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Estimate = Estimate
  { createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    dpVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    eligibleForUpgrade :: Kernel.Prelude.Bool,
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    fareParams :: Kernel.Prelude.Maybe Domain.Types.FareParameters.FareParameters,
    farePolicy :: Kernel.Prelude.Maybe Domain.Types.FarePolicy.FarePolicy,
    id :: Kernel.Types.Id.Id Domain.Types.Estimate.Estimate,
    isBlockedRoute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isCustomerPrefferedSearchRoute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isScheduled :: Kernel.Prelude.Bool,
    maxFare :: Kernel.Types.Common.HighPrecMoney,
    mbActualQARCity :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    mbActualQARFromLocGeohash :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    minFare :: Kernel.Types.Common.HighPrecMoney,
    requestId :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
    smartTipReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    smartTipSuggestion :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    specialLocationTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    supplyDemandRatioFromLoc :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    supplyDemandRatioToLoc :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    tipOptions :: Kernel.Prelude.Maybe [Kernel.Prelude.Int],
    tollNames :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    tripCategory :: Domain.Types.Common.TripCategory,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleServiceTier :: Domain.Types.Common.ServiceTierType,
    vehicleServiceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show)
