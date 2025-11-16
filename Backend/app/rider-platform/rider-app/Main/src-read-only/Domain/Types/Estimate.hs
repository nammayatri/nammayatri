{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Estimate where

import qualified BecknV2.OnDemand.Enums
import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.EstimateStatus
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.SearchRequest
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.TripTerms
import qualified Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import qualified Kernel.Utils.GenericPretty
import qualified Servant.Client.Core
import qualified Tools.Beam.UtilsTH

data Estimate = Estimate
  { backendAppVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    backendConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    boostSearchPreSelectionServiceTierConfig :: Kernel.Prelude.Maybe [Domain.Types.ServiceTierType.ServiceTierType],
    bppEstimateId :: Kernel.Types.Id.Id Domain.Types.Estimate.BPPEstimate,
    clientBundleVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientDevice :: Kernel.Prelude.Maybe Kernel.Types.Version.Device,
    clientSdkVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    createdAt :: Kernel.Prelude.UTCTime,
    device :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    discount :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    driversLocation :: [Kernel.External.Maps.LatLong],
    estimateBreakupList :: [Domain.Types.Estimate.EstimateBreakup],
    estimateTags :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    estimatedFare :: Kernel.Types.Common.Price,
    estimatedPickupDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    estimatedStaticDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    estimatedTotalFare :: Kernel.Types.Common.Price,
    id :: Kernel.Types.Id.Id Domain.Types.Estimate.Estimate,
    insuredAmount :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    isAirConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isBlockedRoute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isCustomerPrefferedSearchRoute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isInsured :: Kernel.Prelude.Bool,
    isMultimodalSearch :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    itemId :: Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    nightShiftInfo :: Kernel.Prelude.Maybe Domain.Types.Estimate.NightShiftInfo,
    providerCompletedRidesCount :: Kernel.Prelude.Int,
    providerId :: Kernel.Prelude.Text,
    providerMobileNumber :: Kernel.Prelude.Text,
    providerName :: Kernel.Prelude.Text,
    providerUrl :: Servant.Client.Core.BaseUrl,
    qar :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    requestId :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
    serviceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceTierShortDesc :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    smartTipReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    smartTipSuggestion :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    specialLocationName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    specialLocationTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Domain.Types.EstimateStatus.EstimateStatus,
    tipOptions :: Kernel.Prelude.Maybe [Kernel.Prelude.Int],
    tollChargesInfo :: Kernel.Prelude.Maybe Domain.Types.Estimate.TollChargesInfo,
    totalFareRange :: Domain.Types.Estimate.FareRange,
    tripCategory :: Kernel.Prelude.Maybe Domain.Types.Common.TripCategory,
    tripTerms :: Kernel.Prelude.Maybe Domain.Types.TripTerms.TripTerms,
    updatedAt :: Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.UTCTime,
    vehicleCategory :: Kernel.Prelude.Maybe BecknV2.OnDemand.Enums.VehicleCategory,
    vehicleIconUrl :: Kernel.Prelude.Maybe Servant.Client.Core.BaseUrl,
    vehicleServiceTierAirConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    vehicleServiceTierSeatingCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleServiceTierType :: Domain.Types.ServiceTierType.ServiceTierType,
    waitingCharges :: Domain.Types.Estimate.WaitingCharges
  }
  deriving (Generic, Show)

data BPPEstimate = BPPEstimate {} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data EstimateBreakup = EstimateBreakup
  { estimateId :: Kernel.Types.Id.Id Domain.Types.Estimate.Estimate,
    id :: Kernel.Types.Id.Id Domain.Types.Estimate.EstimateBreakup,
    price :: Domain.Types.Estimate.EstimateBreakupPrice,
    title :: Kernel.Prelude.Text
  }
  deriving (Generic, Show, Kernel.Utils.GenericPretty.PrettyShow)

newtype EstimateBreakupPrice = EstimateBreakupPrice {value :: Kernel.Types.Common.Price} deriving (Generic, Show, Kernel.Utils.GenericPretty.PrettyShow)

data FareRange = FareRange {maxFare :: Kernel.Types.Common.Price, minFare :: Kernel.Types.Common.Price} deriving (Generic, Show, Kernel.Utils.GenericPretty.PrettyShow)

data NightShiftInfo = NightShiftInfo
  { nightShiftCharge :: Kernel.Types.Common.Price,
    nightShiftEnd :: Kernel.Prelude.TimeOfDay,
    nightShiftStart :: Kernel.Prelude.TimeOfDay,
    oldNightShiftCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal
  }
  deriving (Generic, Show)

data TollChargesInfo = TollChargesInfo {tollCharges :: Kernel.Types.Common.Price, tollNames :: [Kernel.Prelude.Text]} deriving (Generic, Show)

data WaitingCharges = WaitingCharges {waitingChargePerMin :: Kernel.Prelude.Maybe Kernel.Types.Common.Price} deriving (Generic, Show)
