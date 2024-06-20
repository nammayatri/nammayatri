{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Estimate where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.SearchRequest
import qualified Domain.Types.TripTerms
import qualified Domain.Types.VehicleServiceTier
import qualified Kernel.Beam.Lib.UtilsTH
import qualified Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import qualified Kernel.Utils.GenericPretty
import qualified Kernel.Utils.TH
import qualified Servant.Client.Core
import qualified Tools.Beam.UtilsTH

data Estimate = Estimate
  { id :: Kernel.Types.Id.Id Domain.Types.Estimate.Estimate,
    requestId :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    bppEstimateId :: Kernel.Types.Id.Id Domain.Types.Estimate.BPPEstimate,
    estimatedFare :: Kernel.Types.Common.Price,
    discount :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    estimatedTotalFare :: Kernel.Types.Common.Price,
    totalFareRange :: Domain.Types.Estimate.FareRange,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    estimatedPickupDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    device :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    providerId :: Kernel.Prelude.Text,
    providerUrl :: Servant.Client.Core.BaseUrl,
    providerName :: Kernel.Prelude.Text,
    providerMobileNumber :: Kernel.Prelude.Text,
    providerCompletedRidesCount :: Kernel.Prelude.Int,
    vehicleServiceTierType :: Domain.Types.VehicleServiceTier.VehicleServiceTierType,
    vehicleServiceTierSeatingCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleServiceTierAirConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    isAirConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    itemId :: Kernel.Prelude.Text,
    tripTerms :: Kernel.Prelude.Maybe Domain.Types.TripTerms.TripTerms,
    estimateBreakupList :: [Domain.Types.Estimate.EstimateBreakup],
    nightShiftInfo :: Kernel.Prelude.Maybe Domain.Types.Estimate.NightShiftInfo,
    status :: Domain.Types.Estimate.EstimateStatus,
    waitingCharges :: Domain.Types.Estimate.WaitingCharges,
    driversLocation :: [Kernel.External.Maps.LatLong],
    specialLocationTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    specialLocationName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceTierShortDesc :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    clientSdkVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientBundleVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    backendConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    backendAppVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    clientDevice :: Kernel.Prelude.Maybe Kernel.Types.Version.Device,
    tollChargesInfo :: Kernel.Prelude.Maybe Domain.Types.Estimate.TollChargesInfo,
    isCustomerPrefferedSearchRoute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isBlockedRoute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    updatedAt :: Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.UTCTime
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

data EstimateStatus
  = NEW
  | DRIVER_QUOTE_REQUESTED
  | CANCELLED
  | GOT_DRIVER_QUOTE
  | DRIVER_QUOTE_CANCELLED
  | COMPLETED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

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

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''EstimateStatus)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''EstimateStatus)
