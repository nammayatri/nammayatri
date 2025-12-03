{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RouteDetails where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.RouteStopTimeTable
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.JourneyModule.State.Types
import qualified Tools.Beam.UtilsTH

data RouteDetails = RouteDetails
  { agencyGtfsId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    agencyName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    alternateRouteIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    alternateShortNames :: [Kernel.Prelude.Text],
    endLocationLat :: Kernel.Prelude.Double,
    endLocationLon :: Kernel.Prelude.Double,
    frequency :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    fromArrivalTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    fromDepartureTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    fromStopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fromStopGtfsId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fromStopName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fromStopPlatformCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.RouteDetails.RouteDetails,
    journeyLegId :: Kernel.Prelude.Text,
    legEndTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    legStartTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    routeCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeColorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeColorName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeGtfsId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeLongName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeShortName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    startLocationLat :: Kernel.Prelude.Double,
    startLocationLon :: Kernel.Prelude.Double,
    subLegOrder :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    toArrivalTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    toDepartureTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    toStopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    toStopGtfsId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    toStopName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    toStopPlatformCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    trackingStatus :: Kernel.Prelude.Maybe Lib.JourneyModule.State.Types.TrackingStatus,
    trackingStatusLastUpdatedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    userBookedRouteShortName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data AvailableRoutesByTier = AvailableRoutesByTier
  { alsoValidServiceTypes :: Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceTierType],
    availableRoutes :: [Kernel.Prelude.Text],
    availableRoutesInfo :: [Domain.Types.RouteDetails.AvailableRoutesInfo],
    fare :: Kernel.Types.Common.PriceAPIEntity,
    nextAvailableBuses :: [Kernel.Types.Common.Seconds],
    nextAvailableTimings :: [(TimeOfDay, Kernel.Prelude.TimeOfDay)],
    quoteId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote),
    serviceTier :: BecknV2.FRFS.Enums.ServiceTierType,
    serviceTierDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    source :: Domain.Types.RouteStopTimeTable.SourceType,
    ticketTypeCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    trainTypeCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    via :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data AvailableRoutesInfo = AvailableRoutesInfo
  { isLiveTrackingAvailable :: Kernel.Prelude.Bool,
    longName :: Kernel.Prelude.Text,
    routeCode :: Kernel.Prelude.Text,
    routeTimings :: [Kernel.Types.Common.Seconds],
    shortName :: Kernel.Prelude.Text,
    source :: Domain.Types.RouteStopTimeTable.SourceType
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
