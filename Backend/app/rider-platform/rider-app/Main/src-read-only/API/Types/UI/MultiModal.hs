{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.MultiModal where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Booking
import qualified Domain.Types.Common
import qualified Domain.Types.Estimate
import qualified Domain.Types.Location
import qualified Domain.Types.SearchRequest
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data MetroLegInfo = MetroLegInfo {metroLines :: [MetroLineInfo]}
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MetroLineInfo = MetroLineInfo
  { endStation :: MetroStationInfo,
    estimatedFare :: Kernel.Types.Common.PriceAPIEntity,
    lineColor :: Kernel.Prelude.Text,
    lineName :: Kernel.Prelude.Text,
    lineStations :: [MetroStationInfo],
    lineTimings :: MetroLineTiming,
    startStation :: MetroStationInfo
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MetroLineTiming = MetroLineTiming {endTime :: Kernel.Prelude.TimeOfDay, frequency :: Kernel.Types.Common.Seconds, startTime :: Kernel.Prelude.TimeOfDay}
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MetroStationInfo = MetroStationInfo
  { exit :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    platform :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    stationCode :: Kernel.Prelude.Text,
    stationLocation :: Kernel.External.Maps.LatLong,
    stationName :: Kernel.Prelude.Text
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MultiModalEstimate = MultiModalEstimate
  { estimatedFare :: Kernel.Types.Common.PriceAPIEntity,
    id :: Kernel.Types.Id.Id Domain.Types.Estimate.Estimate,
    isValueAddNP :: Kernel.Prelude.Bool,
    serviceTierName :: Kernel.Prelude.Text,
    serviceTierShortDesc :: Kernel.Prelude.Text,
    travelModes :: [Domain.Types.Common.TravelMode],
    validTill :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MultiModalRouteDetails = MultiModalRouteDetails
  { automaticBooking :: Kernel.Prelude.Bool,
    estimatedFare :: Kernel.Types.Common.PriceAPIEntity,
    routeLegs :: [MultiModalRouteLeg],
    totalDistance :: Kernel.Types.Common.Distance,
    totalDuration :: Kernel.Types.Common.Seconds
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MultiModalRouteLeg = MultiModalRouteLeg {distance :: Kernel.Types.Common.Distance, duration :: Kernel.Types.Common.Seconds, legInfo :: MultiModalRouteLegInfo, status :: MultiModalRouteLegStatus}
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MultiModalRouteLegInfo
  = WalkLeg WalkLegInfo
  | MetroLeg MetroLegInfo
  | TaxiLeg TaxiLegInfo
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MultiModalRouteLegStatus
  = Pending
  | Completed
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TaxiLegInfo = TaxiLegInfo {endLocation :: Domain.Types.Location.LocationAPIEntity, legStage :: TaxiLegStage, startLocation :: Domain.Types.Location.LocationAPIEntity}
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TaxiLegStage
  = SearchStage (Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest) (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate))
  | SearchingForDriverStage (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate)
  | PickupStage (Kernel.Types.Id.Id Domain.Types.Booking.Booking)
  | OngoingStage (Kernel.Types.Id.Id Domain.Types.Booking.Booking)
  | CompletedStage
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data WalkLegInfo = WalkLegInfo {endLocation :: Domain.Types.Location.LocationAPIEntity, startLocation :: Domain.Types.Location.LocationAPIEntity}
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
