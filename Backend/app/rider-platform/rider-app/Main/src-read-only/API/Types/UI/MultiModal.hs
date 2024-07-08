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

data MetroLegInfo = MetroLegInfo {metroLines :: [API.Types.UI.MultiModal.MetroLineInfo]} deriving (Generic, ToJSON, FromJSON, ToSchema, Eq, Show)

data MetroLineInfo = MetroLineInfo
  { endStation :: API.Types.UI.MultiModal.MetroStationInfo,
    estimatedFare :: Kernel.Types.Common.PriceAPIEntity,
    lineColor :: Kernel.Prelude.Text,
    lineName :: Kernel.Prelude.Text,
    lineStations :: [API.Types.UI.MultiModal.MetroStationInfo],
    lineTimings :: API.Types.UI.MultiModal.MetroLineTiming,
    startStation :: API.Types.UI.MultiModal.MetroStationInfo
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Eq, Show)

data MetroLineTiming = MetroLineTiming {endTime :: Kernel.Prelude.TimeOfDay, frequency :: Kernel.Types.Common.Seconds, startTime :: Kernel.Prelude.TimeOfDay}
  deriving (Generic, ToJSON, FromJSON, ToSchema, Eq, Show)

data MetroStationInfo = MetroStationInfo
  { exit :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    platform :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    stationCode :: Kernel.Prelude.Text,
    stationLocation :: Kernel.External.Maps.LatLong,
    stationName :: Kernel.Prelude.Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Eq, Show)

data MultiModalEstimate = MultiModalEstimate
  { estimatedFare :: Kernel.Types.Common.PriceAPIEntity,
    id :: Kernel.Types.Id.Id Domain.Types.Estimate.Estimate,
    isValueAddNP :: Kernel.Prelude.Bool,
    serviceTierName :: Kernel.Prelude.Text,
    serviceTierShortDesc :: Kernel.Prelude.Text,
    travelModes :: [Domain.Types.Common.TravelMode],
    validTill :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Eq, Show)

data MultiModalRouteDetails = MultiModalRouteDetails
  { automaticBooking :: Kernel.Prelude.Bool,
    estimatedFare :: Kernel.Types.Common.PriceAPIEntity,
    routeLegs :: [API.Types.UI.MultiModal.MultiModalRouteLeg],
    totalDistance :: Kernel.Types.Common.Distance,
    totalDuration :: Kernel.Types.Common.Seconds
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Eq, Show)

data MultiModalRouteLeg = MultiModalRouteLeg
  { distance :: Kernel.Types.Common.Distance,
    duration :: Kernel.Types.Common.Seconds,
    legInfo :: API.Types.UI.MultiModal.MultiModalRouteLegInfo,
    status :: API.Types.UI.MultiModal.MultiModalRouteLegStatus
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Eq, Show)

data MultiModalRouteLegInfo
  = WalkLeg API.Types.UI.MultiModal.WalkLegInfo
  | MetroLeg API.Types.UI.MultiModal.MetroLegInfo
  | TaxiLeg API.Types.UI.MultiModal.TaxiLegInfo
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data MultiModalRouteLegStatus = Pending | Completed deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data TaxiLegInfo = TaxiLegInfo {endLocation :: Domain.Types.Location.LocationAPIEntity, legStage :: API.Types.UI.MultiModal.TaxiLegStage, startLocation :: Domain.Types.Location.LocationAPIEntity}
  deriving (Generic, ToJSON, FromJSON, ToSchema, Eq, Show)

data TaxiLegStage
  = SearchStage (Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest) (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate))
  | SearchingForDriverStage (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate)
  | PickupStage (Kernel.Types.Id.Id Domain.Types.Booking.Booking)
  | OngoingStage (Kernel.Types.Id.Id Domain.Types.Booking.Booking)
  | CompletedStage
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data WalkLegInfo = WalkLegInfo {endLocation :: Domain.Types.Location.LocationAPIEntity, startLocation :: Domain.Types.Location.LocationAPIEntity}
  deriving (Generic, ToJSON, FromJSON, ToSchema, Eq, Show)
