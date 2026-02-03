{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.NearbyBuses where

import qualified BecknV2.FRFS.Enums
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.RouteStopTimeTable
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Price
import qualified Kernel.Types.Time
import Servant
import qualified Storage.CachedQueries.Merchant.MultiModalBus
import Tools.Auth

data NearbyBus = NearbyBus
  { bearing :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    currentLocation :: Kernel.External.Maps.Types.LatLong,
    distance :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    routeCode :: Data.Text.Text,
    routeState :: Kernel.Prelude.Maybe Storage.CachedQueries.Merchant.MultiModalBus.RouteState,
    serviceSubTypes :: Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceSubType],
    serviceTierName :: Kernel.Prelude.Maybe Data.Text.Text,
    serviceType :: Kernel.Prelude.Maybe BecknV2.FRFS.Enums.ServiceTierType,
    shortName :: Kernel.Prelude.Maybe Data.Text.Text,
    vehicleNumber :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NearbyBusesRequest = NearbyBusesRequest
  { platformType :: Domain.Types.IntegratedBPPConfig.PlatformType,
    requireNearbyBuses :: Kernel.Prelude.Bool,
    requireRecentRide :: Kernel.Prelude.Bool,
    userLat :: Kernel.Prelude.Double,
    userLon :: Kernel.Prelude.Double,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NearbyBusesResponse = NearbyBusesResponse {nearbyBuses :: [NearbyBus], recentRides :: [RecentRide]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RecentRide = RecentRide {fare :: Kernel.Types.Price.Price, fromStopCode :: Data.Text.Text, routeCode :: Kernel.Prelude.Maybe Data.Text.Text, toStopCode :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RouteCodes = RouteCodes {routeCodes :: [Data.Text.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TimetableEntry = TimetableEntry
  { delay :: Kernel.Prelude.Maybe Kernel.Types.Time.Seconds,
    platformCode :: Kernel.Prelude.Maybe Data.Text.Text,
    serviceTierType :: BecknV2.FRFS.Enums.ServiceTierType,
    source :: Domain.Types.RouteStopTimeTable.SourceType,
    timeOfArrival :: Kernel.Prelude.TimeOfDay,
    timeOfDeparture :: Kernel.Prelude.TimeOfDay,
    tripId :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TimetableResponse = TimetableResponse {timetable :: [TimetableEntry]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
