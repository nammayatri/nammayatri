{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.NearbyBuses where

import qualified BecknV2.FRFS.Enums
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.IntegratedBPPConfig
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Price
import Servant
import Tools.Auth

data NearbyBus = NearbyBus
  { currentLocation :: Kernel.External.Maps.Types.LatLong,
    vehicleNumber :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FilteredNearbyBus = FilteredNearbyBus
  { capacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    currentLocation :: Kernel.External.Maps.Types.LatLong,
    distance :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    eta :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    nextStop :: Kernel.Prelude.Maybe Data.Text.Text,
    occupancy :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    routeCode :: Data.Text.Text,
    serviceType :: Kernel.Prelude.Maybe Data.Text.Text,
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

data TimetableEntry = TimetableEntry {serviceTierType :: BecknV2.FRFS.Enums.ServiceTierType, timeOfArrival :: Kernel.Prelude.TimeOfDay, timeOfDeparture :: Kernel.Prelude.TimeOfDay}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TimetableResponse = TimetableResponse {timetable :: [TimetableEntry]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
