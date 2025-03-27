{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.NearbyBuses where

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
  { capacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    currentLocation :: Kernel.External.Maps.Types.LatLong,
    distance :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    eta :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    nextStop :: Kernel.Prelude.Maybe Data.Text.Text,
    occupancy :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    routeCode :: Data.Text.Text,
    routeLongName :: Data.Text.Text,
    routeShortName :: Data.Text.Text,
    serviceType :: Kernel.Prelude.Maybe Data.Text.Text,
    vehicleNumber :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NearbyBusesRequest = NearbyBusesRequest {platformType :: Domain.Types.IntegratedBPPConfig.PlatformType, requireRecentRide :: Kernel.Prelude.Bool, userLat :: Kernel.Prelude.Double, userLon :: Kernel.Prelude.Double}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NearbyBusesResponse = NearbyBusesResponse {nearbyBuses :: [NearbyBus], recentRides :: [RecentRide]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RecentRide = RecentRide
  { fare :: Kernel.Types.Price.Price,
    fromStopCode :: Data.Text.Text,
    fromStopName :: Data.Text.Text,
    routeCode :: Kernel.Prelude.Maybe Data.Text.Text,
    toStopCode :: Data.Text.Text,
    toStopName :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
