{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.NearbyBuses where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data NearbyBus = NearbyBus
  { busId :: Kernel.Prelude.Maybe Data.Text.Text,
    capacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    currentLocation :: Kernel.External.Maps.Types.LatLong,
    distance :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    eta :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    nextStop :: Kernel.Prelude.Maybe Data.Text.Text,
    occupancy :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    routeId :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NearbyBusesRequest = NearbyBusesRequest
  { merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    requireRecentRide :: Kernel.Prelude.Bool,
    userLat :: Kernel.Prelude.Double,
    userLon :: Kernel.Prelude.Double
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NearbyBusesResponse = NearbyBusesResponse {nearbyBuses :: [NearbyBus], recentRides :: [RecentRide]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RecentRide = RecentRide {fromStopCode :: Data.Text.Text, quantity :: Kernel.Prelude.Int, routeCode :: Kernel.Prelude.Maybe Data.Text.Text, toStopCode :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
