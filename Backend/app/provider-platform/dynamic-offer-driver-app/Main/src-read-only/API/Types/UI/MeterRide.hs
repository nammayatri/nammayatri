{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.MeterRide where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Location
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Types.Common
import Servant
import Tools.Auth

data MeterRideAddDestinationReq = MeterRideAddDestinationReq
  { currentLatLong :: Kernel.External.Maps.Types.LatLong,
    destinationLatLong :: Kernel.External.Maps.Types.LatLong,
    destinationLocation :: Domain.Types.Location.LocationAddress
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MeterRideAddDestinationResp = MeterRideAddDestinationResp {estimatedDistance :: Kernel.Types.Common.Meters, estimatedFare :: Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
