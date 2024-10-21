{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.TrackRoute where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.RouteStopMapping
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data TrackingResp = TrackingResp {vehicleTrackingInfo :: [API.Types.UI.TrackRoute.VehicleInfo]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleInfo = VehicleInfo
  { location :: Kernel.External.Maps.Types.LatLong,
    locationUpdateTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    nextStop :: Domain.Types.RouteStopMapping.RouteStopMapping,
    vehicleId :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
