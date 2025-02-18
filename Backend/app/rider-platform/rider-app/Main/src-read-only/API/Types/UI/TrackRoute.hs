{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.TrackRoute where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.RouteStopMapping
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Servant
import Tools.Auth

data TrackingResp = TrackingResp {vehicleTrackingInfo :: [VehicleInfo]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleInfo = VehicleInfo
  { nextStop :: Domain.Types.RouteStopMapping.RouteStopMapping,
    nextStopTravelDistance :: Kernel.Types.Common.Meters,
    nextStopTravelTime :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    vehicleId :: Kernel.Prelude.Text,
    vehicleInfo :: VehicleInfoForRoute
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleInfoForRoute = VehicleInfoForRoute
  { latitude :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    longitude :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    scheduleRelationship :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    speed :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    startDate :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    startTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    timestamp :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tripId :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
