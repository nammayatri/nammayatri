{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.TrackRoute where

import qualified BecknV2.FRFS.Enums
import Data.OpenApi (ToSchema)
import qualified Domain.Types.RouteStopMapping
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Servant
import qualified SharedLogic.External.LocationTrackingService.Types
import qualified SharedLogic.FRFSUtils
import qualified Storage.CachedQueries.Merchant.MultiModalBus
import Tools.Auth

data TrackingResp = TrackingResp {vehicleTrackingInfo :: [VehicleInfo]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleInfo = VehicleInfo
  { delay :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    nextStop :: Kernel.Prelude.Maybe Domain.Types.RouteStopMapping.RouteStopMapping,
    nextStopTravelDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    nextStopTravelTime :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    routeCode :: Kernel.Prelude.Text,
    routeShortName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceTierType :: Kernel.Prelude.Maybe BecknV2.FRFS.Enums.ServiceTierType,
    upcomingStops :: [SharedLogic.FRFSUtils.UpcomingStop],
    vehicleId :: Kernel.Prelude.Text,
    vehicleInfo :: VehicleInfoForRoute
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleInfoForRoute = VehicleInfoForRoute
  { latitude :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    longitude :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    routeState :: Kernel.Prelude.Maybe Storage.CachedQueries.Merchant.MultiModalBus.RouteState,
    scheduleRelationship :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    speed :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    startDate :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    startTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    timestamp :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tripId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    upcomingStops :: Kernel.Prelude.Maybe [SharedLogic.External.LocationTrackingService.Types.UpcomingStop]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
