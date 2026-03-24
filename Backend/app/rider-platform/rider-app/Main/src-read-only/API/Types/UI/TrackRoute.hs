{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Types.UI.TrackRoute where
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth
import Data.OpenApi (ToSchema)
import qualified Kernel.Prelude
import qualified Storage.CachedQueries.Merchant.MultiModalBus
import qualified BecknV2.FRFS.Enums
import qualified SharedLogic.External.LocationTrackingService.Types
import qualified Kernel.Types.Common
import qualified Domain.Types.RouteStopMapping
import qualified SharedLogic.FRFSUtils



data TrackingResp
    = TrackingResp {vehicleTrackingInfo :: [VehicleTrackingInfo]}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data VehicleInfoForRoute
    = VehicleInfoForRoute {latitude :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
                           longitude :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
                           routeState :: Kernel.Prelude.Maybe Storage.CachedQueries.Merchant.MultiModalBus.RouteState,
                           scheduleRelationship :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                           serviceSubTypes :: Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceSubType],
                           speed :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
                           startDate :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                           startTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
                           timestamp :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                           tripId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                           upcomingStops :: Kernel.Prelude.Maybe [SharedLogic.External.LocationTrackingService.Types.UpcomingStop]}
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data VehicleTrackingInfo
    = VehicleTrackingInfo {delay :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
                           nextStop :: Kernel.Prelude.Maybe Domain.Types.RouteStopMapping.RouteStopMapping,
                           nextStopTravelDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
                           nextStopTravelTime :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
                           routeCode :: Kernel.Prelude.Text,
                           routeShortName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                           serviceTierType :: Kernel.Prelude.Maybe BecknV2.FRFS.Enums.ServiceTierType,
                           upcomingStops :: [SharedLogic.FRFSUtils.UpcomingStop],
                           vehicleId :: Kernel.Prelude.Text,
                           vehicleInfo :: VehicleInfoForRoute}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)



