module Domain.Action.UI.SharedCab
  ( getSharedCabRoutes,
    getSharedCabRoute,
  )
where

import qualified API.Types.UI.SharedCab as API
import qualified BecknV2.OnDemand.Enums as Enums
import Data.List (sortOn)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Route as DRoute
import qualified Environment
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import SharedLogic.SharedCab.Booking (liveSeatsOnVehicle)
import SharedLogic.SharedCab.LegState (isSharedCabAgency)
import SharedLogic.SharedCab.Plate (canonicalisePlate)
import qualified SharedLogic.SharedCab.Session as Session
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.Person as QP
import Tools.Error

-- | The city's shared-cab feed sits on its own config (`07` §5), told apart by its SHARED_CAB agency.
findSharedCabConfig :: Maybe (Id DP.Person) -> Environment.Flow (Maybe DIBC.IntegratedBPPConfig)
findSharedCabConfig mbPersonId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  city <- QP.findCityInfoById personId >>= fromMaybeM (PersonNotFound personId.getId)
  find (isSharedCabAgency . (.agencyKey))
    <$> SIBC.findAllIntegratedBPPConfig city.merchantOperatingCityId Enums.BUS DIBC.APPLICATION

mkRouteInfo :: DIBC.IntegratedBPPConfig -> DRoute.Route -> Environment.Flow API.SharedCabRouteInfo
mkRouteInfo integratedBppConfig route = do
  stops <- sortOn (.sequenceNum) <$> OTPRest.getRouteStopMappingByRouteCode route.code integratedBppConfig
  cabsRunning <- length <$> Session.activeSessionsOnRoute route.code
  pure
    API.SharedCabRouteInfo
      { routeCode = route.code,
        name = route.longName,
        stops = [API.SharedCabStop {code = s.stopCode, name = s.stopName, point = s.stopPoint, sequenceNum = s.sequenceNum} | s <- stops],
        polyline = route.polyline,
        cabsRunning
      }

getSharedCabRoutes :: (Maybe (Id DP.Person), Id DM.Merchant) -> Environment.Flow API.SharedCabRouteListResp
getSharedCabRoutes (mbPersonId, _) = do
  routes <-
    findSharedCabConfig mbPersonId >>= \case
      Nothing -> pure []
      Just integratedBppConfig -> OTPRest.getRoutesByGtfsId integratedBppConfig >>= mapM (mkRouteInfo integratedBppConfig)
  pure API.SharedCabRouteListResp {routes}

getSharedCabRoute :: (Maybe (Id DP.Person), Id DM.Merchant) -> Text -> Environment.Flow API.SharedCabRouteDetailResp
getSharedCabRoute (mbPersonId, _) routeCode = do
  integratedBppConfig <- findSharedCabConfig mbPersonId >>= fromMaybeM IntegratedBPPConfigNotFound
  route <- OTPRest.getRouteByRouteId integratedBppConfig routeCode >>= fromMaybeM (RouteNotFound routeCode)
  routeInfo <- mkRouteInfo integratedBppConfig route
  sessions <- Session.activeSessionsOnRoute routeCode
  positions <- livePositions
  cabs <- forM sessions $ \s -> do
    liveSeats <- liveSeatsOnVehicle s.vehicleNumber
    pure
      API.SharedCabLiveCab
        { plateLast4 = T.takeEnd 4 s.vehicleNumber,
          position = Map.lookup s.vehicleNumber positions,
          freeSeats = max 0 (s.capacity - s.walkupCount - liveSeats)
        }
  pure API.SharedCabRouteDetailResp {routeInfo, cabs}
  where
    -- A failed LTS read lists the cabs without positions rather than failing the route view.
    livePositions =
      withTryCatch "sharedCab:vehicleTrackingOnRoute" (LF.vehicleTrackingOnRoute (LF.ByRoute routeCode)) >>= \case
        Left err -> Map.empty <$ logError ("shared-cab route " <> routeCode <> ": LTS read failed: " <> show err)
        Right (vehicles :: [LT.VehicleTrackingOnRouteResp]) ->
          pure $ Map.fromList [(canonicalisePlate v.vehicleNumber, LatLong v.vehicleInfo.latitude v.vehicleInfo.longitude) | v <- vehicles]
