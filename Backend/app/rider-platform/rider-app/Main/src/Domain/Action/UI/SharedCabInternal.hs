module Domain.Action.UI.SharedCabInternal
  ( getSharedCabRoutes,
    postSharedCabRouteSelect,
    getSharedCabSession,
    postSharedCabSeats,
    postSharedCabRouteEnd,
    postSharedCabResume,
  )
where

import qualified API.Types.UI.SharedCabInternal as API
import Data.List (sortOn)
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Route as DRoute
import qualified Domain.Types.RouteStopMapping as DRSM
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState
import qualified Storage.CachedQueries.IntegratedBPPConfig as CQIBC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Tools.Error

-- rider_config.defaultCapacity default (Alto); read from config once that field exists.
defaultCapacity :: Int
defaultCapacity = 4

checkToken :: Maybe Text -> Environment.Flow ()
checkToken mbToken = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == mbToken) $
    throwError $ AuthBlocked "Invalid BPP internal api key"

getIntegratedBppConfig :: Id DIBC.IntegratedBPPConfig -> Environment.Flow DIBC.IntegratedBPPConfig
getIntegratedBppConfig ibcId = CQIBC.findById ibcId >>= fromMaybeM IntegratedBPPConfigNotFound

routeStops :: DIBC.IntegratedBPPConfig -> Text -> Environment.Flow [DRSM.RouteStopMapping]
routeStops integratedBppConfig code = sortOn (.sequenceNum) <$> OTPRest.getRouteStopMappingByRouteCode code integratedBppConfig

feedRoutes :: DIBC.IntegratedBPPConfig -> Environment.Flow [(DRoute.Route, [DRSM.RouteStopMapping])]
feedRoutes integratedBppConfig = do
  routes <- OTPRest.getRoutesByGtfsId integratedBppConfig
  forM routes $ \route -> (route,) <$> routeStops integratedBppConfig route.code

-- | Routes are one per direction, so a route's direction is where it ends.
routeDirection :: [DRSM.RouteStopMapping] -> Text
routeDirection = maybe "" (.stopName) . listToMaybe . reverse

-- | Every route of the feed, nearest stop first; demand ranking and stand pinning join later.
getSharedCabRoutes :: Text -> Double -> Double -> Maybe Text -> Environment.Flow API.SharedCabRoutesResp
getSharedCabRoutes ibcId driverLat driverLon mbToken = do
  checkToken mbToken
  routes <- getIntegratedBppConfig (Id ibcId) >>= feedRoutes
  let distanceFrom = distanceBetweenInMeters (LatLong driverLat driverLon)
      distanceKm route stops =
        realToFrac . (/ 1000) . foldl' min (distanceFrom route.startPoint) $ map distanceFrom (route.endPoint : map (.stopPoint) stops)
      toResp (route, stops) =
        API.SharedCabRoute
          { code = route.code,
            name = route.longName,
            direction = routeDirection stops,
            fromStop = maybe "" (.stopName) (listToMaybe stops),
            toStop = routeDirection stops,
            distanceKm = Just (distanceKm route stops),
            isStandRoute = False
          }
  pure API.SharedCabRoutesResp {routes = sortOn (.distanceKm) (map toResp routes)}

-- | `mode` only matters once riders can be on board; until allocation ships a change always applies.
postSharedCabRouteSelect :: Maybe Text -> API.SelectRouteReq -> Environment.Flow API.SelectRouteResp
postSharedCabRouteSelect mbToken req = do
  checkToken mbToken
  integratedBppConfig <- getIntegratedBppConfig req.integratedBppConfigId
  session <-
    Session.selectRoute
      OpenSessionReq
        { driverId = req.driverId,
          vehicleNumber = req.vehicleNumber,
          merchantId = integratedBppConfig.merchantId,
          merchantOperatingCityId = integratedBppConfig.merchantOperatingCityId,
          integratedBppConfigId = integratedBppConfig.id,
          serviceTierType = req.serviceTierType,
          capacity = fromMaybe defaultCapacity req.capacity,
          routeCode = req.routeCode
        }
  session' <-
    if req.walkupCount == session.walkupCount
      then pure session
      else Session.setWalkupCount req.driverId req.vehicleNumber session.version req.walkupCount
  resp <- mkSessionResp session'
  pure API.SelectRouteResp {session = Just resp, affectedRiders = Nothing}

ownSession :: Text -> Text -> Environment.Flow Session
ownSession driver plate = Session.getSession plate >>= either throwError pure . ownedSession driver

getSharedCabSession :: Text -> Text -> Maybe Text -> Environment.Flow API.SharedCabSession
getSharedCabSession driver plate mbToken = do
  checkToken mbToken
  ownSession driver plate >>= mkSessionResp

postSharedCabSeats :: Maybe Text -> API.SeatsReq -> Environment.Flow API.SharedCabSession
postSharedCabSeats mbToken req = do
  checkToken mbToken
  Session.setWalkupCount req.driverId req.vehicleNumber req.version req.walkupCount >>= mkSessionResp

-- | CHANGE leaves the session as is: the driver picks the next route with route/select, which closes this run.
postSharedCabRouteEnd :: Maybe Text -> API.EndRouteReq -> Environment.Flow (Maybe API.SharedCabSession)
postSharedCabRouteEnd mbToken req = do
  checkToken mbToken
  case req.next of
    API.RETURN -> do
      current <- ownSession req.driverId req.vehicleNumber
      routes <- getIntegratedBppConfig current.integratedBppConfigId >>= feedRoutes
      returnRoute <-
        fromMaybeM (InvalidRequest $ "No return route for " <> current.routeCode) $
          returnRouteOf current.routeCode [(route.code, map (.stopCode) stops) | (route, stops) <- routes]
      Just <$> (Session.endRoute req.driverId req.vehicleNumber (StartReturn returnRoute) >>= mkSessionResp)
    API.CHANGE -> Just <$> (ownSession req.driverId req.vehicleNumber >>= mkSessionResp)
    API.END -> Nothing <$ Session.endRoute req.driverId req.vehicleNumber EndForNow

postSharedCabResume :: Maybe Text -> API.SharedCabDriverReq -> Environment.Flow API.SharedCabSession
postSharedCabResume mbToken req = do
  checkToken mbToken
  Session.resume req.driverId req.vehicleNumber >>= mkSessionResp

sessionRoute :: DIBC.IntegratedBPPConfig -> Text -> Environment.Flow API.SessionRoute
sessionRoute integratedBppConfig code = do
  mbRoute <- OTPRest.getRouteByRouteId integratedBppConfig code
  stops <- routeStops integratedBppConfig code
  pure
    API.SessionRoute
      { code,
        name = maybe code (.longName) mbRoute,
        direction = routeDirection stops,
        nextStops = map (.stopName) stops
      }

-- | Until the tick and allocation land: movement is MOVING, next stops are the whole route,
-- riders/demand are empty and `available` counts walk-ups only.
mkSessionResp :: Session -> Environment.Flow API.SharedCabSession
mkSessionResp s = do
  integratedBppConfig <- getIntegratedBppConfig s.integratedBppConfigId
  route <- sessionRoute integratedBppConfig s.routeCode
  queuedRoute <- traverse (sessionRoute integratedBppConfig) s.queuedRouteCode
  pure
    API.SharedCabSession
      { route,
        queuedRoute,
        status = s.status,
        pauseReason = s.pauseReason,
        movement = MOVING,
        capacity = s.capacity,
        walkupCount = s.walkupCount,
        available = s.capacity - s.walkupCount,
        version = s.version,
        ridersByStop = [],
        demandAhead = [],
        lowDemandCard = Nothing,
        offRoute = Nothing
      }
