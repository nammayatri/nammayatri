module Domain.Action.UI.SharedCabInternal
  ( getSharedCabRoutes,
    postSharedCabRouteSelect,
    getSharedCabSession,
    postSharedCabSeats,
    postSharedCabRouteChange,
    postSharedCabRouteEnd,
    postSharedCabResume,
  )
where

import qualified API.Types.UI.SharedCabInternal as API
import Data.List (sortOn)
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

-- | Every route of the feed, nearest stop first; demand ranking joins once allocation exists.
getSharedCabRoutes :: Text -> Double -> Double -> Maybe Text -> Environment.Flow [API.SharedCabRouteResp]
getSharedCabRoutes ibcId driverLat driverLon mbToken = do
  checkToken mbToken
  integratedBppConfig <- CQIBC.findById (Id ibcId) >>= fromMaybeM IntegratedBPPConfigNotFound
  routes <- OTPRest.getRoutesByGtfsId integratedBppConfig
  resps <- forM routes $ \route -> do
    stops <- OTPRest.getRouteStopMappingByRouteCode route.code integratedBppConfig
    let points = route.endPoint : map (.stopPoint) stops
        distanceFrom = distanceBetweenInMeters (LatLong driverLat driverLon)
    pure
      API.SharedCabRouteResp
        { code = route.code,
          shortName = route.shortName,
          longName = route.longName,
          distanceMeters = realToFrac . foldl' min (distanceFrom route.startPoint) $ map distanceFrom points
        }
  pure $ sortOn (.distanceMeters) resps

postSharedCabRouteSelect :: Maybe Text -> API.SharedCabSelectReq -> Environment.Flow API.SharedCabSessionResp
postSharedCabRouteSelect mbToken req = do
  checkToken mbToken
  integratedBppConfig <- CQIBC.findById req.integratedBppConfigId >>= fromMaybeM IntegratedBPPConfigNotFound
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
  mkSessionResp <$> case req.walkupCount of
    Just count | count /= session.walkupCount -> Session.setWalkupCount req.driverId req.vehicleNumber session.version count
    _ -> pure session

getSharedCabSession :: Text -> Text -> Maybe Text -> Environment.Flow API.SharedCabSessionResp
getSharedCabSession driver plate mbToken = do
  checkToken mbToken
  session <- Session.getSession plate
  either throwError (pure . mkSessionResp) $ ownedSession driver session

postSharedCabSeats :: Maybe Text -> API.SharedCabSeatsReq -> Environment.Flow API.SharedCabSessionResp
postSharedCabSeats mbToken req = do
  checkToken mbToken
  mkSessionResp <$> Session.setWalkupCount req.driverId req.vehicleNumber req.version req.walkupCount

postSharedCabRouteChange :: Maybe Text -> API.SharedCabChangeRouteReq -> Environment.Flow API.SharedCabSessionResp
postSharedCabRouteChange mbToken req = do
  checkToken mbToken
  mkSessionResp <$> Session.changeRoute req.driverId req.vehicleNumber req.routeCode

postSharedCabRouteEnd :: Maybe Text -> API.SharedCabEndReq -> Environment.Flow API.SharedCabSessionResp
postSharedCabRouteEnd mbToken req = do
  checkToken mbToken
  mkSessionResp <$> case req.next of
    API.RETURN -> Session.endRoute req.driverId req.vehicleNumber (StartReturn req.routeCode)
    API.CHANGE -> do
      newRoute <- fromMaybeM (InvalidRequest "routeCode is required to change route") req.routeCode
      Session.changeRoute req.driverId req.vehicleNumber newRoute
    API.END -> Session.endRoute req.driverId req.vehicleNumber EndForNow

postSharedCabResume :: Maybe Text -> API.SharedCabDriverReq -> Environment.Flow API.SharedCabSessionResp
postSharedCabResume mbToken req = do
  checkToken mbToken
  mkSessionResp <$> Session.resume req.driverId req.vehicleNumber

-- | `available` counts walk-ups only until allocation adds seats held by bookings.
mkSessionResp :: Session -> API.SharedCabSessionResp
mkSessionResp s =
  API.SharedCabSessionResp
    { vehicleNumber = s.vehicleNumber,
      routeCode = s.routeCode,
      queuedRouteCode = s.queuedRouteCode,
      status = s.status,
      pauseReason = s.pauseReason,
      capacity = s.capacity,
      walkupCount = s.walkupCount,
      available = s.capacity - s.walkupCount,
      version = s.version,
      startedAt = s.startedAt,
      vehicleTripId = s.vehicleTripId
    }
