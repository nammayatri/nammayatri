{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.TrackRoute (getTrackVehicles) where

import qualified API.Types.UI.TrackRoute as TrackRoute
import Data.Function
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.OpenApi (ToSchema)
import qualified Data.Text as DT
import qualified Data.Time
import qualified Data.Vector as V
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Route
import qualified Domain.Types.RouteStopMapping as DRSM
import qualified Domain.Types.RouteStopMapping as RouteStopMapping
import Domain.Types.RouteTripMapping as DRTM
import qualified Environment
import qualified EulerHS.Language
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import qualified Kernel.Types.APISuccess
import Kernel.Types.Distance
import qualified Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Kernel.Utils.Common (fromMaybeM)
import Servant
import qualified Storage.CachedQueries.Merchant.RiderConfig as CQRiderConfig
import qualified Storage.CachedQueries.Person as CQPerson
import Storage.Queries.Route as QRoute
import qualified Storage.Queries.RouteStopMapping as QRSM
import Storage.Queries.RouteTripMapping as RTM
import Storage.Queries.Station as QStation
import Tools.Auth
import Tools.Error
import Tools.Maps as Maps

getTrackVehicles ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow TrackRoute.TrackingResp
  )
getTrackVehicles (mbPersonId, merchantId) routeCode = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")

  initialRouteInfo :: [(Text, TrackRoute.VehicleInfoForRoute)] <-
    Redis.withCrossAppRedis $ Redis.hGetAll (makeRouteKey routeCode)

  currentUTC <- getCurrentTime
  personCityInfo <- CQPerson.findCityInfoById personId >>= fromMaybeM (PersonCityInformationNotFound personId.getId)
  riderConfigInfo <- CQRiderConfig.findByMerchantOperatingCityId personCityInfo.merchantOperatingCityId >>= fromMaybeM (RiderConfigNotFound personCityInfo.merchantOperatingCityId.getId)
  let staleThreshold = riderConfigInfo.routeTimestampExpiryTTL :: Kernel.Prelude.NominalDiffTime
  let isInfoStale (vInfo :: TrackRoute.VehicleInfoForRoute) =
        case vInfo.timestamp of
          Just timestampText ->
            case parseUTCTime (DT.unpack timestampText) of
              Just ts -> ts < addUTCTime (- staleThreshold) currentUTC
              Nothing -> True
          Nothing -> True

  let finlInitialRouteInfo = filter (not . isInfoStale . snd) initialRouteInfo
  routeInfoFromRedis <-
    if null finlInitialRouteInfo
      then do
        tripIds <- map DRTM.tripCode <$> RTM.findAllTripIdByRouteCode routeCode
        concat <$> forM tripIds (\tripId -> Redis.withCrossAppRedis $ Redis.hGetAll (makeTripKey tripId))
      else pure finlInitialRouteInfo

  let routeInfoWithLatLong :: [(Text, TrackRoute.VehicleInfoForRoute, Double, Double)] =
        mapMaybe (\(vId, vInfo) -> (vId,vInfo,,) <$> vInfo.latitude <*> vInfo.longitude) routeInfoFromRedis
  logDebug $ "got route from redis " <> show routeInfoFromRedis
  logDebug $ "got route from redis with lat/long " <> show routeInfoWithLatLong
  reqStops <- QRSM.findByRouteCode routeCode
  let sortedStops = sortBy (compare `on` RouteStopMapping.sequenceNum) reqStops
      stopPairs = map (\(x, y) -> (x, y)) (pairWithNext sortedStops)

  stopPairsWithWaypoints <- do
    res <- Redis.get (stopPairRoutePointsKey routeCode)
    case res of
      Just res' -> pure res'
      Nothing -> do
        res' <-
          forM stopPairs $ \stopPair -> do
            let firstStop = fst stopPair
                secondStop = snd stopPair
                request =
                  Maps.GetRoutesReq
                    { waypoints = NE.fromList [firstStop.stopPoint, secondStop.stopPoint],
                      calcPoints = True,
                      mode = Just Maps.CAR
                    }

            routeInfo <- Maps.getRoutes Nothing personId merchantId Nothing request
            reqRouteInfo <- listToMaybe routeInfo & fromMaybeM (InvalidRequest "Route not found")
            let wayPoints = reqRouteInfo.points
            pure (stopPair, wayPoints)
        Redis.set (stopPairRoutePointsKey routeCode) res'
        pure res'
  logDebug $ "stopPairsWithWaypoints" <> show stopPairsWithWaypoints
  trackingResp <-
    forM routeInfoWithLatLong $ \(vehicleId_, vi, vehicleLat, vehicleLong) -> do
      minDistancesWithPoint <-
        forM stopPairsWithWaypoints $ \stopPairWithWaypoints -> do
          distancesFromCurrLocation <-
            forM (snd stopPairWithWaypoints) $ \point -> do
              pure (highPrecMetersToMeters $ distanceBetweenInMeters (mkLatLong vehicleLat vehicleLong) point)
          let minDistance = minimum (pure distancesFromCurrLocation)
          logDebug $ "distancesFromCurrLocation " <> show distancesFromCurrLocation
          pure (minDistance, snd (fst stopPairWithWaypoints))
      let nextStop = List.minimumBy (comparing fst) minDistancesWithPoint
      pure $ TrackRoute.VehicleInfo {nextStop = snd nextStop, nextStopTravelTime = Just $ Seconds 300, vehicleId = vehicleId_, vehicleInfo = vi} -- TODO :: `nextStopTravelTime` should be calculated as (Avg Speed / Distance), where Average speed should be stored by Bus Tracking in VehicleInfo ?
  pure $ TrackRoute.TrackingResp {vehicleTrackingInfo = trackingResp}
  where
    parseUTCTime :: String -> Maybe UTCTime
    parseUTCTime ts = Data.Time.parseTimeM True Data.Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" ts

mkLatLong :: Double -> Double -> Maps.LatLong
mkLatLong lat_ lon_ =
  Maps.LatLong
    { lat = lat_,
      lon = lon_
    }

pairWithNext :: [a] -> [(a, a)]
pairWithNext xs = zip xs (List.tail xs)

makeRouteKey :: Text -> Text
makeRouteKey routeId = "route:" <> routeId

makeTripKey :: Text -> Text
makeTripKey tripId = "trip:" <> tripId

stopPairRoutePointsKey :: Text -> Text
stopPairRoutePointsKey routeId = "Tk:SPRPK:" <> routeId
