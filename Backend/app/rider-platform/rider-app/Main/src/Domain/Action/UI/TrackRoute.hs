module Domain.Action.UI.TrackRoute (getTrackVehicles) where

import qualified API.Types.UI.TrackRoute as TrackRoute
import Data.Function
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.RouteStopMapping as RouteStopMapping
import Domain.Types.RouteTripMapping as DRTM
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Distance
import qualified Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified Storage.Queries.RouteStopMapping as QRSM
import Storage.Queries.RouteTripMapping as RTM
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

  routeInfoByRouteCode :: [(Text, TrackRoute.VehicleInfoForRoute)] <- do
    vehicleTrackingResp <- LF.vehicleTrackingOnRoute (LF.ByRoute routeCode)
    pure $ mkVehicleInfoForRoute vehicleTrackingResp

  routeInfo' <-
    if null routeInfoByRouteCode
      then do
        tripIds <- map DRTM.tripCode <$> RTM.findAllTripIdByRouteCode routeCode
        vehicleTrackingResp <- LF.vehicleTrackingOnRoute (LF.ByTrips tripIds)
        pure $ mkVehicleInfoForRoute vehicleTrackingResp
      else pure routeInfoByRouteCode

  let routeInfoWithLatLong :: [(Text, TrackRoute.VehicleInfoForRoute, Double, Double)] =
        mapMaybe (\(vId, vInfo) -> (vId,vInfo,,) <$> vInfo.latitude <*> vInfo.longitude) routeInfo'
  logDebug $ "got route from lts " <> show routeInfo'
  logDebug $ "got route from lts with lat/long " <> show routeInfoWithLatLong
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
            reqRouteInfo <- listToMaybe routeInfo & fromMaybeM (RouteNotFound routeCode)
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
    mkVehicleInfoForRoute :: [LT.VehicleTrackingOnRouteResp] -> [(Text, TrackRoute.VehicleInfoForRoute)]
    mkVehicleInfoForRoute vehiclesInfo =
      vehiclesInfo
        <&> ( \vehicleInfo ->
                ( vehicleInfo.vehicleNumber,
                  TrackRoute.VehicleInfoForRoute
                    { latitude = Just vehicleInfo.vehicleInfo.latitude,
                      longitude = Just vehicleInfo.vehicleInfo.longitude,
                      scheduleRelationship = vehicleInfo.vehicleInfo.scheduleRelationship,
                      speed = vehicleInfo.vehicleInfo.speed,
                      startDate =
                        ( \startTime ->
                            T.pack $
                              Time.formatTime
                                Time.defaultTimeLocale
                                "%d-%m-%Y"
                                ( addUTCTime
                                    (secondsToNominalDiffTime 19800)
                                    startTime
                                )
                        )
                          <$> vehicleInfo.vehicleInfo.startTime,
                      startTime = vehicleInfo.vehicleInfo.startTime,
                      timestamp = Just vehicleInfo.vehicleInfo.timestamp,
                      tripId = vehicleInfo.vehicleInfo.tripId
                    }
                )
            )

mkLatLong :: Double -> Double -> Maps.LatLong
mkLatLong lat_ lon_ =
  Maps.LatLong
    { lat = lat_,
      lon = lon_
    }

pairWithNext :: [a] -> [(a, a)]
pairWithNext xs = zip xs (List.tail xs)

stopPairRoutePointsKey :: Text -> Text
stopPairRoutePointsKey routeId = "Tk:SPRPK:" <> routeId
