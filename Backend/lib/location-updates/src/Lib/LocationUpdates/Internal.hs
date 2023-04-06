{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.LocationUpdates.Internal
  ( RideInterpolationHandler (..),
    recalcDistanceBatches,
    addPointsImplementation,
    getWaypointsNumberImplementation,
    getFirstNwaypointsImplementation,
    getAllWaypointsImplementation,
    deleteFirstNwaypointsImplementation,
    interpolatePointsAndCalculateDistanceImplementation,
    clearLocationUpdatesImplementation,
    addInterpolatedPointsImplementation,
    clearInterpolatedPointsImplementation,
    expireInterpolatedPointsImplementation,
    getInterpolatedPointsImplementation,
    isDistanceCalculationFailedImplementation,
    wrapDistanceCalculationImplementation,
    processWaypoints,
    mkRideInterpolationHandler,
  )
where

import qualified Control.Monad.Catch as C
import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra
import Kernel.External.Maps as Maps
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Id (Id)
import Kernel.Utils.Common

data RideInterpolationHandler person m = RideInterpolationHandler
  { batchSize :: Integer,
    addPoints :: Id person -> NonEmpty LatLong -> m (),
    clearLocationUpdates :: Id person -> m (),
    getWaypointsNumber :: Id person -> m Integer,
    getFirstNwaypoints :: Id person -> Integer -> m [LatLong],
    getAllWaypoints :: Id person -> m [LatLong],
    deleteFirstNwaypoints :: Id person -> Integer -> m (),
    addInterpolatedPoints :: Id person -> NonEmpty LatLong -> m (),
    clearInterpolatedPoints :: Id person -> m (),
    expireInterpolatedPoints :: Id person -> m (),
    getInterpolatedPoints :: Id person -> m [LatLong],
    interpolatePointsAndCalculateDistance :: [LatLong] -> m (HighPrecMeters, [LatLong], MapsService),
    wrapDistanceCalculation :: Id person -> m () -> m (),
    isDistanceCalculationFailed :: Id person -> m Bool,
    updateDistance :: Id person -> HighPrecMeters -> Int -> Int -> m (),
    updateRouteDeviation :: Id person -> [LatLong] -> m Bool
  }

--------------------------------------------------------------------------------

wrapDistanceCalculationImplementation :: (C.MonadMask m, Log m, HedisFlow m r) => Id person -> m () -> m ()
wrapDistanceCalculationImplementation driverId action =
  action `C.catchAll` \e -> C.mask_ $ do
    logError $ "failed distance calculation: " <> show e
    let oneDayInSeconds = 60 * 60 * 24
    Hedis.setExp (getFailedDistanceCalculationKey driverId) () oneDayInSeconds

getFailedDistanceCalculationKey :: Id person -> Text
getFailedDistanceCalculationKey driverId = mconcat [driverId.getId, ":locationUpdatesFailed"]

isDistanceCalculationFailedImplementation :: (HedisFlow m r) => Id person -> m Bool
isDistanceCalculationFailedImplementation driverId = isJust <$> Hedis.get @() (getFailedDistanceCalculationKey driverId)

resetFailedDistanceCalculationFlag :: (HedisFlow m r) => Id person -> m ()
resetFailedDistanceCalculationFlag driverId = Hedis.del $ getFailedDistanceCalculationKey driverId

processWaypoints ::
  (Log m, MonadThrow m) =>
  RideInterpolationHandler person m ->
  Id person ->
  Bool ->
  Meters ->
  Bool ->
  NonEmpty LatLong ->
  m ()
processWaypoints ih@RideInterpolationHandler {..} driverId ending estDist pickupDropOutsideThreshold waypoints = do
  calculationFailed <- isDistanceCalculationFailed driverId
  if calculationFailed
    then logWarning "Failed to calculate actual distance for this ride, ignoring"
    else wrapDistanceCalculation driverId $ do
      addPoints driverId waypoints
      when ending $ recalcDistanceBatches ih ending driverId estDist pickupDropOutsideThreshold

recalcDistanceBatches ::
  (Monad m, Log m) =>
  RideInterpolationHandler person m ->
  Bool ->
  Id person ->
  Meters ->
  Bool ->
  m ()
recalcDistanceBatches h@RideInterpolationHandler {..} ending driverId estDist pickupDropOutsideThreshold = do
  waypoints <- getAllWaypoints driverId
  routeDeviation <- updateRouteDeviation driverId (toList waypoints)
  if routeDeviation || pickupDropOutsideThreshold
    then do
      (distanceToUpdate, googleSnapToRoadCalls, osrmSnapToRoadCalls) <- recalcDistanceBatches' 0 0 0
      updateDistance driverId distanceToUpdate googleSnapToRoadCalls osrmSnapToRoadCalls
    else updateDistance driverId (metersToHighPrecMeters estDist) 0 0
  where
    -- atLeastBatchPlusOne = (> batchSize) <$> getWaypointsNumber driverId
    pointsRemaining = (> 0) <$> getWaypointsNumber driverId
    continueCondition = if ending then pointsRemaining else return False

    recalcDistanceBatches' acc googleSnapToRoadCalls osrmSnapToRoadCalls = do
      batchLeft <- continueCondition
      if batchLeft
        then do
          (dist, serviceProvider) <- recalcDistanceBatchStep h driverId
          case serviceProvider of
            Google -> recalcDistanceBatches' (acc + dist) (googleSnapToRoadCalls + 1) osrmSnapToRoadCalls
            OSRM -> recalcDistanceBatches' (acc + dist) googleSnapToRoadCalls (osrmSnapToRoadCalls + 1)
            _ -> recalcDistanceBatches' (acc + dist) googleSnapToRoadCalls osrmSnapToRoadCalls
        else pure (acc, googleSnapToRoadCalls, osrmSnapToRoadCalls)

recalcDistanceBatchStep ::
  (Monad m, Log m) =>
  RideInterpolationHandler person m ->
  Id person ->
  m (HighPrecMeters, MapsService)
recalcDistanceBatchStep RideInterpolationHandler {..} driverId = do
  batchWaypoints <- getFirstNwaypoints driverId (batchSize + 1)
  (distance, interpolatedWps, serviceProvider) <- interpolatePointsAndCalculateDistance batchWaypoints
  whenJust (nonEmpty interpolatedWps) $ \nonEmptyInterpolatedWps -> do
    addInterpolatedPoints driverId nonEmptyInterpolatedWps
  logInfo $ mconcat ["points interpolation: input=", show batchWaypoints, "; output=", show interpolatedWps]
  logInfo $ mconcat ["calculated distance for ", show (length interpolatedWps), " points, ", "distance is ", show distance]
  deleteFirstNwaypoints driverId batchSize
  pure (distance, serviceProvider)

-------------------------------------------------------------------------
mkRideInterpolationHandler ::
  ( HedisFlow m r,
    HasPrettyLogger m r,
    HasCallStack,
    Metrics.CoreMetrics m,
    EncFlow m r,
    HasFlowEnv m r '["snapToRoadSnippetThreshold" ::: HighPrecMeters]
  ) =>
  Bool ->
  (Id person -> HighPrecMeters -> Int -> Int -> m ()) ->
  (Id person -> [LatLong] -> m Bool) ->
  (Maps.SnapToRoadReq -> m (Maps.MapsService, Maps.SnapToRoadResp)) ->
  RideInterpolationHandler person m
mkRideInterpolationHandler isEndRide updateDistance updateRouteDeviation snapToRoadCall =
  RideInterpolationHandler
    { batchSize = 98,
      addPoints = addPointsImplementation,
      clearLocationUpdates = clearLocationUpdatesImplementation,
      getWaypointsNumber = getWaypointsNumberImplementation,
      getFirstNwaypoints = getFirstNwaypointsImplementation,
      getAllWaypoints = getAllWaypointsImplementation,
      deleteFirstNwaypoints = deleteFirstNwaypointsImplementation,
      addInterpolatedPoints = addInterpolatedPointsImplementation,
      clearInterpolatedPoints = clearInterpolatedPointsImplementation,
      getInterpolatedPoints = getInterpolatedPointsImplementation,
      expireInterpolatedPoints = expireInterpolatedPointsImplementation,
      interpolatePointsAndCalculateDistance = interpolatePointsAndCalculateDistanceImplementation isEndRide snapToRoadCall,
      updateDistance,
      updateRouteDeviation,
      isDistanceCalculationFailed = isDistanceCalculationFailedImplementation,
      wrapDistanceCalculation = wrapDistanceCalculationImplementation
    }

makeWaypointsRedisKey :: Id person -> Text
makeWaypointsRedisKey driverId = mconcat ["waypoints", ":", driverId.getId]

addPointsImplementation :: (HedisFlow m env) => Id person -> NonEmpty LatLong -> m ()
addPointsImplementation driverId waypoints = do
  let key = makeWaypointsRedisKey driverId
      numPoints = length waypoints
  rPush key waypoints
  logInfo $ mconcat ["added ", show numPoints, " points for driverId = ", driverId.getId]

clearLocationUpdatesImplementation :: (HedisFlow m env) => Id person -> m ()
clearLocationUpdatesImplementation driverId = do
  let key = makeWaypointsRedisKey driverId
  clearList key
  logInfo $ mconcat ["cleared location updates for driverId = ", driverId.getId]
  resetFailedDistanceCalculationFlag driverId

getWaypointsNumberImplementation :: (HedisFlow m env) => Id person -> m Integer
getWaypointsNumberImplementation = lLen . makeWaypointsRedisKey

getFirstNwaypointsImplementation :: (HedisFlow m env) => Id person -> Integer -> m [LatLong]
getFirstNwaypointsImplementation driverId num = lRange (makeWaypointsRedisKey driverId) 0 (num - 1)

getAllWaypointsImplementation :: (HedisFlow m env) => Id person -> m [LatLong]
getAllWaypointsImplementation driverId = lRange (makeWaypointsRedisKey driverId) 0 (-1)

deleteFirstNwaypointsImplementation :: (HedisFlow m env) => Id person -> Integer -> m ()
deleteFirstNwaypointsImplementation driverId numToDel = lTrim (makeWaypointsRedisKey driverId) numToDel (-1)

makeInterpolatedPointsRedisKey :: Id person -> Text
makeInterpolatedPointsRedisKey driverId = mconcat ["interpolatedPoints", ":", driverId.getId]

addInterpolatedPointsImplementation :: (HedisFlow m env) => Id person -> NonEmpty LatLong -> m ()
addInterpolatedPointsImplementation driverId waypoints = do
  let key = makeInterpolatedPointsRedisKey driverId
      numPoints = length waypoints
  rPush key waypoints
  logInfo $ mconcat ["added ", show numPoints, " interpolated points for driverId = ", driverId.getId]

clearInterpolatedPointsImplementation :: HedisFlow m env => Id person -> m ()
clearInterpolatedPointsImplementation driverId = do
  let key = makeInterpolatedPointsRedisKey driverId
  clearList key
  logInfo $ mconcat ["cleared interpolated location updates for driverId = ", driverId.getId]

getInterpolatedPointsImplementation :: HedisFlow m env => Id person -> m [LatLong]
getInterpolatedPointsImplementation = Hedis.getList . makeInterpolatedPointsRedisKey

expireInterpolatedPointsImplementation :: HedisFlow m env => Id person -> m ()
expireInterpolatedPointsImplementation driverId = do
  let key = makeInterpolatedPointsRedisKey driverId
  Hedis.expire key 86400 -- 24 hours

interpolatePointsAndCalculateDistanceImplementation ::
  ( HasCallStack,
    EncFlow m r,
    Metrics.CoreMetrics m,
    HasFlowEnv m r '["snapToRoadSnippetThreshold" ::: HighPrecMeters]
  ) =>
  Bool ->
  (Maps.SnapToRoadReq -> m (Maps.MapsService, Maps.SnapToRoadResp)) ->
  [LatLong] ->
  m (HighPrecMeters, [LatLong], Maps.MapsService)
interpolatePointsAndCalculateDistanceImplementation isEndRide snapToRoadCall wps = do
  if isEndRide && isAllPointsEqual wps
    then pure (0, take 1 wps, Maps.Google)
    else do
      (service, res) <- snapToRoadCall $ Maps.SnapToRoadReq {points = wps}
      pure (res.distance, res.snappedPoints, service)

isAllPointsEqual :: [LatLong] -> Bool
isAllPointsEqual [] = True
isAllPointsEqual [_] = True
isAllPointsEqual (x : xs) = all (\t -> (abs (x.lat - t.lat) <= eps) && (abs (x.lon - t.lon) <= eps)) xs

eps :: Double
eps = 0.0001
