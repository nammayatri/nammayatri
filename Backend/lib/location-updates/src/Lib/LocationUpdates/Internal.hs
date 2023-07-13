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
    recalcPickupDistanceBatches,
    addPointsImplementation,
    addPickupPointsImplementation,
    getWaypointsNumberImplementation,
    getPickupWaypointsNumberImplementation,
    getFirstNwaypointsImplementation,
    getFirstNPickupwaypointsImplementation,
    deleteFirstNwaypointsImplementation,
    deleteFirstNPickupwaypointsImplementation,
    interpolatePointsAndCalculateDistanceImplementation,
    clearLocationUpdatesImplementation,
    clearPickupLocationUpdatesImplementation,
    addInterpolatedPointsImplementation,
    addPickupInterpolatedPointsImplementation,
    clearInterpolatedPointsImplementation,
    clearPickupInterpolatedPointsImplementation,
    expireInterpolatedPointsImplementation,
    expirePickupInterpolatedPointsImplementation,
    getInterpolatedPointsImplementation,
    getPickupInterpolatedPointsImplementation,
    isDistanceCalculationFailedImplementation,
    isPickupDistanceCalculationFailedImplementation,
    wrapDistanceCalculationImplementation,
    wrapPickupDistanceCalculationImplementation,
    processWaypoints,
    processPickupWaypoints,
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
    addPickupPoints :: Id person -> NonEmpty LatLong -> m (),
    clearLocationUpdates :: Id person -> m (),
    clearPickupLocationUpdates :: Id person -> m (),
    getWaypointsNumber :: Id person -> m Integer,
    getPickupWaypointsNumber :: Id person -> m Integer,
    getFirstNwaypoints :: Id person -> Integer -> m [LatLong],
    getFirstNPickupwaypoints :: Id person -> Integer -> m [LatLong],
    deleteFirstNwaypoints :: Id person -> Integer -> m (),
    deleteFirstNPickupwaypoints :: Id person -> Integer -> m (),
    addInterpolatedPoints :: Id person -> NonEmpty LatLong -> m (),
    addPickupInterpolatedPoints :: Id person -> NonEmpty LatLong -> m (),
    clearInterpolatedPoints :: Id person -> m (),
    clearPickupInterpolatedPoints :: Id person -> m (),
    expireInterpolatedPoints :: Id person -> m (),
    expirePickupInterpolatedPoints :: Id person -> m (),
    getInterpolatedPoints :: Id person -> m [LatLong],
    getPickupInterpolatedPoints :: Id person -> m [LatLong],
    interpolatePointsAndCalculateDistance :: [LatLong] -> m (HighPrecMeters, [LatLong]),
    wrapDistanceCalculation :: Id person -> m () -> m (),
    wrapPickupDistanceCalculation :: Id person -> m () -> m (),
    isDistanceCalculationFailed :: Id person -> m Bool,
    isPickupDistanceCalculationFailed :: Id person -> m Bool,
    updateDistance :: Id person -> HighPrecMeters -> m ()
  }

--------------------------------------------------------------------------------

wrapDistanceCalculationImplementation :: (C.MonadMask m, Log m, HedisFlow m r) => Id person -> m () -> m ()
wrapDistanceCalculationImplementation driverId action =
  action `C.catchAll` \e -> C.mask_ $ do
    logError $ "failed distance calculation: " <> show e
    let oneDayInSeconds = 60 * 60 * 24
    Hedis.setExp (getFailedDistanceCalculationKey driverId) () oneDayInSeconds

wrapPickupDistanceCalculationImplementation :: (C.MonadMask m, Log m, HedisFlow m r) => Id person -> m () -> m ()
wrapPickupDistanceCalculationImplementation driverId action = do
  action `C.catchAll` \e -> C.mask_ $ do
    logError $ "failed pickup distance calculation: " <> show e
    let oneDayInSeconds = 60 * 60 * 24
    Hedis.setExp (getFailedPickupDistanceCalculationKey driverId) () oneDayInSeconds

getFailedDistanceCalculationKey :: Id person -> Text
getFailedDistanceCalculationKey driverId = mconcat [driverId.getId, ":locationUpdatesFailed"]

getFailedPickupDistanceCalculationKey :: Id person -> Text
getFailedPickupDistanceCalculationKey driverId = mconcat [driverId.getId, ":pickupLocationUpdatesFailed"]

isDistanceCalculationFailedImplementation :: (HedisFlow m r) => Id person -> m Bool
isDistanceCalculationFailedImplementation driverId = isJust <$> Hedis.get @() (getFailedDistanceCalculationKey driverId)

isPickupDistanceCalculationFailedImplementation :: (HedisFlow m r) => Id person -> m Bool
isPickupDistanceCalculationFailedImplementation driverId = isJust <$> Hedis.get @() (getFailedPickupDistanceCalculationKey driverId)

resetFailedDistanceCalculationFlag :: (HedisFlow m r) => Id person -> m ()
resetFailedDistanceCalculationFlag driverId = Hedis.del $ getFailedDistanceCalculationKey driverId

resetFailedPickupDistanceCalculationFlag :: (HedisFlow m r) => Id person -> m ()
resetFailedPickupDistanceCalculationFlag driverId = Hedis.del $ getFailedPickupDistanceCalculationKey driverId

processWaypoints ::
  (Log m, MonadThrow m) =>
  RideInterpolationHandler person m ->
  Id person ->
  Bool ->
  NonEmpty LatLong ->
  m ()
processWaypoints ih@RideInterpolationHandler {..} driverId ending waypoints = do
  calculationFailed <- ih.isDistanceCalculationFailed driverId
  if calculationFailed
    then logWarning "Failed to calculate actual distance for this ride, ignoring"
    else ih.wrapDistanceCalculation driverId $ do
      addPoints driverId waypoints
      recalcDistanceBatches ih ending driverId

processPickupWaypoints ::
  (Log m, MonadThrow m) =>
  RideInterpolationHandler person m ->
  Id person ->
  Bool ->
  NonEmpty LatLong ->
  m ()
processPickupWaypoints ih@RideInterpolationHandler {..} driverId ending waypoints = do
  pickupCalculationFailed <- ih.isPickupDistanceCalculationFailed driverId
  if pickupCalculationFailed
    then logWarning "Failed to calculate actual pickup distance for this ride, ignoring"
    else ih.wrapPickupDistanceCalculation driverId $ do
      addPickupPoints driverId waypoints
      recalcPickupDistanceBatches ih ending driverId

recalcDistanceBatches ::
  (Monad m, Log m) =>
  RideInterpolationHandler person m ->
  Bool ->
  Id person ->
  m ()
recalcDistanceBatches h@RideInterpolationHandler {..} ending driverId = do
  distanceToUpdate <- recalcDistanceBatches' 0
  updateDistance driverId distanceToUpdate
  where
    atLeastBatchPlusOne = getWaypointsNumber driverId <&> (> batchSize)
    pointsRemaining = (> 0) <$> getWaypointsNumber driverId
    continueCondition =
      if ending
        then pointsRemaining
        else atLeastBatchPlusOne

    recalcDistanceBatches' acc = do
      batchLeft <- continueCondition
      if batchLeft
        then do
          dist <- recalcDistanceBatchStep h driverId
          recalcDistanceBatches' (acc + dist)
        else pure acc

recalcPickupDistanceBatches ::
  (Monad m, Log m) =>
  RideInterpolationHandler person m ->
  Bool ->
  Id person ->
  m ()
recalcPickupDistanceBatches h@RideInterpolationHandler {..} ending driverId = do
  distanceToUpdate <- recalcPickupDistanceBatches' 0
  updateDistance driverId distanceToUpdate
  pure ()
  where
    atLeastBatchPlusOne = getPickupWaypointsNumber driverId <&> (> batchSize)
    pointsRemaining = (> 0) <$> getPickupWaypointsNumber driverId
    continueCondition =
      if ending
        then pointsRemaining
        else atLeastBatchPlusOne

    recalcPickupDistanceBatches' acc = do
      batchLeft <- continueCondition
      if batchLeft
        then do
          dist <- recalcPickupDistanceBatchStep h driverId
          recalcPickupDistanceBatches' (acc + dist)
        else pure acc

recalcDistanceBatchStep ::
  (Monad m, Log m) =>
  RideInterpolationHandler person m ->
  Id person ->
  m HighPrecMeters
recalcDistanceBatchStep RideInterpolationHandler {..} driverId = do
  batchWaypoints <- getFirstNwaypoints driverId (batchSize + 1)
  (distance, interpolatedWps) <- interpolatePointsAndCalculateDistance batchWaypoints
  whenJust (nonEmpty interpolatedWps) $ \nonEmptyInterpolatedWps -> do
    addInterpolatedPoints driverId nonEmptyInterpolatedWps
  logInfo $ mconcat ["points interpolation: input=", show batchWaypoints, "; output=", show interpolatedWps]
  logInfo $ mconcat ["calculated distance for ", show (length interpolatedWps), " points, ", "distance is ", show distance]
  deleteFirstNwaypoints driverId batchSize
  pure distance

recalcPickupDistanceBatchStep ::
  (Monad m, Log m) =>
  RideInterpolationHandler person m ->
  Id person ->
  m HighPrecMeters
recalcPickupDistanceBatchStep RideInterpolationHandler {..} driverId = do
  batchWaypoints <- getFirstNPickupwaypoints driverId (batchSize + 1)
  (distance, interpolatedWps) <- interpolatePointsAndCalculateDistance batchWaypoints
  whenJust (nonEmpty interpolatedWps) $ \nonEmptyInterpolatedWps -> do
    addPickupInterpolatedPoints driverId nonEmptyInterpolatedWps
  logInfo $ mconcat ["pickup points interpolation: input=", show batchWaypoints, "; output=", show interpolatedWps]
  logInfo $ mconcat ["calculated pickup distance for ", show (length interpolatedWps), " points, ", "distance is ", show distance]
  deleteFirstNPickupwaypoints driverId batchSize
  pure distance

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
  MapsServiceConfig ->
  (Id person -> HighPrecMeters -> m ()) ->
  RideInterpolationHandler person m
mkRideInterpolationHandler isEndRide mapsCfg updateDistance =
  RideInterpolationHandler
    { batchSize = 98,
      addPoints = addPointsImplementation,
      addPickupPoints = addPickupPointsImplementation,
      clearLocationUpdates = clearLocationUpdatesImplementation,
      clearPickupLocationUpdates = clearPickupLocationUpdatesImplementation,
      getWaypointsNumber = getWaypointsNumberImplementation,
      getPickupWaypointsNumber = getPickupWaypointsNumberImplementation,
      getFirstNwaypoints = getFirstNwaypointsImplementation,
      getFirstNPickupwaypoints = getFirstNPickupwaypointsImplementation,
      deleteFirstNwaypoints = deleteFirstNwaypointsImplementation,
      deleteFirstNPickupwaypoints = deleteFirstNPickupwaypointsImplementation,
      addInterpolatedPoints = addInterpolatedPointsImplementation,
      addPickupInterpolatedPoints = addPickupInterpolatedPointsImplementation,
      clearInterpolatedPoints = clearInterpolatedPointsImplementation,
      clearPickupInterpolatedPoints = clearPickupInterpolatedPointsImplementation,
      getInterpolatedPoints = getInterpolatedPointsImplementation,
      getPickupInterpolatedPoints = getPickupInterpolatedPointsImplementation,
      expireInterpolatedPoints = expireInterpolatedPointsImplementation,
      expirePickupInterpolatedPoints = expirePickupInterpolatedPointsImplementation,
      interpolatePointsAndCalculateDistance = interpolatePointsAndCalculateDistanceImplementation isEndRide mapsCfg,
      updateDistance,
      isDistanceCalculationFailed = isDistanceCalculationFailedImplementation,
      isPickupDistanceCalculationFailed = isPickupDistanceCalculationFailedImplementation,
      wrapDistanceCalculation = wrapDistanceCalculationImplementation,
      wrapPickupDistanceCalculation = wrapPickupDistanceCalculationImplementation
    }

makeWaypointsRedisKey :: Id person -> Text
makeWaypointsRedisKey driverId = mconcat ["waypoints", ":", driverId.getId]

makePickupWaypointsRedisKey :: Id person -> Text
makePickupWaypointsRedisKey driverId = mconcat ["pickupwaypoints", ":", driverId.getId]

addPointsImplementation :: (HedisFlow m env) => Id person -> NonEmpty LatLong -> m ()
addPointsImplementation driverId waypoints = do
  let key = makeWaypointsRedisKey driverId
      numPoints = length waypoints
  rPush key waypoints
  logInfo $ mconcat ["added ", show numPoints, " points for driverId = ", driverId.getId]

addPickupPointsImplementation :: (HedisFlow m env) => Id person -> NonEmpty LatLong -> m ()
addPickupPointsImplementation driverId waypoints = do
  let key = makePickupWaypointsRedisKey driverId
      numPoints = length waypoints
  rPush key waypoints
  logInfo $ mconcat ["added ", show numPoints, " pickup points for driverId = ", driverId.getId]

clearLocationUpdatesImplementation :: (HedisFlow m env) => Id person -> m ()
clearLocationUpdatesImplementation driverId = do
  let key = makeWaypointsRedisKey driverId
  clearList key
  logInfo $ mconcat ["cleared location updates for driverId = ", driverId.getId]
  resetFailedDistanceCalculationFlag driverId

clearPickupLocationUpdatesImplementation :: (HedisFlow m env) => Id person -> m ()
clearPickupLocationUpdatesImplementation driverId = do
  let key = makePickupWaypointsRedisKey driverId
  clearList key
  logInfo $ mconcat ["cleared pickup location updates for driverId = ", driverId.getId]
  resetFailedPickupDistanceCalculationFlag driverId

getWaypointsNumberImplementation :: (HedisFlow m env) => Id person -> m Integer
getWaypointsNumberImplementation = lLen . makeWaypointsRedisKey

getPickupWaypointsNumberImplementation :: (HedisFlow m env) => Id person -> m Integer
getPickupWaypointsNumberImplementation = lLen . makePickupWaypointsRedisKey

getFirstNwaypointsImplementation :: (HedisFlow m env) => Id person -> Integer -> m [LatLong]
getFirstNwaypointsImplementation driverId num = lRange (makeWaypointsRedisKey driverId) 0 (num - 1)

getFirstNPickupwaypointsImplementation :: (HedisFlow m env) => Id person -> Integer -> m [LatLong]
getFirstNPickupwaypointsImplementation driverId num = lRange (makePickupWaypointsRedisKey driverId) 0 (num - 1)

deleteFirstNwaypointsImplementation :: (HedisFlow m env) => Id person -> Integer -> m ()
deleteFirstNwaypointsImplementation driverId numToDel = lTrim (makeWaypointsRedisKey driverId) numToDel (-1)

deleteFirstNPickupwaypointsImplementation :: (HedisFlow m env) => Id person -> Integer -> m ()
deleteFirstNPickupwaypointsImplementation driverId numToDel = lTrim (makePickupWaypointsRedisKey driverId) numToDel (-1)

makeInterpolatedPointsRedisKey :: Id person -> Text
makeInterpolatedPointsRedisKey driverId = mconcat ["interpolatedPoints", ":", driverId.getId]

makePickupInterpolatedPointsRedisKey :: Id person -> Text
makePickupInterpolatedPointsRedisKey driverId = mconcat ["pickupinterpolatedPoints", ":", driverId.getId]

addInterpolatedPointsImplementation :: (HedisFlow m env) => Id person -> NonEmpty LatLong -> m ()
addInterpolatedPointsImplementation driverId waypoints = do
  let key = makeInterpolatedPointsRedisKey driverId
      numPoints = length waypoints
  rPush key waypoints
  logInfo $ mconcat ["added ", show numPoints, " interpolated points for driverId = ", driverId.getId]

addPickupInterpolatedPointsImplementation :: (HedisFlow m env) => Id person -> NonEmpty LatLong -> m ()
addPickupInterpolatedPointsImplementation driverId waypoints = do
  let key = makeInterpolatedPointsRedisKey driverId
      numPoints = length waypoints
  rPush key waypoints
  logInfo $ mconcat ["added ", show numPoints, " pickup interpolated points for driverId = ", driverId.getId]

clearInterpolatedPointsImplementation :: HedisFlow m env => Id person -> m ()
clearInterpolatedPointsImplementation driverId = do
  let key = makeInterpolatedPointsRedisKey driverId
  clearList key
  logInfo $ mconcat ["cleared interpolated location updates for driverId = ", driverId.getId]

clearPickupInterpolatedPointsImplementation :: HedisFlow m env => Id person -> m ()
clearPickupInterpolatedPointsImplementation driverId = do
  let key = makePickupInterpolatedPointsRedisKey driverId
  clearList key
  logInfo $ mconcat ["cleared pickup interpolated location updates for driverId = ", driverId.getId]

getInterpolatedPointsImplementation :: HedisFlow m env => Id person -> m [LatLong]
getInterpolatedPointsImplementation = Hedis.getList . makeInterpolatedPointsRedisKey

getPickupInterpolatedPointsImplementation :: HedisFlow m env => Id person -> m [LatLong]
getPickupInterpolatedPointsImplementation = Hedis.getList . makePickupInterpolatedPointsRedisKey

expireInterpolatedPointsImplementation :: HedisFlow m env => Id person -> m ()
expireInterpolatedPointsImplementation driverId = do
  let key = makeInterpolatedPointsRedisKey driverId
  Hedis.expire key 86400 -- 24 hours

expirePickupInterpolatedPointsImplementation :: HedisFlow m env => Id person -> m ()
expirePickupInterpolatedPointsImplementation driverId = do
  let key = makePickupInterpolatedPointsRedisKey driverId
  Hedis.expire key 86400 -- 24 hours

interpolatePointsAndCalculateDistanceImplementation ::
  ( HasCallStack,
    EncFlow m r,
    Metrics.CoreMetrics m,
    HasFlowEnv m r '["snapToRoadSnippetThreshold" ::: HighPrecMeters]
  ) =>
  Bool ->
  MapsServiceConfig ->
  [LatLong] ->
  m (HighPrecMeters, [LatLong])
interpolatePointsAndCalculateDistanceImplementation isEndRide mapsCfg wps = do
  if isEndRide && isAllPointsEqual wps
    then pure (0, take 1 wps)
    else do
      res <- Maps.snapToRoad mapsCfg $ Maps.SnapToRoadReq {points = wps}
      pure (res.distance, res.snappedPoints)

isAllPointsEqual :: [LatLong] -> Bool
isAllPointsEqual [] = True
isAllPointsEqual [_] = True
isAllPointsEqual (x : xs) = all (\t -> (abs (x.lat - t.lat) <= eps) && (abs (x.lon - t.lon) <= eps)) xs

eps :: Double
eps = 0.0001
