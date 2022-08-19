module Beckn.LocationUpdates
  ( RideInterpolationHandler (..),
    recalcDistanceBatches,
    --- next functions are needed for tests:
    addPointsImplementation,
    getWaypointsNumberImplementation,
    getFirstNwaypointsImplementation,
    deleteFirstNwaypointsImplementation,
    clearLocationUpdatesOnRideEndImplementation,
    isDistanceCalculationFailed,
    wrapDistanceCalculationImplementation,
    processWaypoints,
    mkHandlerWithDefaultRedisFuncs,
  )
where

import Beckn.Product.MapSearch.GoogleMaps.SnapToRoad
import Beckn.Storage.Hedis
import qualified Beckn.Storage.Hedis as Hedis
import Beckn.Tools.Metrics.CoreMetrics as Metrics
import Beckn.Types.Common
import Beckn.Types.Error (GenericError (InvalidRequest))
import Beckn.Types.Id (Id)
import Beckn.Types.MapSearch
import Beckn.Utils.CalculateDistance
import Beckn.Utils.Common (throwError)
import Beckn.Utils.Logging
import qualified Control.Monad.Catch as C
import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra

data RideInterpolationHandler person m = RideInterpolationHandler
  { batchSize :: Integer,
    addPoints :: Id person -> NonEmpty LatLong -> m (),
    clearLocationUpdatesOnRideEnd :: Id person -> m (),
    getWaypointsNumber :: Id person -> m Integer,
    getFirstNwaypoints :: Id person -> Integer -> m [LatLong],
    deleteFirstNwaypoints :: Id person -> Integer -> m (),
    interpolatePoints :: [LatLong] -> m [LatLong],
    wrapDistanceCalculation :: Id person -> m () -> m (),
    updateDistance :: Id person -> HighPrecMeters -> m ()
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

isDistanceCalculationFailed :: (HedisFlow m r) => Id person -> m Bool
isDistanceCalculationFailed driverId = isJust <$> Hedis.get @() (getFailedDistanceCalculationKey driverId)

resetFailedDistanceCalculationFlag :: (HedisFlow m r) => Id person -> m ()
resetFailedDistanceCalculationFlag driverId = Hedis.del $ getFailedDistanceCalculationKey driverId

processWaypoints ::
  (Monad m, Log m, MonadThrow m) =>
  RideInterpolationHandler person m ->
  Id person ->
  Bool ->
  NonEmpty LatLong ->
  m ()
processWaypoints ih@RideInterpolationHandler {..} driverId ending waypoints =
  ih.wrapDistanceCalculation driverId $ do
    void $ throwError $ InvalidRequest "test"
    addPoints driverId waypoints
    recalcDistanceBatches ih ending driverId

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

recalcDistanceBatchStep ::
  (Monad m, Log m) =>
  RideInterpolationHandler person m ->
  Id person ->
  m Double
recalcDistanceBatchStep RideInterpolationHandler {..} driverId = do
  batchWaypoints <- getFirstNwaypoints driverId (batchSize + 1)
  interpolatedWps <- interpolatePoints batchWaypoints
  let distance = getRouteLinearLength interpolatedWps
  logInfo $ mconcat ["calculated distance for ", show (length interpolatedWps), " points, ", "distance is ", show distance]
  deleteFirstNwaypoints driverId batchSize
  pure distance

-------------------------------------------------------------------------
mkHandlerWithDefaultRedisFuncs ::
  ( HedisFlow m env,
    HasPrettyLogger m env,
    HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m,
    MonadReader env m,
    HasField "googleMapsKey" env Text,
    EsqDBFlow m env
  ) =>
  (Id person -> Double -> m ()) ->
  RideInterpolationHandler person m
mkHandlerWithDefaultRedisFuncs updateDistance =
  RideInterpolationHandler
    { batchSize = 3,
      addPoints = addPointsImplementation,
      clearLocationUpdatesOnRideEnd = clearLocationUpdatesOnRideEndImplementation,
      getWaypointsNumber = getWaypointsNumberImplementation,
      getFirstNwaypoints = getFirstNwaypointsImplementation,
      deleteFirstNwaypoints = deleteFirstNwaypointsImplementation,
      interpolatePoints = callSnapToRoad,
      updateDistance,
      wrapDistanceCalculation = wrapDistanceCalculationImplementation
    }

makeWaypointsRedisKey :: Id person -> Text
makeWaypointsRedisKey driverId = mconcat ["waypoints", ":", driverId.getId]

addPointsImplementation :: (HedisFlow m env) => Id person -> NonEmpty LatLong -> m ()
addPointsImplementation driverId waypoints = do
  let key = makeWaypointsRedisKey driverId
      pointsList = toList waypoints :: [LatLong]
      numPoints = length pointsList
  rPush key pointsList
  logInfo $ mconcat ["added ", show numPoints, " points for driverId = ", driverId.getId]

clearLocationUpdatesOnRideEndImplementation :: (HedisFlow m env) => Id person -> m ()
clearLocationUpdatesOnRideEndImplementation driverId = do
  let key = makeWaypointsRedisKey driverId
  clearList key
  logInfo $ mconcat ["cleared location updates for driverId = ", driverId.getId]
  resetFailedDistanceCalculationFlag driverId

getWaypointsNumberImplementation :: (HedisFlow m env) => Id person -> m Integer
getWaypointsNumberImplementation = lLen . makeWaypointsRedisKey

getFirstNwaypointsImplementation :: (HedisFlow m env) => Id person -> Integer -> m [LatLong]
getFirstNwaypointsImplementation driverId num = lRange (makeWaypointsRedisKey driverId) 0 (num - 1)

deleteFirstNwaypointsImplementation :: (HedisFlow m env) => Id person -> Integer -> m ()
deleteFirstNwaypointsImplementation driverId numToDel = lTrim (makeWaypointsRedisKey driverId) numToDel (-1)

callSnapToRoad ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasField "googleMapsKey" r Text
  ) =>
  [LatLong] ->
  m [LatLong]
callSnapToRoad wps = do
  res <- callSnapToRoadAPI True (PointsList wps)
  pure $ map (snappedLocationtoLatLong . (.location)) (res.snappedPoints)
