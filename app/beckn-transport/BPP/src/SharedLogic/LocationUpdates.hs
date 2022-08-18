module SharedLogic.LocationUpdates
  ( RideInterpolationHandler (..),
    recalcDistanceBatches,
    defaultRideInterpolationHandler,
    --- next functions are needed for tests:
    addPointsImplementation,
    getWaypointsNumberImplementation,
    getFirstNwaypointsImplementation,
    deleteFirstNwaypointsImplementation,
    clearLocationUpdatesOnRideEndImplementation,
    isDistanceCalculationFailed,
    wrapDistanceCalculationImplementation,
    processWaypoints,
  )
where

import Beckn.Product.MapSearch.GoogleMaps.SnapToRoad
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis
import qualified Beckn.Storage.Hedis as Hedis
import Beckn.Types.Common
import Beckn.Types.Error (GenericError (InvalidRequest))
import Beckn.Types.Id (Id)
import Beckn.Types.MapSearch
import Beckn.Utils.CalculateDistance
import Beckn.Utils.Common (throwError)
import Beckn.Utils.Logging
import qualified Control.Monad.Catch as C
import qualified Domain.Types.Person as Person
import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra
import qualified Storage.Queries.Ride as QRide
import Tools.Metrics as Metrics

data RideInterpolationHandler m = RideInterpolationHandler
  { batchSize :: Integer,
    addPoints :: Id Person.Person -> NonEmpty LatLong -> m (),
    clearLocationUpdatesOnRideEnd :: Id Person.Person -> m (),
    getWaypointsNumber :: Id Person.Person -> m Integer,
    getFirstNwaypoints :: Id Person.Person -> Integer -> m [LatLong],
    deleteFirstNwaypoints :: Id Person.Person -> Integer -> m (),
    interpolatePoints :: [LatLong] -> m [LatLong],
    wrapDistanceCalculation :: Id Person.Person -> m () -> m (),
    updateDistance :: Id Person.Person -> HighPrecMeters -> m ()
  }

--------------------------------------------------------------------------------

wrapDistanceCalculationImplementation :: (C.MonadMask m, Log m, HedisFlow m r) => Id Person.Person -> m () -> m ()
wrapDistanceCalculationImplementation driverId action =
  action `C.catchAll` \e -> C.mask_ $ do
    logError $ "failed distance calculation: " <> show e
    let oneDayInSeconds = 60 * 60 * 24
    Hedis.setExp (getFailedDistanceCalculationKey driverId) () oneDayInSeconds

getFailedDistanceCalculationKey :: Id Person.Person -> Text
getFailedDistanceCalculationKey driverId = mconcat [driverId.getId, ":locationUpdatesFailed"]

isDistanceCalculationFailed :: (HedisFlow m r) => Id Person.Person -> m Bool
isDistanceCalculationFailed driverId = isJust <$> Hedis.get @() (getFailedDistanceCalculationKey driverId)

processWaypoints ::
  (Monad m, Log m, MonadThrow m) =>
  RideInterpolationHandler m ->
  Id Person.Person ->
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
  RideInterpolationHandler m ->
  Bool ->
  Id Person.Person ->
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
  RideInterpolationHandler m ->
  Id Person.Person ->
  m HighPrecMeters
recalcDistanceBatchStep RideInterpolationHandler {..} driverId = do
  batchWaypoints <- getFirstNwaypoints driverId (batchSize + 1)
  interpolatedWps <- interpolatePoints batchWaypoints
  let distance = getRouteLinearLength interpolatedWps
  logInfo $ mconcat ["calculated distance for ", show (length interpolatedWps), " points, ", "distance is ", show distance]
  deleteFirstNwaypoints driverId batchSize
  pure distance

-------------------------------------------------------------------------
defaultRideInterpolationHandler ::
  ( HedisFlow m env,
    HasPrettyLogger m env,
    HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m,
    MonadReader env m,
    HasField "googleMapsKey" env Text,
    EsqDBFlow m env
  ) =>
  RideInterpolationHandler m
defaultRideInterpolationHandler =
  RideInterpolationHandler
    { batchSize = 3,
      addPoints = addPointsImplementation,
      --      addPoints = \_ _ -> throwError $ InvalidRequest "",
      clearLocationUpdatesOnRideEnd = clearLocationUpdatesOnRideEndImplementation,
      getWaypointsNumber = getWaypointsNumberImplementation,
      getFirstNwaypoints = getFirstNwaypointsImplementation,
      deleteFirstNwaypoints = deleteFirstNwaypointsImplementation,
      interpolatePoints = callSnapToRoad,
      updateDistance = \driverId dist -> Esq.runTransaction $ QRide.updateDistance driverId dist,
      wrapDistanceCalculation = wrapDistanceCalculationImplementation
    }

makeWaypointsRedisKey :: Id Person.Person -> Text
makeWaypointsRedisKey driverId = mconcat ["waypoints", ":", driverId.getId]

addPointsImplementation :: (HedisFlow m env) => Id Person.Person -> NonEmpty LatLong -> m ()
addPointsImplementation driverId waypoints = do
  let key = makeWaypointsRedisKey driverId
      pointsList = toList waypoints :: [LatLong]
      numPoints = length pointsList
  rPush key pointsList
  logInfo $ mconcat ["added ", show numPoints, " points for driverId = ", driverId.getId]

clearLocationUpdatesOnRideEndImplementation :: (HedisFlow m env) => Id Person.Person -> m ()
clearLocationUpdatesOnRideEndImplementation driverId = do
  let key = makeWaypointsRedisKey driverId
  clearList key
  logInfo $ mconcat ["cleared location updates for driverId = ", driverId.getId]

getWaypointsNumberImplementation :: (HedisFlow m env) => Id Person.Person -> m Integer
getWaypointsNumberImplementation = lLen . makeWaypointsRedisKey

getFirstNwaypointsImplementation :: (HedisFlow m env) => Id Person.Person -> Integer -> m [LatLong]
getFirstNwaypointsImplementation driverId num = lRange (makeWaypointsRedisKey driverId) 0 (num - 1)

deleteFirstNwaypointsImplementation :: (HedisFlow m env) => Id Person.Person -> Integer -> m ()
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
