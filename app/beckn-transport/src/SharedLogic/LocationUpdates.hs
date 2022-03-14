module SharedLogic.LocationUpdates
  ( RideInterpolationHandler (..),
    processWaypoints,
    recalcDistanceBatches,
    defaultRideInterpolationHandler,
    --- next functions are needed for tests:
    addPointsImplementation,
    getWaypointsNumberImplementation,
    getFirstNwaypointsImplementation,
    deleteFirstNwaypointsImplementation,
  )
where

import Beckn.Storage.Hedis (HedisFlow)
import Beckn.Storage.Hedis.Queries
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id (Id)
import Beckn.Types.MapSearch
import Beckn.Utils.Logging
import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra
import Product.Services.GoogleMaps.SnapToRoad (PointsList (PointsList), callSnapToRoadAPI, snappedLocationtoLatLong)
import SharedLogic.CalculateDistance
import qualified Storage.Queries.Ride as QRide
import Tools.Metrics as Metrics
import qualified Types.Storage.Person as Person

data RideInterpolationHandler m = RideInterpolationHandler
  { batchSize :: Integer,
    addPoints :: Id Person.Person -> NonEmpty LatLong -> m (),
    getWaypointsNumber :: Id Person.Person -> m Integer,
    getFirstNwaypoints :: Id Person.Person -> Integer -> m [LatLong],
    deleteFirstNwaypoints :: Id Person.Person -> Integer -> m (),
    interpolatePoints :: [LatLong] -> m [LatLong],
    updateDistance :: Id Person.Person -> Double -> m ()
  }

--------------------------------------------------------------------------------
processWaypoints ::
  (Monad m, Log m) =>
  RideInterpolationHandler m ->
  Id Person.Person ->
  NonEmpty LatLong ->
  m ()
processWaypoints ih@RideInterpolationHandler {..} driverId waypoints = do
  addPoints driverId waypoints
  let ending = False
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
    recalcDistanceBatches' acc = do
      let continueCondition =
            if ending
              then pointsRemaining
              else atLeastBatchPlusOne
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
  m Double
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
    HasPrettyLogger m env conf,
    HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m,
    MonadReader env m,
    HasField "snapToRoadAPIKey" env Text,
    DBFlow m env
  ) =>
  RideInterpolationHandler m
defaultRideInterpolationHandler =
  RideInterpolationHandler
    { batchSize = 98,
      addPoints = addPointsImplementation,
      getWaypointsNumber = getWaypointsNumberImplementation,
      getFirstNwaypoints = getFirstNwaypointsImplementation,
      deleteFirstNwaypoints = deleteFirstNwaypointsImplementation,
      interpolatePoints = callSnapToRoad,
      updateDistance = \driverId dist -> DB.runSqlDBTransaction $ QRide.updateDistance driverId dist
    }

makeWaypointsRedisKey :: Id Person.Person -> Text
makeWaypointsRedisKey driverId = mconcat ["waypoints", ":", driverId.getId]

addPointsImplementation :: (HedisFlow m env) => Id Person.Person -> NonEmpty LatLong -> m ()
addPointsImplementation driverId waypoints = do
  let key = makeWaypointsRedisKey driverId
      pointsList = toList waypoints :: [LatLong]
      numPoints = length pointsList
  rPush key pointsList
  logInfo $ mconcat ["added ", show numPoints, " points for riderId = ", driverId.getId]

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
    HasField "snapToRoadAPIKey" r Text
  ) =>
  [LatLong] ->
  m [LatLong]
callSnapToRoad wps = do
  res <- callSnapToRoadAPI True (PointsList wps)
  pure $ map (snappedLocationtoLatLong . (.location)) (res.snappedPoints)
