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
    SnapToRoadState (..),
    onRideSnapToRoadStateKey,
    recalcDistanceBatches,
    addPointsImplementation,
    getWaypointsNumberImplementation,
    getFirstNwaypointsImplementation,
    getAllWaypointsImplementation,
    deleteFirstNwaypointsImplementation,
    interpolatePointsAndCalculateDistanceAndTollImplementation,
    clearLocationUpdatesImplementation,
    addInterpolatedPointsImplementation,
    clearInterpolatedPointsImplementation,
    expireInterpolatedPointsImplementation,
    getInterpolatedPointsImplementation,
    isDistanceCalculationFailedImplementation,
    wrapDistanceCalculationImplementation,
    processWaypoints,
    mkRideInterpolationHandler,
    getPassedThroughDrop,
    getTravelledDistanceOutsideThreshold,
    updatePassedThroughDrop,
    getEditDestinationWaypoints,
    deleteEditDestinationWaypoints,
    addEditDestinationSnappedWayPoints,
    deleteAndPushEditDestinationSnappedWayPoints,
    getEditDestinationSnappedWaypoints,
    deleteEditDestinationSnappedWaypoints,
    getTravelledDistance,
  )
where

import qualified Control.Monad.Catch as C
import qualified Data.List.NonEmpty as NE
import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra
import Kernel.External.Maps as Maps
import Kernel.Prelude (last, roundToIntegral)
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import Kernel.Utils.ComputeIntersection

data RideInterpolationHandler person m = RideInterpolationHandler
  { batchSize :: Integer,
    maxSnapToRoadReqPoints :: Integer,
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
    interpolatePointsAndCalculateDistanceAndToll :: Maybe LatLong -> Maybe MapsServiceConfig -> Bool -> Id person -> [LatLong] -> m (HighPrecMeters, [LatLong], [MapsService], Bool, Maybe (HighPrecMoney, [Text], [Text], Bool, Maybe Bool)),
    wrapDistanceCalculation :: Id person -> m () -> m (),
    isDistanceCalculationFailed :: Id person -> m Bool,
    updateDistance :: Id person -> HighPrecMeters -> Int -> Int -> Maybe Int -> Bool -> m (),
    updateTollChargesAndNamesAndIds :: Id person -> HighPrecMoney -> [Text] -> [Text] -> m (),
    updateRouteDeviation :: Id person -> [LatLong] -> m (Bool, Bool, Bool),
    getTravelledDistanceAndTollInfo :: Id person -> Meters -> Maybe (HighPrecMoney, [Text], [Text], Bool, Maybe Bool) -> m (Meters, Maybe (HighPrecMoney, [Text], [Text], Bool, Maybe Bool)),
    getRecomputeIfPickupDropNotOutsideOfThreshold :: Bool,
    sendTollCrossedNotificationToDriver :: Id person -> m (),
    sendTollCrossedUpdateToBAP :: Id person -> m ()
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
isDistanceCalculationFailedImplementation driverId = isJust <$> (Hedis.withMasterRedis $ Hedis.get @() (getFailedDistanceCalculationKey driverId))

resetFailedDistanceCalculationFlag :: (HedisFlow m r) => Id person -> m ()
resetFailedDistanceCalculationFlag driverId = Hedis.del $ getFailedDistanceCalculationKey driverId

processWaypoints ::
  (CacheFlow m r, Log m, MonadThrow m, MonadFlow m) =>
  RideInterpolationHandler person m ->
  Id person ->
  Bool ->
  Meters ->
  Maybe HighPrecMoney ->
  Maybe [Text] ->
  Maybe [Text] ->
  Bool ->
  Maybe MapsServiceConfig ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  NonEmpty LatLong ->
  m ()
processWaypoints ih@RideInterpolationHandler {..} driverId ending estDist estTollCharges estTollNames estTollIds pickupDropOutsideThreshold rectifyDistantPointsFailureUsing isTollApplicable enableTollCrossedNotifications passedThroughDrop isMeterRide waypoints = do
  calculationFailed <- isDistanceCalculationFailed driverId
  when calculationFailed do
    logWarning "Failed to calculate actual distance for this ride, ignoring"
  wrapDistanceCalculation driverId $ do
    addEditDestinationPoints driverId waypoints
    addPoints driverId waypoints
    recalcDistanceBatches ih ending driverId estDist estTollCharges estTollNames estTollIds pickupDropOutsideThreshold rectifyDistantPointsFailureUsing isTollApplicable enableTollCrossedNotifications waypoints calculationFailed passedThroughDrop isMeterRide

lastTwoOnRidePointsRedisKey :: Id person -> Text
lastTwoOnRidePointsRedisKey driverId = "Driver-Location-Last-Two-OnRide-Points:DriverId-" <> driverId.getId

data SnapToRoadState = SnapToRoadState
  { distanceTravelled :: HighPrecMeters,
    distanceTravelledOutSideDropThreshold :: Maybe HighPrecMeters,
    googleSnapToRoadCalls :: Int,
    osrmSnapToRoadCalls :: Int,
    numberOfSelfTuned :: Maybe Int,
    passThroughDropThreshold :: Maybe Bool,
    startPatchingDistance :: Maybe Bool,
    distanceCalcReferencePoint :: Maybe LatLong
  }
  deriving (Generic, FromJSON, ToJSON)

onRideSnapToRoadStateKey :: Id person -> Text
onRideSnapToRoadStateKey driverId = "Driver-Location-OnRide-SnapToRoad:DriverId-" <> driverId.getId

onRideTollChargesKey :: Id person -> Text
onRideTollChargesKey driverId = "OnRideTollCharges:" <> driverId.getId

onRideTollNamesKey :: Id person -> Text
onRideTollNamesKey driverId = "OnRideTollNames:" <> driverId.getId

onRideTollIdsKey :: Id person -> Text
onRideTollIdsKey driverId = "OnRideTollIds:" <> driverId.getId

takeLastTwo :: [a] -> [a]
takeLastTwo xs = drop (max 0 (length xs - 2)) xs

fromBool :: Num a => Bool -> a
fromBool False = 0
fromBool True = 1

recalcDistanceBatches ::
  (CacheFlow m r, Monad m, Log m, MonadFlow m) =>
  RideInterpolationHandler person m ->
  Bool ->
  Id person ->
  Meters ->
  Maybe HighPrecMoney ->
  Maybe [Text] ->
  Maybe [Text] ->
  Bool ->
  Maybe MapsServiceConfig ->
  Bool ->
  Bool ->
  NonEmpty LatLong ->
  Bool ->
  Bool ->
  Bool ->
  m ()
recalcDistanceBatches h@RideInterpolationHandler {..} ending driverId estDist estTollCharges estTollNames estTollIds pickupDropOutsideThreshold rectifyDistantPointsFailureUsing isTollApplicable enableTollCrossedNotifications waypoints calculationFailed passedThroughDrop isMeterRide = do
  prevBatchTwoEndPoints :: Maybe [LatLong] <- Redis.safeGet $ lastTwoOnRidePointsRedisKey driverId
  let modifiedWaypoints =
        case prevBatchTwoEndPoints of
          Just points -> NE.fromList points <> waypoints
          _ -> waypoints
  let currentLastTwoPoints = takeLastTwo (toList waypoints)
  Redis.setExp (lastTwoOnRidePointsRedisKey driverId) currentLastTwoPoints 21600 -- 6 hours
  (routeDeviation, tollRouteDeviation, isTollPresentOnCurrentRoute) <- updateRouteDeviation driverId (toList modifiedWaypoints)
  when (isTollApplicable && enableTollCrossedNotifications && isTollPresentOnCurrentRoute) $
    fork "Toll Crossed OnUpdate" $ do
      sendTollCrossedNotificationToDriver driverId
      sendTollCrossedUpdateToBAP driverId
  let recomputeIfPickupDropNotOutsideOfThreshold = getRecomputeIfPickupDropNotOutsideOfThreshold
  _ <- processPassedThroughDrop
  let snapToRoadCallCondition = (routeDeviation && recomputeIfPickupDropNotOutsideOfThreshold) || pickupDropOutsideThreshold || isJust rectifyDistantPointsFailureUsing || isJust estTollCharges || tollRouteDeviation
  if ending
    then do
      if snapToRoadCallCondition
        then do
          currSnapToRoadState <- processSnapToRoadCall
          updateDistance driverId currSnapToRoadState.distanceTravelled currSnapToRoadState.googleSnapToRoadCalls currSnapToRoadState.osrmSnapToRoadCalls currSnapToRoadState.numberOfSelfTuned calculationFailed
          when isTollApplicable $ do
            mbTollCharges :: Maybe HighPrecMoney <- Redis.safeGet (onRideTollChargesKey driverId)
            tollNames :: [Text] <- Redis.lRange (onRideTollNamesKey driverId) 0 (-1)
            tollIds :: [Text] <- Redis.lRange (onRideTollIdsKey driverId) 0 (-1)
            whenJust mbTollCharges $ \tollCharges -> updateTollChargesAndNamesAndIds driverId tollCharges tollNames tollIds
        else do
          (distanceToBeUpdated, tollChargesInfo) <- getTravelledDistanceAndTollInfo driverId estDist ((,,,False,Just False) <$> estTollCharges <*> estTollNames <*> estTollIds)
          updateDistance driverId (metersToHighPrecMeters distanceToBeUpdated) 0 0 (Just 0) calculationFailed
          whenJust tollChargesInfo $ \(tollCharges, tollNames, tollIds, _, _) ->
            when isTollApplicable $
              updateTollChargesAndNamesAndIds driverId tollCharges tollNames tollIds
    else do
      isAtLeastBatchPlusOne <- atLeastBatchPlusOne
      when (snapToRoadCallCondition && isAtLeastBatchPlusOne) $ do
        currSnapToRoadState <- processSnapToRoadCall
        Redis.setExp (onRideSnapToRoadStateKey driverId) currSnapToRoadState 86400 -- 24 hours
  where
    pointsRemaining = (> 0) <$> getWaypointsNumber driverId
    continueCondition = if ending then pointsRemaining else atLeastBatchPlusOne
    atLeastBatchPlusOne = (> batchSize) <$> getWaypointsNumber driverId

    recalcDistanceBatches' isFirstCall snapToRoad'@SnapToRoadState {..} snapToRoadCallFailed = do
      batchLeft <- continueCondition
      if (batchLeft && not isMeterRide) || (batchLeft && isMeterRide && isFirstCall)
        then do
          (dist, newReferencePoint, startPatching, servicesUsed, snapCallFailed) <- recalcDistanceBatchStep h isMeterRide distanceCalcReferencePoint rectifyDistantPointsFailureUsing isTollApplicable driverId
          let googleCalled = Google `elem` servicesUsed
              osrmCalled = OSRM `elem` servicesUsed
              selfTunedCount = SelfTuned `elem` servicesUsed
              isPassedThroughDrop = bool passThroughDropThreshold (Just True) passedThroughDrop
              distanceTravelledOutSideDropThreshold'' = bool (Just distanceTravelled) distanceTravelledOutSideDropThreshold (isPassedThroughDrop == Just True)
              distanceTravelledOutSideDropThreshold' = bool (distanceTravelledOutSideDropThreshold'' <&> (+ dist)) distanceTravelledOutSideDropThreshold'' (isPassedThroughDrop == Just True || snapCallFailed)
              updDistance = bool dist (distanceTravelled + dist) (startPatching || fromMaybe True startPatchingDistance)
          if snapCallFailed
            then pure (SnapToRoadState distanceTravelled distanceTravelledOutSideDropThreshold' (googleSnapToRoadCalls + fromBool googleCalled) (osrmSnapToRoadCalls + fromBool osrmCalled) ((\numberOfSelfTuned' -> fromBool selfTunedCount + numberOfSelfTuned') <$> numberOfSelfTuned) isPassedThroughDrop (Just (startPatching || fromMaybe True startPatchingDistance)) newReferencePoint, snapCallFailed)
            else recalcDistanceBatches' False (SnapToRoadState updDistance distanceTravelledOutSideDropThreshold' (googleSnapToRoadCalls + fromBool googleCalled) (osrmSnapToRoadCalls + fromBool osrmCalled) ((\numberOfSelfTuned' -> fromBool selfTunedCount + numberOfSelfTuned') <$> numberOfSelfTuned) isPassedThroughDrop (Just (startPatching || fromMaybe True startPatchingDistance)) newReferencePoint) snapCallFailed
        else pure (snapToRoad', snapToRoadCallFailed)

    processSnapToRoadCall = do
      prevSnapToRoadState :: SnapToRoadState <-
        Redis.safeGet (onRideSnapToRoadStateKey driverId)
          <&> fromMaybe (SnapToRoadState 0 (Just 0) 0 0 (Just 0) (Just passedThroughDrop) (Just False) Nothing)
      (currSnapToRoadState, snapToRoadCallFailed) <- recalcDistanceBatches' True prevSnapToRoadState False
      when snapToRoadCallFailed $ do
        updateDistance driverId currSnapToRoadState.distanceTravelled currSnapToRoadState.googleSnapToRoadCalls currSnapToRoadState.osrmSnapToRoadCalls currSnapToRoadState.numberOfSelfTuned calculationFailed
        throwError $ InternalError $ "Snap to road call failed for driverId =" <> driverId.getId
      return currSnapToRoadState

    processPassedThroughDrop = do
      prevSnapToRoadState :: SnapToRoadState <-
        Redis.safeGet (onRideSnapToRoadStateKey driverId)
          <&> fromMaybe (SnapToRoadState 0 (Just 0) 0 0 (Just 0) (Just passedThroughDrop) (Just False) Nothing)
      let isPassedThroughDrop = bool prevSnapToRoadState.passThroughDropThreshold (Just True) passedThroughDrop
      let currSnapToRoadState = prevSnapToRoadState {passThroughDropThreshold = isPassedThroughDrop}
      Redis.setExp (onRideSnapToRoadStateKey driverId) currSnapToRoadState 86400 -- 24 hours

recalcDistanceBatchStep ::
  (CacheFlow m r, Monad m, Log m, MonadFlow m) =>
  RideInterpolationHandler person m ->
  Bool ->
  Maybe LatLong ->
  Maybe MapsServiceConfig ->
  Bool ->
  Id person ->
  m (HighPrecMeters, Maybe LatLong, Bool, [MapsService], Bool)
recalcDistanceBatchStep RideInterpolationHandler {..} isMeterRide distanceCalcReferencePoint rectifyDistantPointsFailureUsing isTollApplicable driverId = do
  (startPatching, distanceCalcReferencePoint') <-
    if isMeterRide
      then do
        pointsTillNow <- getWaypointsNumber driverId
        let pointToRemove = pointsTillNow - maxSnapToRoadReqPoints
        when (pointToRemove > 0) $ deleteFirstNwaypoints driverId pointToRemove
        pure $ bool (False, Nothing) (True, distanceCalcReferencePoint) (pointToRemove > 0) -- means we have covered 99 points once, now we have to start patching
      else pure (True, Nothing)
  batchWaypoints <- getFirstNwaypoints driverId (maxSnapToRoadReqPoints + 1)
  (distance, interpolatedWps, servicesUsed, snapToRoadFailed, mbTollChargesAndNames) <- interpolatePointsAndCalculateDistanceAndToll distanceCalcReferencePoint' rectifyDistantPointsFailureUsing isTollApplicable driverId batchWaypoints
  whenJust mbTollChargesAndNames $ \(tollCharges, tollNames, tollIds, _, _) -> do
    void $ Redis.rPushExp (onRideTollNamesKey driverId) tollNames 21600
    void $ Redis.rPushExp (onRideTollIdsKey driverId) tollIds 21600
    void $ Redis.incrby (onRideTollChargesKey driverId) (round tollCharges.getHighPrecMoney)
    Redis.expire (onRideTollChargesKey driverId) 21600 -- 6 hours
  whenJust (nonEmpty interpolatedWps) $ \nonEmptyInterpolatedWps -> do
    addInterpolatedPoints driverId nonEmptyInterpolatedWps
  when snapToRoadFailed $ do
    whenJust (nonEmpty batchWaypoints) $ \nonEmptyBatchWaypoints ->
      addInterpolatedPoints driverId nonEmptyBatchWaypoints
  unless isMeterRide $ deleteFirstNwaypoints driverId maxSnapToRoadReqPoints -- delete the batch irrespective of the snapToRoadFailed
  unless snapToRoadFailed $ do
    logInfo $ mconcat ["points interpolation: input=", show batchWaypoints, "; output=", show interpolatedWps]
    logInfo $ mconcat ["calculated distance for ", show (length interpolatedWps), " points, ", "distance is ", show distance]
    when isTollApplicable $ do
      mbTollCharges :: Maybe HighPrecMoney <- Redis.safeGet (onRideTollChargesKey driverId)
      tollNames :: [Text] <- Redis.lRange (onRideTollNamesKey driverId) 0 (-1)
      tollIds :: [Text] <- Redis.lRange (onRideTollIdsKey driverId) 0 (-1)
      whenJust mbTollCharges $ \tollCharges -> updateTollChargesAndNamesAndIds driverId tollCharges tollNames tollIds
  let newReferencePoint = if snapToRoadFailed then Nothing else lastMaybe interpolatedWps
  pure (distance, newReferencePoint, startPatching, servicesUsed, snapToRoadFailed)
  where
    lastMaybe [] = Nothing
    lastMaybe xs = Just $ last xs

redisOnRideKeysCleanup :: (HedisFlow m env) => Id person -> m ()
redisOnRideKeysCleanup driverId = do
  Redis.del (lastTwoOnRidePointsRedisKey driverId)
  Redis.del (onRideSnapToRoadStateKey driverId)
  Redis.del (onRideTollChargesKey driverId)
  Redis.del (onRideTollNamesKey driverId)
  Redis.del (onRideTollIdsKey driverId)

-------------------------------------------------------------------------
mkRideInterpolationHandler ::
  ( HedisFlow m r,
    HasPrettyLogger m r,
    HasCallStack,
    Metrics.CoreMetrics m,
    EncFlow m r,
    HasFlowEnv m r '["snapToRoadSnippetThreshold" ::: HighPrecMeters],
    HasFlowEnv m r '["droppedPointsThreshold" ::: HighPrecMeters],
    HasFlowEnv m r '["osrmMatchThreshold" ::: HighPrecMeters]
  ) =>
  Integer ->
  Integer ->
  Bool ->
  (Id person -> HighPrecMeters -> Int -> Int -> Maybe Int -> Bool -> m ()) ->
  (Id person -> HighPrecMoney -> [Text] -> [Text] -> m ()) ->
  (Id person -> [LatLong] -> m (Bool, Bool, Bool)) ->
  (Maybe (Id person) -> RoutePoints -> m (Maybe (HighPrecMoney, [Text], [Text], Bool, Maybe Bool))) ->
  (Id person -> Meters -> Maybe (HighPrecMoney, [Text], [Text], Bool, Maybe Bool) -> m (Meters, Maybe (HighPrecMoney, [Text], [Text], Bool, Maybe Bool))) ->
  Bool ->
  (Maybe MapsServiceConfig -> Maps.SnapToRoadReq -> m ([Maps.MapsService], Either String Maps.SnapToRoadResp)) ->
  (Id person -> m ()) ->
  (Id person -> m ()) ->
  RideInterpolationHandler person m
mkRideInterpolationHandler batchSize maxSnapToRoadReqPoints isEndRide updateDistance updateTollChargesAndNamesAndIds updateRouteDeviation getTollInfoOnTheRoute getTravelledDistanceAndTollInfo getRecomputeIfPickupDropNotOutsideOfThreshold snapToRoadCall sendTollCrossedNotificationToDriver sendTollCrossedUpdateToBAP =
  RideInterpolationHandler
    { batchSize = batchSize,
      maxSnapToRoadReqPoints = maxSnapToRoadReqPoints,
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
      interpolatePointsAndCalculateDistanceAndToll = interpolatePointsAndCalculateDistanceAndTollImplementation isEndRide snapToRoadCall getTollInfoOnTheRoute,
      isDistanceCalculationFailed = isDistanceCalculationFailedImplementation,
      wrapDistanceCalculation = wrapDistanceCalculationImplementation,
      ..
    }

makeWaypointsRedisKey :: Id person -> Text
makeWaypointsRedisKey driverId = mconcat ["waypoints", ":", driverId.getId]

addPointsImplementation :: (HedisFlow m env) => Id person -> NonEmpty LatLong -> m ()
addPointsImplementation driverId waypoints = do
  let key = makeWaypointsRedisKey driverId
      numPoints = length waypoints
  rPush key waypoints
  Hedis.expire key 21600 -- 6 hours
  logInfo $ mconcat ["added ", show numPoints, " points for driverId = ", driverId.getId]

makeEditDestinationWaypointsRedisKey :: Id person -> Text
makeEditDestinationWaypointsRedisKey driverId = mconcat ["editDestinationWaypoints", ":", driverId.getId]

makeEditDestinationSnappedWaypointsRedisKey :: Id person -> Text
makeEditDestinationSnappedWaypointsRedisKey driverId = mconcat ["editDestinationSnappedWaypoints", ":", driverId.getId]

addEditDestinationSnappedWayPoints :: (HedisFlow m env) => Id person -> NonEmpty (LatLong, Bool) -> m ()
addEditDestinationSnappedWayPoints driverId waypoints = do
  let key = makeEditDestinationSnappedWaypointsRedisKey driverId
  rPush key waypoints
  Hedis.expire key 86400 -- 24 hours

deleteAndPushEditDestinationSnappedWayPoints :: (HedisFlow m env) => Id person -> NonEmpty (LatLong, Bool) -> m ()
deleteAndPushEditDestinationSnappedWayPoints driverId waypoints = do
  let key = makeEditDestinationSnappedWaypointsRedisKey driverId
  Hedis.del key
  rPush key waypoints
  Hedis.expire key 86400 -- 24 hours

getEditDestinationSnappedWaypoints :: (HedisFlow m env) => Id person -> m [(LatLong, Bool)]
getEditDestinationSnappedWaypoints driverId = lRange (makeEditDestinationSnappedWaypointsRedisKey driverId) 0 (-1)

deleteEditDestinationSnappedWaypoints :: (HedisFlow m env) => Id person -> m ()
deleteEditDestinationSnappedWaypoints driverId = do
  let key = makeEditDestinationSnappedWaypointsRedisKey driverId
  Hedis.del key

addEditDestinationPoints :: (HedisFlow m env) => Id person -> NonEmpty LatLong -> m ()
addEditDestinationPoints driverId waypoints = do
  let key = makeEditDestinationWaypointsRedisKey driverId
  rPush key waypoints
  Hedis.expire key 86400 -- 24 hours

deleteEditDestinationWaypoints :: (HedisFlow m env) => Id person -> m ()
deleteEditDestinationWaypoints driverId = do
  let key = makeEditDestinationWaypointsRedisKey driverId
  Hedis.del key

getEditDestinationWaypoints :: (HedisFlow m env) => Id person -> m [LatLong]
getEditDestinationWaypoints driverId = lRange (makeEditDestinationWaypointsRedisKey driverId) 0 (-1)

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
  redisOnRideKeysCleanup driverId
  logInfo $ mconcat ["cleared interpolated location updates for driverId = ", driverId.getId]

getInterpolatedPointsImplementation :: HedisFlow m env => Id person -> m [LatLong]
getInterpolatedPointsImplementation = Hedis.getList . makeInterpolatedPointsRedisKey

expireInterpolatedPointsImplementation :: HedisFlow m env => Id person -> m ()
expireInterpolatedPointsImplementation driverId = do
  let key = makeInterpolatedPointsRedisKey driverId
  Hedis.expire key 86400 -- 24 hours

interpolatePointsAndCalculateDistanceAndTollImplementation ::
  ( HasCallStack,
    EncFlow m r,
    Metrics.CoreMetrics m,
    HasFlowEnv m r '["snapToRoadSnippetThreshold" ::: HighPrecMeters],
    HasFlowEnv m r '["droppedPointsThreshold" ::: HighPrecMeters],
    HasFlowEnv m r '["osrmMatchThreshold" ::: HighPrecMeters]
  ) =>
  Bool ->
  (Maybe MapsServiceConfig -> Maps.SnapToRoadReq -> m ([Maps.MapsService], Either String Maps.SnapToRoadResp)) ->
  (Maybe (Id person) -> RoutePoints -> m (Maybe (HighPrecMoney, [Text], [Text], Bool, Maybe Bool))) ->
  Maybe LatLong ->
  Maybe MapsServiceConfig ->
  Bool ->
  Id person ->
  [LatLong] ->
  m (HighPrecMeters, [LatLong], [Maps.MapsService], Bool, Maybe (HighPrecMoney, [Text], [Text], Bool, Maybe Bool))
interpolatePointsAndCalculateDistanceAndTollImplementation isEndRide snapToRoadCall getTollInfoOnTheRoute distanceCalcReferencePoint rectifyDistantPointsFailureUsing isTollApplicable driverId wps = do
  if isEndRide && isAllPointsEqual wps
    then pure (0, take 1 wps, [], False, Nothing)
    else do
      (servicesUsed, res) <- snapToRoadCall rectifyDistantPointsFailureUsing $ Maps.SnapToRoadReq {points = wps, distanceUnit = Meter, calculateDistanceFrom = distanceCalcReferencePoint}
      case res of
        Left _ -> pure (0, [], [], True, Nothing)
        Right response -> do
          tollInfo <-
            if isTollApplicable
              then getTollInfoOnTheRoute (Just driverId) response.snappedPoints
              else return Nothing
          pure (response.distance, response.snappedPoints, servicesUsed, False, tollInfo)

isAllPointsEqual :: [LatLong] -> Bool
isAllPointsEqual [] = True
isAllPointsEqual [_] = True
isAllPointsEqual (x : xs) = all (\t -> (abs (x.lat - t.lat) <= eps) && (abs (x.lon - t.lon) <= eps)) xs

eps :: Double
eps = 0.0001

getPassedThroughDrop :: (HedisFlow m env) => Id person -> m Bool
getPassedThroughDrop driverId = do
  prevSnapToRoadState :: SnapToRoadState <-
    Redis.safeGet (onRideSnapToRoadStateKey driverId)
      <&> fromMaybe (SnapToRoadState 0 (Just 0) 0 0 (Just 0) (Just False) (Just False) Nothing)
  fromMaybe False <$> pure prevSnapToRoadState.passThroughDropThreshold

getTravelledDistance :: (HedisFlow m env) => Id person -> m HighPrecMeters
getTravelledDistance driverId = do
  prevSnapToRoadState :: SnapToRoadState <-
    Redis.safeGet (onRideSnapToRoadStateKey driverId)
      <&> fromMaybe (SnapToRoadState 0 (Just 0) 0 0 (Just 0) (Just False) (Just False) Nothing)
  pure prevSnapToRoadState.distanceTravelled

getTravelledDistanceOutsideThreshold :: (HedisFlow m env) => Id person -> m Meters
getTravelledDistanceOutsideThreshold driverId = do
  prevSnapToRoadState :: SnapToRoadState <-
    Redis.safeGet (onRideSnapToRoadStateKey driverId)
      <&> fromMaybe (SnapToRoadState 0 (Just 0) 0 0 (Just 0) (Just False) (Just False) Nothing)
  pure . roundToIntegral $ fromMaybe prevSnapToRoadState.distanceTravelled prevSnapToRoadState.distanceTravelledOutSideDropThreshold

updatePassedThroughDrop :: (HedisFlow m env) => Id person -> m ()
updatePassedThroughDrop driverId = do
  prevSnapToRoadState :: SnapToRoadState <-
    Redis.safeGet (onRideSnapToRoadStateKey driverId)
      <&> fromMaybe (SnapToRoadState 0 (Just 0) 0 0 (Just 0) (Just False) (Just False) Nothing)
  Redis.setExp (onRideSnapToRoadStateKey driverId) prevSnapToRoadState {passThroughDropThreshold = Just False} 86400 -- 24 hours
