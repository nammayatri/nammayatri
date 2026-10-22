{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.LocationUpdates
  ( I.RideInterpolationHandler,
    I.mkRideInterpolationHandler,
    isDistanceCalculationFailed,
    initializeDistanceCalculation,
    finalDistanceCalculation,
    finalPickupDistanceCalculation,
    addIntermediateRoutePoints,
    addIntermediatePickupPoints,
    getInterpolatedPoints,
    clearInterpolatedPoints,
    pickupLegDriverId,
    resetPickupLegState,
    isPassedThroughDrop,
  )
where

import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra
import Kernel.External.Maps.Interface.Types (MapsServiceConfig)
import Kernel.External.Maps.Types
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.CacheFlow
import Kernel.Types.Common
import Kernel.Types.Id (Id (..))
import qualified Lib.LocationUpdates.Internal as I

withRideIdLogTag :: (Log m) => Id ride -> m a -> m a
withRideIdLogTag rideId = withLogTag ("locupd-rideId-" <> rideId.getId)

-- API
-- All these functions take rideId such that it was possible later to fetch location updates
-- and interpolated route points from log files.

initializeDistanceCalculation :: (Monad m, Log m) => I.RideInterpolationHandler person m -> Id ride -> Id person -> LatLong -> m ()
initializeDistanceCalculation ih rideId driverId pt = withRideIdLogTag rideId $ do
  ih.clearLocationUpdates driverId
  ih.clearInterpolatedPoints driverId
  ih.expireInterpolatedPoints driverId
  ih.addPoints driverId $ pt :| []

finalDistanceCalculation :: (CacheFlow m r, Log m, MonadThrow m, MonadFlow m) => I.RideInterpolationHandler person m -> Maybe MapsServiceConfig -> Bool -> Bool -> Id ride -> Id person -> [(LatLong, Int64)] -> NonEmpty (LatLong, Int64) -> Meters -> Maybe HighPrecMoney -> Maybe [Text] -> Maybe [Text] -> Bool -> Bool -> Bool -> m ()
finalDistanceCalculation ih rectifyDistantPointsFailureUsing isTollApplicable sendTollCrossedNotification rideId driverId accWaypointsWithTime allWaypointsWithTime estDist estTollCharges estTollNames estTollIds pickupDropOutsideThreshold passedThroughDrop isMeterRide = withRideIdLogTag rideId $ do
  I.processWaypoints ih driverId True estDist estTollCharges estTollNames estTollIds pickupDropOutsideThreshold rectifyDistantPointsFailureUsing isTollApplicable sendTollCrossedNotification passedThroughDrop isMeterRide accWaypointsWithTime allWaypointsWithTime

-- | Pickup-leg variant of 'finalDistanceCalculation'
finalPickupDistanceCalculation :: (CacheFlow m r, Log m, MonadThrow m, MonadFlow m) => I.RideInterpolationHandler person m -> Id ride -> Id person -> [(LatLong, Int64)] -> NonEmpty (LatLong, Int64) -> m ()
finalPickupDistanceCalculation ih rideId driverId accWaypointsWithTime allWaypointsWithTime = withRideIdLogTag rideId $ do
  I.processWaypointsPickup ih driverId True accWaypointsWithTime allWaypointsWithTime

getInterpolatedPoints :: I.RideInterpolationHandler person m -> Id person -> m [LatLong]
getInterpolatedPoints ih = ih.getInterpolatedPoints

clearInterpolatedPoints :: I.RideInterpolationHandler person m -> Id person -> m ()
clearInterpolatedPoints ih = ih.clearInterpolatedPoints

addIntermediateRoutePoints :: (CacheFlow m r, Log m, MonadThrow m, MonadFlow m) => I.RideInterpolationHandler person m -> Maybe MapsServiceConfig -> Bool -> Bool -> Id ride -> Id person -> Bool -> Bool -> [(LatLong, Int64)] -> NonEmpty (LatLong, Int64) -> m ()
addIntermediateRoutePoints ih rectifyDistantPointsFailureUsing isTollApplicable sendTollCrossedNotification rideId driverId passedThroughDrop isMeterRide accWaypointsWithTime allWaypointsWithTime = withRideIdLogTag rideId $ do
  I.processWaypoints ih driverId False 0 Nothing Nothing Nothing False rectifyDistantPointsFailureUsing isTollApplicable sendTollCrossedNotification passedThroughDrop isMeterRide accWaypointsWithTime allWaypointsWithTime

-- | Pickup-leg variant of 'addIntermediateRoutePoints'
addIntermediatePickupPoints :: (CacheFlow m r, Log m, MonadThrow m, MonadFlow m) => I.RideInterpolationHandler person m -> Id ride -> Id person -> [(LatLong, Int64)] -> NonEmpty (LatLong, Int64) -> m ()
addIntermediatePickupPoints ih rideId driverId accWaypointsWithTime allWaypointsWithTime = withRideIdLogTag rideId $ do
  I.processWaypointsPickup ih driverId False accWaypointsWithTime allWaypointsWithTime

-- | The pickup leg keeps its Redis state under a ":pickup"-suffixed driver id, so it never
-- shares buffers with the on-ride leg.
pickupLegDriverId :: Id person -> Id person
pickupLegDriverId driverId = Id (driverId.getId <> ":pickup")

-- | Drops any pickup-leg distance state left in Redis for this driver.
--
-- The pickup leg is only finalized when a ride starts; an assignment that is cancelled or
-- reallocated before start leaves its waypoints and its cumulative @distanceTravelled@
-- behind for the keys' TTL (6-24h). The next assignment for the same driver then appends
-- to that leftover state, so its @distanceToPickup@ comes out as the sum of every pickup
-- leg since the last ride that actually started. Call this at assignment and on
-- cancellation to keep each leg self-contained.
resetPickupLegState :: (HedisFlow m env) => Id person -> m ()
resetPickupLegState = I.clearAccumulatedDistanceState . pickupLegDriverId

isDistanceCalculationFailed :: I.RideInterpolationHandler person m -> Id person -> m Bool
isDistanceCalculationFailed ih = ih.isDistanceCalculationFailed

isPassedThroughDrop :: (CacheFlow m r, Log m, MonadThrow m, MonadFlow m) => Id person -> m Bool
isPassedThroughDrop = I.getPassedThroughDrop
