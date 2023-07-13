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
    addIntermediateRoutePoints,
    getInterpolatedPoints,
    clearInterpolatedPoints,
    isPickupDistanceCalculationFailed,
    initializePickupDistanceCalculation,
    finalPickupDistanceCalculation,
    getPickupInterpolatedPoints,
    clearPickupInterpolatedPoints,
    addIntermediatePickupRoutePoints,
    getFirstNPickupwaypoints,
    updatePickupDistance,
  )
where

import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra
import Kernel.External.Maps.Types
import Kernel.Types.Common
import Kernel.Types.Id (Id)
import qualified Lib.LocationUpdates.Internal as I

withRideIdLogTag :: (Log m) => Id ride -> m a -> m a
withRideIdLogTag rideId = withLogTag ("locupd-rideId-" <> rideId.getId)

-- API
-- All these functions take rideId such that it was possible later to fetch location updates
-- and interpolated route points from log files by the visualization tool (see <beckn>/app/utils/route-extractor and dev/visualize-ride)

initializeDistanceCalculation :: (Monad m, Log m) => I.RideInterpolationHandler person m -> Id ride -> Id person -> LatLong -> m ()
initializeDistanceCalculation ih rideId driverId pt = withRideIdLogTag rideId $ do
  ih.clearLocationUpdates driverId
  ih.clearInterpolatedPoints driverId
  ih.expireInterpolatedPoints driverId
  ih.addPoints driverId $ pt :| []

finalDistanceCalculation :: (Log m, MonadThrow m) => I.RideInterpolationHandler person m -> Id ride -> Id person -> LatLong -> m ()
finalDistanceCalculation ih rideId driverId pt = withRideIdLogTag rideId $ I.processWaypoints ih driverId True $ pt :| []

getInterpolatedPoints :: I.RideInterpolationHandler person m -> Id person -> m [LatLong]
getInterpolatedPoints ih = ih.getInterpolatedPoints

clearInterpolatedPoints :: I.RideInterpolationHandler person m -> Id person -> m ()
clearInterpolatedPoints ih = ih.clearInterpolatedPoints

addIntermediateRoutePoints :: (Log m, MonadThrow m) => I.RideInterpolationHandler person m -> Id ride -> Id person -> NonEmpty LatLong -> m ()
addIntermediateRoutePoints ih rideId driverId = withRideIdLogTag rideId . I.processWaypoints ih driverId False

isDistanceCalculationFailed :: I.RideInterpolationHandler person m -> Id person -> m Bool
isDistanceCalculationFailed ih = ih.isDistanceCalculationFailed

initializePickupDistanceCalculation :: (Monad m, Log m) => I.RideInterpolationHandler person m -> Id ride -> Id person -> LatLong -> m ()
initializePickupDistanceCalculation ih rideId driverId pt = withRideIdLogTag rideId $ do
  ih.clearPickupLocationUpdates driverId
  ih.clearPickupInterpolatedPoints driverId
  ih.expirePickupInterpolatedPoints driverId
  ih.addPickupPoints driverId $ pt :| []

finalPickupDistanceCalculation :: (Log m, MonadThrow m) => I.RideInterpolationHandler person m -> Id ride -> Id person -> LatLong -> m ()
finalPickupDistanceCalculation ih rideId driverId pt = withRideIdLogTag rideId $ I.processPickupWaypoints ih driverId True $ pt :| []

getPickupInterpolatedPoints :: I.RideInterpolationHandler person m -> Id person -> m [LatLong]
getPickupInterpolatedPoints ih = ih.getPickupInterpolatedPoints

clearPickupInterpolatedPoints :: I.RideInterpolationHandler person m -> Id person -> m ()
clearPickupInterpolatedPoints ih = ih.clearPickupInterpolatedPoints

addIntermediatePickupRoutePoints :: (Log m, MonadThrow m) => I.RideInterpolationHandler person m -> Id ride -> Id person -> NonEmpty LatLong -> m ()
addIntermediatePickupRoutePoints ih rideId driverId = withRideIdLogTag rideId . I.processPickupWaypoints ih driverId False

isPickupDistanceCalculationFailed :: I.RideInterpolationHandler person m -> Id person -> m Bool
isPickupDistanceCalculationFailed ih = ih.isPickupDistanceCalculationFailed

getFirstNPickupwaypoints :: I.RideInterpolationHandler person m -> Id person -> Integer -> m [LatLong]
getFirstNPickupwaypoints ih = ih.getFirstNPickupwaypoints

updatePickupDistance :: I.RideInterpolationHandler person m -> Id person -> HighPrecMeters -> m ()
updatePickupDistance ih = ih.updateDistance
