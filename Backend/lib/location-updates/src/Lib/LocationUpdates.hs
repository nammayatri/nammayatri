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
    isPassedThroughDrop,
    getTravelledDistanceOutsideThreshold,
  )
where

import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra
import Kernel.External.Maps.Interface.Types (MapsServiceConfig)
import Kernel.External.Maps.Types
import Kernel.Types.CacheFlow
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

finalDistanceCalculation :: (CacheFlow m r, Log m, MonadThrow m, MonadFlow m) => I.RideInterpolationHandler person m -> Maybe MapsServiceConfig -> Bool -> Bool -> Id ride -> Id person -> NonEmpty LatLong -> Meters -> Maybe HighPrecMoney -> Maybe [Text] -> Maybe [Text] -> Bool -> Bool -> Bool -> m ()
finalDistanceCalculation ih rectifyDistantPointsFailureUsing isTollApplicable sendTollCrossedNotification rideId driverId pts estDist estTollCharges estTollNames estTollIds pickupDropOutsideThreshold passedThroughDrop isMeterRide = withRideIdLogTag rideId $ I.processWaypoints ih driverId True estDist estTollCharges estTollNames estTollIds pickupDropOutsideThreshold rectifyDistantPointsFailureUsing isTollApplicable sendTollCrossedNotification passedThroughDrop isMeterRide pts

getInterpolatedPoints :: I.RideInterpolationHandler person m -> Id person -> m [LatLong]
getInterpolatedPoints ih = ih.getInterpolatedPoints

clearInterpolatedPoints :: I.RideInterpolationHandler person m -> Id person -> m ()
clearInterpolatedPoints ih = ih.clearInterpolatedPoints

addIntermediateRoutePoints :: (CacheFlow m r, Log m, MonadThrow m, MonadFlow m) => I.RideInterpolationHandler person m -> Maybe MapsServiceConfig -> Bool -> Bool -> Id ride -> Id person -> Bool -> Bool -> NonEmpty LatLong -> m ()
addIntermediateRoutePoints ih rectifyDistantPointsFailureUsing isTollApplicable sendTollCrossedNotification rideId driverId passedThroughDrop isMeterRide = withRideIdLogTag rideId . I.processWaypoints ih driverId False 0 Nothing Nothing Nothing False rectifyDistantPointsFailureUsing isTollApplicable sendTollCrossedNotification passedThroughDrop isMeterRide -- estimateDistace, estimatedTollCharges and estimatedTollIds not required in case of add intermediatory points

isDistanceCalculationFailed :: I.RideInterpolationHandler person m -> Id person -> m Bool
isDistanceCalculationFailed ih = ih.isDistanceCalculationFailed

isPassedThroughDrop :: (CacheFlow m r, Log m, MonadThrow m, MonadFlow m) => Id person -> m Bool
isPassedThroughDrop = I.getPassedThroughDrop

getTravelledDistanceOutsideThreshold :: (CacheFlow m r, Log m, MonadThrow m, MonadFlow m) => Id person -> m Meters
getTravelledDistanceOutsideThreshold = I.getTravelledDistanceOutsideThreshold
