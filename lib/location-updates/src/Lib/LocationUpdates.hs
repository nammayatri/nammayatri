module Lib.LocationUpdates
  ( I.RideInterpolationHandler,
    I.mkHandlerWithDefaultRedisFuncs,
    isDistanceCalculationFailed,
    initializeDistanceCalculation,
    finalDistanceCalculation,
    addIntermediateRoutePoints,
  )
where

import Beckn.External.Maps.Types
import Beckn.Types.Common
import Beckn.Types.Id (Id)
import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra
import qualified Lib.LocationUpdates.Internal as I

withRideIdLogTag :: (Log m) => Id ride -> m a -> m a
withRideIdLogTag rideId = withLogTag ("locupd-rideId-" <> rideId.getId)

-- API
-- All these functions take rideId such that it was possible later to fetch location updates
-- and interpolated route points from log files by the visualization tool (see <beckn>/app/utils/route-extractor and dev/visualize-ride)

initializeDistanceCalculation :: (Monad m, Log m) => I.RideInterpolationHandler person m -> Id ride -> Id person -> LatLong -> m ()
initializeDistanceCalculation ih rideId driverId pt = withRideIdLogTag rideId $ do
  ih.clearLocationUpdates driverId
  ih.addPoints driverId $ pt :| []

finalDistanceCalculation :: (Monad m, Log m, MonadThrow m) => I.RideInterpolationHandler person m -> Id ride -> Id person -> LatLong -> m ()
finalDistanceCalculation ih rideId driverId pt = withRideIdLogTag rideId $ I.processWaypoints ih driverId True $ pt :| []

addIntermediateRoutePoints :: (Log m, MonadThrow m) => I.RideInterpolationHandler person m -> Id ride -> Id person -> NonEmpty LatLong -> m ()
addIntermediateRoutePoints ih rideId driverId = withRideIdLogTag rideId . I.processWaypoints ih driverId False

isDistanceCalculationFailed :: I.RideInterpolationHandler person m -> Id person -> m Bool
isDistanceCalculationFailed ih = ih.isDistanceCalculationFailed
