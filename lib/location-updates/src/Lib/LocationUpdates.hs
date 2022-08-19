module Lib.LocationUpdates
  ( I.RideInterpolationHandler,
    I.mkHandlerWithDefaultRedisFuncs,
    startRide,
    endRide,
    updateIntermediateRideLocation,
    isDistanceCalculationFailed,
  )
where

import Beckn.Types.Common
import Beckn.Types.Id (Id)
import Beckn.Types.MapSearch
import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra
import qualified Lib.LocationUpdates.Internal as I

-- API
startRide :: (Monad m) => I.RideInterpolationHandler person m -> Id person -> LatLong -> m ()
startRide ih driverId pt = do
  ih.clearLocationUpdates driverId
  ih.addPoints driverId $ pt :| []

endRide :: (Monad m, Log m, MonadThrow m) => I.RideInterpolationHandler person m -> Id person -> LatLong -> m ()
endRide ih driverId pt = I.processWaypoints ih driverId True $ pt :| []

updateIntermediateRideLocation :: (Log m, MonadThrow m) => I.RideInterpolationHandler person m -> Id person -> NonEmpty LatLong -> m ()
updateIntermediateRideLocation ih driverId = I.processWaypoints ih driverId False

isDistanceCalculationFailed :: I.RideInterpolationHandler person m -> Id person -> m Bool
isDistanceCalculationFailed ih = ih.isDistanceCalculationFailed
