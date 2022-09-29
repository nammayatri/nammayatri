module Lib.LocationUpdates
  ( I.RideInterpolationHandler,
    I.mkHandlerWithDefaultRedisFuncs,
    isDistanceCalculationFailed,
    initializeDistanceCalculation,
    finalDistanceCalculation,
    addIntermediateRoutePoints,
  )
where

import Beckn.Types.Common
import Beckn.Types.Id (Id)
import Beckn.Types.MapSearch
import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra
import qualified Lib.LocationUpdates.Internal as I

-- API
initializeDistanceCalculation :: (Monad m) => I.RideInterpolationHandler person m -> Id person -> LatLong -> m ()
initializeDistanceCalculation ih driverId pt = do
  ih.clearLocationUpdates driverId
  ih.addPoints driverId $ pt :| []

finalDistanceCalculation :: (Monad m, Log m, MonadThrow m) => I.RideInterpolationHandler person m -> Id person -> LatLong -> m ()
finalDistanceCalculation ih driverId pt = I.processWaypoints ih driverId True $ pt :| []

addIntermediateRoutePoints :: (Log m, MonadThrow m) => I.RideInterpolationHandler person m -> Id person -> NonEmpty LatLong -> m ()
addIntermediateRoutePoints ih driverId = I.processWaypoints ih driverId False

isDistanceCalculationFailed :: I.RideInterpolationHandler person m -> Id person -> m Bool
isDistanceCalculationFailed ih = ih.isDistanceCalculationFailed
