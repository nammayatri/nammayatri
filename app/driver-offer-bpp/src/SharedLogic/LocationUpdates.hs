module SharedLogic.LocationUpdates where

import Beckn.LocationUpdates
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Domain.Types.Person as Person
import Environment
import qualified Storage.Queries.Ride as QRide

defaultRideInterpolationHandler :: RideInterpolationHandler Person.Person Flow
defaultRideInterpolationHandler = mkHandlerWithDefaultRedisFuncs $
  \driverId dist -> Esq.runTransaction $ QRide.updateDistance driverId dist
