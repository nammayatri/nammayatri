module Product.RideAPI.Handlers.CancelRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Types.Mobility.Order as Mobility
import EulerHS.Prelude
import Types.API.Ride (CancelRideReq (..))
import Types.Error
import qualified Types.Storage.Person as SP
import qualified Types.Storage.RideCancellationReason as SRCR
import qualified Types.Storage.Ride as Ride
import Utils.Common

type MonadHandler m = (MonadThrow m, Log m)

data ServiceHandle m = ServiceHandle
  { findRideById :: Id Ride.Ride -> m (Maybe Ride.Ride),
    findPersonById :: Id SP.Person -> m (Maybe SP.Person),
    cancelRide :: Id Ride.Ride -> SRCR.RideCancellationReason -> m ()
  }

cancelRideHandler :: MonadHandler m => ServiceHandle m -> Id SP.Person -> Id Ride.Ride -> CancelRideReq -> m APISuccess.APISuccess
cancelRideHandler ServiceHandle {..} personId rideId req = do
  ride <- findRideById rideId >>= fromMaybeM RideDoesNotExist
  unless (isValidRide ride) $ throwError $ PIInvalidStatus "This ride cannot be canceled"
  authPerson <-
    findPersonById personId
      >>= fromMaybeM PersonNotFound
  case authPerson.role of
    SP.ADMIN -> cancelRide rideId $ rideCancelationReason Mobility.ByOrganization
    SP.DRIVER -> do
      driverId <- ride.personId & fromMaybeM (RideFieldNotPresent "person")
      unless (authPerson.id == driverId) $ throwError NotAnExecutor
      cancelRide rideId $ rideCancelationReason Mobility.ByDriver
  pure APISuccess.Success
  where
    isValidRide ride =
      (ride.status) `elem` [Ride.CONFIRMED, Ride.TRIP_ASSIGNED, Ride.TRIP_REASSIGNMENT]
    rideCancelationReason source = do
      let CancelRideReq {..} = req
      SRCR.RideCancellationReason
        { rideId = rideId,
          source = source,
          reasonCode = Just reasonCode,
          ..
        }
