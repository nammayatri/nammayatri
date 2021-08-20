module Product.RideAPI.Handlers.CancelRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Types.Mobility.Order as Mobility
import EulerHS.Prelude
import Types.API.Ride (CancelRideReq (..))
import Types.Error
import qualified Types.Storage.RideCancellationReason as SRCR
import qualified Types.Storage.Person as Person
import qualified Types.Storage.Ride as SRide
import Utils.Common

type MonadHandler m = (MonadThrow m, Log m)

data ServiceHandle m = ServiceHandle
  { findRideById :: Id SRide.Ride -> m (Maybe SRide.Ride),
    findPersonById :: Id Person.Person -> m (Maybe Person.Person),
    cancelRide :: Id SRide.Ride -> SRCR.RideCancellationReason -> m ()
  }

cancelRideHandler :: MonadHandler m => ServiceHandle m -> Id Person.Person -> Id SRide.Ride -> CancelRideReq -> m APISuccess.APISuccess
cancelRideHandler ServiceHandle {..} personId rideId req = do
  ride <- findRideById rideId >>= fromMaybeM RideDoesNotExist
  unless (isValidRide ride) $ throwError $ QuoteInvalidStatus "This ride cannot be canceled"
  authPerson <-
    findPersonById personId
      >>= fromMaybeM PersonNotFound
  case authPerson.role of
    Person.ADMIN -> cancelRide rideId $ rideCancelationReason Mobility.ByOrganization ride.bookingId
    Person.DRIVER -> do
      let driverId = ride.driverId
      unless (authPerson.id == driverId) $ throwError NotAnExecutor
      cancelRide rideId $ rideCancelationReason Mobility.ByDriver ride.bookingId
  pure APISuccess.Success
  where
    isValidRide ride =
      ride.status == SRide.NEW
    rideCancelationReason source rideBookingId = do
      let CancelRideReq {..} = req
      SRCR.RideCancellationReason
        { rideBookingId = rideBookingId,
          source = source,
          reasonCode = Just reasonCode,
          ..
        }
