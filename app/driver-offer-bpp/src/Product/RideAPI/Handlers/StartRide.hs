module Product.RideAPI.Handlers.StartRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.MapSearch
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideBooking as SRB
import EulerHS.Prelude
import Types.API.Ride
import Types.Error
import Utils.Common

data ServiceHandle m = ServiceHandle
  { findById :: Id Person.Person -> m (Maybe Person.Person),
    findRideBookingById :: Id SRB.RideBooking -> m (Maybe SRB.RideBooking),
    findRideById :: Id SRide.Ride -> m (Maybe SRide.Ride),
    startRideAndUpdateLocation :: Id SRide.Ride -> Id SRB.RideBooking -> Id Person.Person -> LatLong -> m (),
    notifyBAPRideStarted :: SRB.RideBooking -> SRide.Ride -> m (),
    rateLimitStartRide :: Id Person.Person -> Id SRide.Ride -> m (),
    addFirstWaypoint :: Id Person.Person -> LatLong -> m ()
  }

startRideHandler :: (MonadThrow m, Log m) => ServiceHandle m -> Id Person.Person -> Id SRide.Ride -> StartRideReq -> m APISuccess.APISuccess
startRideHandler ServiceHandle {..} requestorId rideId req = do
  rateLimitStartRide requestorId rideId
  requestor <- findById requestorId >>= fromMaybeM (PersonNotFound requestorId.getId)
  ride <- findRideById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  case requestor.role of
    Person.DRIVER -> do
      let rideDriver = ride.driverId
      unless (rideDriver == requestorId) $ throwError NotAnExecutor
    _ -> throwError AccessDenied
  let driverId = requestorId
  unless (isValidRideStatus (ride.status)) $ throwError $ RideInvalidStatus "This ride cannot be started"
  rideBooking <- findRideBookingById ride.bookingId >>= fromMaybeM (RideBookingNotFound ride.bookingId.getId)
  let inAppOtp = ride.otp
  when (req.rideOtp /= inAppOtp) $ throwError IncorrectOTP
  logTagInfo "startRide" ("DriverId " <> getId requestorId <> ", RideId " <> getId rideId)
  startRideAndUpdateLocation ride.id rideBooking.id requestorId req.point
  addFirstWaypoint driverId req.point
  notifyBAPRideStarted rideBooking ride
  pure APISuccess.Success
  where
    isValidRideStatus status = status == SRide.NEW
