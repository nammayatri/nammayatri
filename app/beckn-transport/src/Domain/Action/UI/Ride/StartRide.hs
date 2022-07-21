module Domain.Action.UI.Ride.StartRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude
import Types.Error
import Utils.Common

data ServiceHandle m = ServiceHandle
  { findById :: Id Person.Person -> m (Maybe Person.Person),
    findBookingById :: Id SRB.Booking -> m (Maybe SRB.Booking),
    findRideById :: Id SRide.Ride -> m (Maybe SRide.Ride),
    startRide :: Id SRide.Ride -> Id SRB.Booking -> Id Person.Person -> m (),
    notifyBAPRideStarted :: SRB.Booking -> SRide.Ride -> m (),
    rateLimitStartRide :: Id Person.Person -> Id SRide.Ride -> m ()
  }

startRideHandler :: (MonadThrow m, Log m) => ServiceHandle m -> Id Person.Person -> Id SRide.Ride -> Text -> m APISuccess.APISuccess
startRideHandler ServiceHandle {..} requestorId rideId otp = do
  rateLimitStartRide requestorId rideId
  requestor <- findById requestorId >>= fromMaybeM (PersonNotFound requestorId.getId)
  ride <- findRideById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  case requestor.role of
    Person.DRIVER -> do
      let rideDriver = ride.driverId
      unless (rideDriver == requestorId) $ throwError NotAnExecutor
    _ -> throwError AccessDenied
  unless (isValidRideStatus (ride.status)) $ throwError $ RideInvalidStatus "This ride cannot be started"
  booking <- findBookingById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let inAppOtp = ride.otp
  when (otp /= inAppOtp) $ throwError IncorrectOTP
  logTagInfo "startRide" ("DriverId " <> getId requestorId <> ", RideId " <> getId rideId)
  startRide ride.id booking.id requestorId
  notifyBAPRideStarted booking ride
  pure APISuccess.Success
  where
    isValidRideStatus status = status == SRide.NEW
