module Product.RideAPI.Handlers.StartRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import EulerHS.Prelude
import Types.Error
import qualified Types.Storage.Person as Person
import qualified Types.Storage.Quote as Quote
import qualified Types.Storage.Ride as Ride
import Utils.Common

data ServiceHandle m = ServiceHandle
  { findPersonById :: Id Person.Person -> m (Maybe Person.Person),
    findPIById :: Id Quote.Quote -> m (Maybe Quote.Quote),
    findRideById :: Id Ride.Ride -> m (Maybe Ride.Ride),
    startRide :: Id Ride.Ride -> m (),
    notifyBAPRideStarted :: Quote.Quote -> Ride.Ride -> m (),
    rateLimitStartRide :: Id Person.Person -> Id Ride.Ride -> m ()
  }

startRideHandler :: (MonadThrow m, Log m) => ServiceHandle m -> Id Person.Person -> Id Ride.Ride -> Text -> m APISuccess.APISuccess
startRideHandler ServiceHandle {..} requestorId rideId otp = do
  rateLimitStartRide requestorId rideId
  requestor <- findPersonById requestorId >>= fromMaybeM PersonNotFound
  ride <- findRideById (cast rideId) >>= fromMaybeM RideDoesNotExist
  case requestor.role of
    Person.DRIVER -> do
      rideDriver <- ride.personId & fromMaybeM (RideFieldNotPresent "person")
      unless (rideDriver == requestorId) $ throwError NotAnExecutor
    _ -> throwError AccessDenied
  unless (isValidRideStatus (ride.status)) $ throwError $ RideInvalidStatus "This ride cannot be started"
  let quoteId = ride.quoteId
  quote <- findPIById quoteId >>= fromMaybeM QuoteNotFound
  inAppOtp <- ride.udf4 & fromMaybeM (QuoteFieldNotPresent "udf4")
  when (otp /= inAppOtp) $ throwError IncorrectOTP
  logTagInfo "startRide" ("DriverId " <> getId requestorId <> ", RideId " <> getId rideId)
  startRide ride.id
  notifyBAPRideStarted quote ride{status = Ride.INPROGRESS}
  pure APISuccess.Success
  where
    isValidRideStatus status = status `elem` [Ride.CONFIRMED, Ride.TRIP_ASSIGNED, Ride.INSTOCK]
