module Product.RideAPI.Handlers.EndRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (pi)
import Types.App (Driver)
import Types.Error
import qualified Types.Storage.SearchRequest as SSearchRequest
import Types.Storage.Organization (Organization)
import qualified Types.Storage.Vehicle as Vehicle
import qualified Types.Storage.Person as Person
import qualified Types.Storage.Ride as Ride
import qualified Types.Storage.RideBooking as SRB
import Utils.Common
import qualified Types.Storage.Quote as SQuote

data ServiceHandle m = ServiceHandle
  { findPersonById :: Id Person.Person -> m (Maybe Person.Person),
    findRideBookingById :: Id SRB.RideBooking -> m (Maybe SRB.RideBooking),
    findRideById :: Id Ride.Ride -> m (Maybe Ride.Ride),
    findQuoteById :: Id SQuote.Quote -> m (Maybe SQuote.Quote),
    endRideTransaction :: Id SRB.RideBooking -> Id Ride.Ride -> Id Driver -> Amount -> m (),
    findSearchRequestById :: Id SSearchRequest.SearchRequest -> m (Maybe SSearchRequest.SearchRequest),
    notifyUpdateToBAP :: SQuote.Quote -> SRB.RideBooking -> Ride.Ride -> Ride.RideStatus -> m (),
    calculateFare ::
      Id Organization ->
      Vehicle.Variant ->
      Double ->
      UTCTime ->
      m Amount,
    recalculateFareEnabled :: m Bool,
    putDiffMetric :: Amount -> Double -> m ()
  }

endRideHandler ::
  (MonadThrow m, Log m) =>
  ServiceHandle m ->
  Id Person.Person ->
  Id Ride.Ride ->
  m APISuccess.APISuccess
endRideHandler ServiceHandle {..} requestorId rideId = do
  requestor <- findPersonById requestorId >>= fromMaybeM PersonNotFound
  ride <- findRideById (cast rideId) >>= fromMaybeM RideDoesNotExist
  let driverId = ride.driverId
  case requestor.role of
    Person.DRIVER -> unless (requestorId == driverId) $ throwError NotAnExecutor
    _ -> throwError AccessDenied
  unless (ride.status == Ride.INPROGRESS) $ throwError $ RideInvalidStatus "This ride cannot be ended"

  rideBooking <- findRideBookingById ride.bookingId >>= fromMaybeM RideBookingNotFound
  logTagInfo "endRide" ("DriverId " <> getId requestorId <> ", RideId " <> getId rideId)

  actualPrice <-
    ifM
      recalculateFareEnabled
      (recalculateFare rideBooking ride)
      (return rideBooking.price)

  endRideTransaction rideBooking.id ride.id (cast driverId) actualPrice

  quote <- findQuoteById rideBooking.quoteId >>= fromMaybeM QuoteNotFound

  notifyUpdateToBAP quote rideBooking (updateActualPrice actualPrice ride){status = Ride.COMPLETED} Ride.COMPLETED

  return APISuccess.Success
  where
    recalculateFare rideBooking ride = do
      let transporterId = rideBooking.providerId
          vehicleVariant = rideBooking.vehicleVariant
          oldDistance = rideBooking.distance 
          actualDistance = ride.finalDistance
      fare <- calculateFare transporterId vehicleVariant actualDistance rideBooking.startTime
      let distanceDiff = actualDistance - oldDistance
      let price = rideBooking.price
      let fareDiff = fare - price
      logTagInfo "Fare recalculation" $
        "Fare difference: "
          <> show (amountToDouble fareDiff)
          <> ", Distance difference: "
          <> show distanceDiff
      putDiffMetric fareDiff distanceDiff
      pure fare
    updateActualPrice :: Amount -> Ride.Ride -> Ride.Ride
    updateActualPrice = \p ride -> ride{finalPrice = Just p}
