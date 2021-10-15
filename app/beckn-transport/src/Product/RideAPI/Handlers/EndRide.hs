module Product.RideAPI.Handlers.EndRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (pi)
import qualified Product.FareCalculator.Interpreter as Fare
import Types.App (Driver)
import Types.Error
import Types.Storage.Organization (Organization)
import qualified Types.Storage.Person as Person
import qualified Types.Storage.Quote as SQuote
import qualified Types.Storage.Ride as Ride
import qualified Types.Storage.RideBooking as SRB
import qualified Types.Storage.SearchRequest as SSearchRequest
import qualified Types.Storage.Vehicle as Vehicle
import Utils.Common

data ServiceHandle m = ServiceHandle
  { findPersonById :: Id Person.Person -> m (Maybe Person.Person),
    findRideBookingById :: Id SRB.RideBooking -> m (Maybe SRB.RideBooking),
    findRideById :: Id Ride.Ride -> m (Maybe Ride.Ride),
    findQuoteById :: Id SQuote.Quote -> m (Maybe SQuote.Quote),
    endRideTransaction :: Id SRB.RideBooking -> Ride.Ride -> Id Driver -> m (),
    findSearchRequestById :: Id SSearchRequest.SearchRequest -> m (Maybe SSearchRequest.SearchRequest),
    notifyCompleteToBAP :: SQuote.Quote -> SRB.RideBooking -> Ride.Ride -> m (),
    calculateFare ::
      Id Organization ->
      Vehicle.Variant ->
      Double ->
      UTCTime ->
      m Fare.FareParameters,
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

  (chargeableDistance, actualFare, totalFare) <- recalculateFare rideBooking ride

  let updRide = 
        ride{chargeableDistance = Just chargeableDistance,
             finalPrice = Just actualFare,
             totalFare = Just totalFare
            }

  endRideTransaction rideBooking.id updRide (cast driverId)

  quote <- findQuoteById rideBooking.quoteId >>= fromMaybeM QuoteNotFound

  notifyCompleteToBAP quote rideBooking updRide

  return APISuccess.Success
  where
    recalculateFare rideBooking ride = do
      let transporterId = rideBooking.providerId
          vehicleVariant = rideBooking.vehicleVariant
          oldDistance = rideBooking.distance
          estimatedFare = rideBooking.price
      shouldRecalculateFare <- recalculateFareEnabled
      if shouldRecalculateFare
        then do
          let actualDistance = ride.traveledDistance
          fareParams <- calculateFare transporterId vehicleVariant actualDistance rideBooking.startTime
          let updatedFare = Fare.fareSum fareParams
              totalFare = Fare.fareSumWithDiscount fareParams
          let distanceDiff = actualDistance - oldDistance
          let fareDiff = updatedFare - estimatedFare
          logTagInfo "Fare recalculation" $
            "Fare difference: "
              <> show (amountToDouble fareDiff)
              <> ", Distance difference: "
              <> show distanceDiff
          putDiffMetric fareDiff distanceDiff
          pure (actualDistance, updatedFare, totalFare)
        else pure (oldDistance, estimatedFare, rideBooking.estimatedTotalFare)