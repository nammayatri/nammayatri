module Product.RideAPI.Handlers.EndRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import Data.Time (NominalDiffTime)
import qualified Domain.Types.DriverLocation as DrLoc
import Domain.Types.Organization (Organization)
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.RideBooking as SRB
import qualified Domain.Types.SearchRequest as SSearchRequest
import qualified Domain.Types.Vehicle as Vehicle
import EulerHS.Prelude hiding (pi)
import qualified Product.FareCalculator.Interpreter as Fare
import Product.Location
import Types.App (Driver)
import Types.Error
import Utils.Common

data ServiceHandle m = ServiceHandle
  { findById :: Id Person.Person -> m (Maybe Person.Person),
    findRideBookingById :: Id SRB.RideBooking -> m (Maybe SRB.RideBooking),
    findRideById :: Id Ride.Ride -> m (Maybe Ride.Ride),
    endRideTransaction :: Id SRB.RideBooking -> Ride.Ride -> Id Driver -> m (),
    findSearchRequestById :: Id SSearchRequest.SearchRequest -> m (Maybe SSearchRequest.SearchRequest),
    notifyCompleteToBAP :: SRB.RideBooking -> Ride.Ride -> m (),
    calculateFare ::
      Id Organization ->
      Vehicle.Variant ->
      Meter ->
      UTCTime ->
      m Fare.FareParameters,
    recalculateFareEnabled :: m Bool,
    putDiffMetric :: Amount -> Double -> m (),
    findDriverLocById :: Id Person.Person -> m (Maybe DrLoc.DriverLocation),
    getKeyRedis :: Text -> m (Maybe ()),
    updateLocationAllowedDelay :: m NominalDiffTime,
    recalcDistanceEnding :: Id Person.Person -> m ()
  }

endRideHandler ::
  (MonadThrow m, Log m, MonadTime m) =>
  ServiceHandle m ->
  Id Person.Person ->
  Id Ride.Ride ->
  m APISuccess.APISuccess
endRideHandler ServiceHandle {..} requestorId rideId = do
  requestor <- findById requestorId >>= fromMaybeM PersonNotFound

  recalcDistanceEnding requestorId

  ride <- findRideById (cast rideId) >>= fromMaybeM RideDoesNotExist
  let driverId = ride.driverId
  case requestor.role of
    Person.DRIVER -> unless (requestorId == driverId) $ throwError NotAnExecutor
    _ -> throwError AccessDenied
  unless (ride.status == Ride.INPROGRESS) $ throwError $ RideInvalidStatus "This ride cannot be ended"

  rideBooking <- findRideBookingById ride.bookingId >>= fromMaybeM RideBookingNotFound
  logTagInfo "endRide" ("DriverId " <> getId requestorId <> ", RideId " <> getId rideId)

  (chargeableDistance, fare, totalFare) <- recalculateFare rideBooking ride

  let updRide =
        ride{chargeableDistance = Just chargeableDistance,
             fare = Just fare,
             totalFare = Just totalFare
            }

  endRideTransaction rideBooking.id updRide (cast driverId)

  notifyCompleteToBAP rideBooking updRide

  return APISuccess.Success
  where
    lastLocUdateTooLongAgo = do
      now <- getCurrentTime
      allowedUpdatesDelay <- updateLocationAllowedDelay
      res <-
        findDriverLocById requestorId
          <&> maybe True (\loc -> now `diffUTCTime` loc.updatedAt > allowedUpdatesDelay)
      logDebug $ "last update was too long ago: " <> show res
      pure res

    thereWereMissingLocUpdates = do
      res <-
        getKeyRedis (missingLocationUpdatesKey rideId)
          <&> isJust
      logDebug $ "there were missing location updates: " <> show res
      pure res

    recalculateFare rideBooking ride = do
      let transporterId = rideBooking.providerId
          vehicleVariant = rideBooking.vehicleVariant
          oldDistance = rideBooking.distance
          estimatedFare = rideBooking.estimatedFare
      shouldRecalculateFare <- recalculateFareEnabled
      missingLocationUpdates <-
        (||)
          <$> lastLocUdateTooLongAgo
          <*> thereWereMissingLocUpdates
      if shouldRecalculateFare && not missingLocationUpdates
        then do
          let actualDistance = ride.traveledDistance
          fareParams <- calculateFare transporterId vehicleVariant (Meter actualDistance) rideBooking.startTime
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
