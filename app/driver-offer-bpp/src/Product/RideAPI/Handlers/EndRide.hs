module Product.RideAPI.Handlers.EndRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.MapSearch
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.DriverLocation as DrLoc
import Domain.Types.FareParams as Fare
import Domain.Types.Organization (Organization)
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import EulerHS.Prelude hiding (pi)
import Product.FareCalculator.Calculator as Fare
import Types.API.Ride (EndRideReq)
import Types.App (Driver)
import Types.Error
import Utils.Common

data ServiceHandle m = ServiceHandle
  { findById :: Id Person.Person -> m (Maybe Person.Person),
    findBookingById :: Id SRB.Booking -> m (Maybe SRB.Booking),
    findRideById :: Id Ride.Ride -> m (Maybe Ride.Ride),
    endRideTransaction :: Id SRB.Booking -> Ride.Ride -> Id Driver -> m (),
    notifyCompleteToBAP :: SRB.Booking -> Ride.Ride -> Fare.FareParameters -> m (),
    calculateFare ::
      Id Organization ->
      --      Vehicle.Variant ->
      HighPrecMeters ->
      UTCTime ->
      Maybe Amount ->
      m Fare.FareParameters,
    putDiffMetric :: Amount -> HighPrecMeters -> m (),
    findDriverLocById :: Id Person.Person -> m (Maybe DrLoc.DriverLocation),
    getKeyRedis :: Text -> m (Maybe ()),
    addLastWaypointAndRecalcDistanceOnEnd :: Id Person.Person -> LatLong -> m ()
  }

endRideHandler ::
  (MonadThrow m, Log m, MonadTime m) =>
  ServiceHandle m ->
  Id Person.Person ->
  Id Ride.Ride ->
  EndRideReq ->
  m APISuccess.APISuccess
endRideHandler ServiceHandle {..} requestorId rideId req = do
  requestor <- findById requestorId >>= fromMaybeM (PersonNotFound requestorId.getId)

  addLastWaypointAndRecalcDistanceOnEnd requestorId req.point
  -- here we update the current ride, so below we fetch the updated version

  ride <- findRideById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  let driverId = ride.driverId
  case requestor.role of
    Person.DRIVER -> unless (requestorId == driverId) $ throwError NotAnExecutor
    _ -> throwError AccessDenied
  unless (ride.status == Ride.INPROGRESS) $ throwError $ RideInvalidStatus "This ride cannot be ended"

  booking <- findBookingById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  logTagInfo "endRide" ("DriverId " <> getId requestorId <> ", RideId " <> getId rideId)

  now <- getCurrentTime
  putDiffs booking ride

  let updRide =
        ride{tripEndTime = Just now
            }

  endRideTransaction booking.id updRide (cast driverId)

  notifyCompleteToBAP booking updRide booking.fareParams

  return APISuccess.Success
  where
    putDiffs booking ride = do
      let transporterId = booking.providerId
          actualDistance = ride.traveledDistance
          oldDistance = booking.estimatedDistance

      -- maybe compare only distance fare?
      let estimatedBaseFare = fareSum booking.fareParams

      fareParams <- calculateFare transporterId actualDistance booking.startTime booking.fareParams.driverSelectedFare
      let updatedBaseFare = Fare.fareSum fareParams
      let distanceDiff = actualDistance - oldDistance
      let fareDiff = updatedBaseFare - estimatedBaseFare
      logTagInfo "Fare recalculation" $
        "Fare difference: "
          <> show (amountToDouble fareDiff)
          <> ", Distance difference: "
          <> show distanceDiff
      putDiffMetric fareDiff distanceDiff
