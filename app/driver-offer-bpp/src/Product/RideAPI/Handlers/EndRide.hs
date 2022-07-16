module Product.RideAPI.Handlers.EndRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.MapSearch
import qualified Domain.Types.DriverLocation as DrLoc
import Domain.Types.FareParams as Fare
import Domain.Types.Organization (Organization)
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.RideBooking as SRB
import EulerHS.Prelude hiding (pi)
import Product.FareCalculator.Calculator as Fare
import Types.API.Ride (EndRideReq)
import Types.App (Driver)
import Types.Error
import Utils.Common

data ServiceHandle m = ServiceHandle
  { findById :: Id Person.Person -> m (Maybe Person.Person),
    findRideBookingById :: Id SRB.RideBooking -> m (Maybe SRB.RideBooking),
    findRideById :: Id Ride.Ride -> m (Maybe Ride.Ride),
    endRideTransaction :: Id SRB.RideBooking -> Ride.Ride -> Id Driver -> m (),
    notifyCompleteToBAP :: SRB.RideBooking -> Ride.Ride -> Fare.FareParameters -> m (),
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

  rideBooking <- findRideBookingById ride.bookingId >>= fromMaybeM (RideBookingNotFound ride.bookingId.getId)
  logTagInfo "endRide" ("DriverId " <> getId requestorId <> ", RideId " <> getId rideId)

  now <- getCurrentTime
  putDiffs rideBooking ride

  let updRide =
        ride{tripEndTime = Just now
            }

  endRideTransaction rideBooking.id updRide (cast driverId)

  notifyCompleteToBAP rideBooking updRide rideBooking.fareParams

  return APISuccess.Success
  where
    putDiffs rideBooking ride = do
      let transporterId = rideBooking.providerId
          actualDistance = ride.traveledDistance
          oldDistance = rideBooking.estimatedDistance

      -- maybe compare only distance fare?
      let estimatedBaseFare = fareSum rideBooking.fareParams

      fareParams <- calculateFare transporterId actualDistance rideBooking.startTime rideBooking.fareParams.driverSelectedFare
      let updatedBaseFare = Fare.fareSum fareParams
      let distanceDiff = actualDistance - oldDistance
      let fareDiff = updatedBaseFare - estimatedBaseFare
      logTagInfo "Fare recalculation" $
        "Fare difference: "
          <> show (amountToDouble fareDiff)
          <> ", Distance difference: "
          <> show distanceDiff
      putDiffMetric fareDiff distanceDiff
