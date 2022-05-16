module Domain.Action.UI.Ride.EndRide where

import Beckn.Prelude (ToSchema)
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.MapSearch
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.DriverLocation as DrLoc
import qualified Domain.Types.FarePolicy.FareBreakup as DFareBreakup
import qualified Domain.Types.FarePolicy.RentalFarePolicy as DRentalFP
import Domain.Types.Organization (Organization)
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.Vehicle as Vehicle
import EulerHS.Prelude hiding (pi)
import qualified SharedLogic.FareCalculator.OneWayFareCalculator as Fare
import qualified SharedLogic.FareCalculator.RentalFareCalculator as RentalFare
import Types.App (Driver)
import Types.Error
import Utils.Common

data ServiceHandle m = ServiceHandle
  { findById :: Id Person.Person -> m (Maybe Person.Person),
    findBookingById :: Id SRB.Booking -> m (Maybe SRB.Booking),
    findRideById :: Id Ride.Ride -> m (Maybe Ride.Ride),
    endRideTransaction :: Id SRB.Booking -> Ride.Ride -> Id Driver -> [DFareBreakup.FareBreakup] -> m (),
    notifyCompleteToBAP :: SRB.Booking -> Ride.Ride -> [DFareBreakup.FareBreakup] -> m (),
    calculateFare ::
      Id Organization ->
      Vehicle.Variant ->
      HighPrecMeters ->
      UTCTime ->
      m Fare.OneWayFareParameters,
    calculateRentalFare ::
      Id DRentalFP.RentalFarePolicy ->
      HighPrecMeters ->
      UTCTime ->
      UTCTime ->
      m RentalFare.RentalFareParameters,
    buildOneWayFareBreakups :: Fare.OneWayFareParameters -> Id SRB.Booking -> m [DFareBreakup.FareBreakup],
    buildRentalFareBreakups :: RentalFare.RentalFareParameters -> Id SRB.Booking -> m [DFareBreakup.FareBreakup],
    recalculateFareEnabled :: m Bool,
    putDiffMetric :: Amount -> HighPrecMeters -> m (),
    findDriverLocById :: Id Person.Person -> m (Maybe DrLoc.DriverLocation),
    addLastWaypointAndRecalcDistanceOnEnd :: Id Person.Person -> LatLong -> m ()
  }

newtype EndRideReq = EndRideReq
  { point :: LatLong
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

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
  (chargeableDistance, fare, totalFare, fareBreakups) <- do
    case booking.bookingDetails of
      SRB.OneWayDetails oneWayDetails -> recalculateFare booking ride oneWayDetails
      SRB.RentalDetails rentalDetails -> calcRentalFare booking ride rentalDetails now

  let updRide =
        ride{chargeableDistance = Just chargeableDistance,
             fare = Just fare,
             totalFare = Just totalFare,
             tripEndTime = Just now
            }

  endRideTransaction booking.id updRide (cast driverId) fareBreakups

  notifyCompleteToBAP booking updRide fareBreakups

  return APISuccess.Success
  where
    recalculateFare booking ride oneWayDetails = do
      let transporterId = booking.providerId
          vehicleVariant = booking.vehicleVariant

          actualDistance = ride.traveledDistance
          oldDistance = oneWayDetails.estimatedDistance

          estimatedFare = booking.estimatedFare
      shouldRecalculateFare <- recalculateFareEnabled
      if shouldRecalculateFare
        then do
          fareParams <- calculateFare transporterId vehicleVariant actualDistance booking.startTime
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
          fareBreakups <- buildOneWayFareBreakups fareParams booking.id
          pure (actualDistance, updatedFare, totalFare, fareBreakups)
        else do
          -- calculate fare again with old data for creating fare breakup
          fareParams <- calculateFare transporterId vehicleVariant oldDistance booking.startTime
          fareBreakups <- buildOneWayFareBreakups fareParams booking.id
          pure (oldDistance, estimatedFare, booking.estimatedTotalFare, fareBreakups)

    calcRentalFare booking ride rentalDetails now = do
      let actualDistance = ride.traveledDistance
      fareParams <- calculateRentalFare rentalDetails.rentalFarePolicyId actualDistance booking.startTime now
      let fare = RentalFare.rentalFareSum fareParams
          totalFare = RentalFare.rentalFareSumWithDiscount fareParams
      logTagInfo "Rental fare calculation" $
        "Base fare: "
          <> show (amountToDouble fareParams.baseFare)
          <> ", Extra distance fare: "
          <> show (amountToDouble fareParams.extraDistanceFare)
          <> ", Extra time fare: "
          <> show (amountToDouble fareParams.extraTimeFare)
          <> ", Next days fare: "
          <> show (amountToDouble (fromMaybe 0 fareParams.nextDaysFare))
          <> ", Discount: "
          <> show (amountToDouble (fromMaybe 0 fareParams.discount))
      -- Do we ned this metrics in rental case?
      -- putDiffMetric fareDiff distanceDiff
      fareBreakups <- buildRentalFareBreakups fareParams booking.id
      pure (actualDistance, fare, totalFare, fareBreakups)
