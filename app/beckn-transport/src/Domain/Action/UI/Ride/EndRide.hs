module Domain.Action.UI.Ride.EndRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import Data.Time (NominalDiffTime)
import qualified Domain.Types.DriverLocation as DrLoc
import qualified Domain.Types.FareBreakup as DFareBreakup
import Domain.Types.Organization (Organization)
import qualified Domain.Types.Person as Person
import qualified Domain.Types.RentalFarePolicy as DRentalFP
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.RideBooking as SRB
import qualified Domain.Types.Vehicle as Vehicle
import EulerHS.Prelude hiding (pi)
import qualified Product.FareCalculator as Fare
import qualified Product.RentalFareCalculator as RentalFare
import Types.App (Driver)
import Types.Error
import Utils.Common

data ServiceHandle m = ServiceHandle
  { findById :: Id Person.Person -> m (Maybe Person.Person),
    findRideBookingById :: Id SRB.RideBooking -> m (Maybe SRB.RideBooking),
    findRideById :: Id Ride.Ride -> m (Maybe Ride.Ride),
    endRideTransaction :: Id SRB.RideBooking -> Ride.Ride -> Id Driver -> [DFareBreakup.FareBreakup] -> m (),
    notifyCompleteToBAP :: SRB.RideBooking -> Ride.Ride -> [DFareBreakup.FareBreakup] -> m (),
    calculateFare ::
      Id Organization ->
      Vehicle.Variant ->
      HighPrecMeters ->
      UTCTime ->
      m Fare.FareParameters,
    calculateRentalFare ::
      Id DRentalFP.RentalFarePolicy ->
      HighPrecMeters ->
      UTCTime ->
      UTCTime ->
      m RentalFare.RentalFareParameters,
    buildFareBreakups :: Fare.FareParameters -> Id SRB.RideBooking -> m [DFareBreakup.FareBreakup],
    buildRentalFareBreakups :: RentalFare.RentalFareParameters -> Id SRB.RideBooking -> m [DFareBreakup.FareBreakup],
    recalculateFareEnabled :: m Bool,
    putDiffMetric :: Amount -> HighPrecMeters -> m (),
    findDriverLocById :: Id Person.Person -> m (Maybe DrLoc.DriverLocation),
    isMarketAsMissingLocationUpdates :: Id Ride.Ride -> m Bool,
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
  requestor <- findById requestorId >>= fromMaybeM (PersonNotFound requestorId.getId)

  recalcDistanceEnding requestorId

  ride <- findRideById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  let driverId = ride.driverId
  case requestor.role of
    Person.DRIVER -> unless (requestorId == driverId) $ throwError NotAnExecutor
    _ -> throwError AccessDenied
  unless (ride.status == Ride.INPROGRESS) $ throwError $ RideInvalidStatus "This ride cannot be ended"

  rideBooking <- findRideBookingById ride.bookingId >>= fromMaybeM (RideBookingNotFound ride.bookingId.getId)
  logTagInfo "endRide" ("DriverId " <> getId requestorId <> ", RideId " <> getId rideId)

  now <- getCurrentTime
  (chargeableDistance, fare, totalFare, fareBreakups) <- do
    case rideBooking.rideBookingDetails of
      SRB.OneWayDetails oneWayDetails -> recalculateFare rideBooking ride oneWayDetails now
      SRB.RentalDetails rentalDetails -> calcRentalFare rideBooking ride rentalDetails now

  let updRide =
        ride{chargeableDistance = Just chargeableDistance,
             fare = Just fare,
             totalFare = Just totalFare,
             tripEndTime = Just now
            }

  endRideTransaction rideBooking.id updRide (cast driverId) fareBreakups

  notifyCompleteToBAP rideBooking updRide fareBreakups

  return APISuccess.Success
  where
    lastLocUpdateTooLongAgo now = do
      allowedUpdatesDelay <- updateLocationAllowedDelay
      res <-
        findDriverLocById requestorId
          <&> maybe True (\loc -> now `diffUTCTime` loc.updatedAt > allowedUpdatesDelay)
      logDebug $ "last update was too long ago: " <> show res
      pure res

    thereWereMissingLocUpdates = do
      res <- isMarketAsMissingLocationUpdates rideId
      logDebug $ "there were missing location updates: " <> show res
      pure res

    recalculateFare rideBooking ride oneWayDetails now = do
      let transporterId = rideBooking.providerId
          vehicleVariant = rideBooking.vehicleVariant

          actualDistance = ride.traveledDistance
          oldDistance = oneWayDetails.estimatedDistance

          estimatedFare = rideBooking.estimatedFare
      shouldRecalculateFare <- recalculateFareEnabled
      missingLocationUpdates <-
        (||)
          <$> lastLocUpdateTooLongAgo now
          <*> thereWereMissingLocUpdates
      if shouldRecalculateFare && not missingLocationUpdates
        then do
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
          fareBreakups <- buildFareBreakups fareParams rideBooking.id
          pure (actualDistance, updatedFare, totalFare, fareBreakups)
        else do
          -- calculate fare again with old data for creating fare breakup
          fareParams <- calculateFare transporterId vehicleVariant oldDistance rideBooking.startTime
          fareBreakups <- buildFareBreakups fareParams rideBooking.id
          pure (oldDistance, estimatedFare, rideBooking.estimatedTotalFare, fareBreakups)

    calcRentalFare rideBooking ride rentalDetails now = do
      let actualDistance = ride.traveledDistance
      fareParams <- calculateRentalFare rentalDetails.rentalFarePolicyId actualDistance rideBooking.startTime now
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
      fareBreakups <- buildRentalFareBreakups fareParams rideBooking.id
      pure (actualDistance, fare, totalFare, fareBreakups)
