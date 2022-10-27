module Domain.Action.UI.Ride.EndRide
  ( EndRideReq (..),
    ServiceHandle (..),
    endRideHandler,
  )
where

import Beckn.Prelude (roundToIntegral)
import Beckn.Product.MapSearch.GoogleMaps (HasCoordinates (getCoordinates))
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.MapSearch
import Beckn.Utils.CalculateDistance (distanceBetweenInMeters)
import Beckn.Utils.Common
import Data.OpenApi
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.DriverLocation as DrLoc
import Domain.Types.FareParams as Fare
import Domain.Types.FarePolicy (FarePolicy)
import Domain.Types.Organization (Organization)
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.TransporterConfig as DTConf
import Domain.Types.Vehicle.Variant (Variant)
import EulerHS.Prelude hiding (pi)
import qualified SharedLogic.FareCalculator as Fare
import Tools.Error

newtype EndRideReq = EndRideReq
  { point :: LatLong
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data ServiceHandle m = ServiceHandle
  { findById :: Id Person.Person -> m (Maybe Person.Person),
    findBookingById :: Id SRB.Booking -> m (Maybe SRB.Booking),
    findRideById :: Id Ride.Ride -> m (Maybe Ride.Ride),
    endRide :: Id SRB.Booking -> Ride.Ride -> Id Person.Driver -> m (),
    notifyCompleteToBAP :: SRB.Booking -> Ride.Ride -> Fare.FareParameters -> Money -> m (),
    getFarePolicy :: Id Organization -> Variant -> m (Maybe FarePolicy),
    calculateFare ::
      Id Organization ->
      FarePolicy ->
      Meters ->
      UTCTime ->
      Maybe Money ->
      m Fare.FareParameters,
    putDiffMetric :: Id Organization -> Money -> Meters -> m (),
    findDriverLocById :: Id Person.Person -> m (Maybe DrLoc.DriverLocation),
    isDistanceCalculationFailed :: Id Person.Person -> m Bool,
    finalDistanceCalculation :: Id Person.Person -> LatLong -> m (),
    getDefaultPickupLocThreshold :: m Meters,
    getDefaultDropLocThreshold :: m Meters,
    findConfigByOrgId :: Id Organization -> m (Maybe DTConf.TransporterConfig)
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

  finalDistanceCalculation requestorId req.point
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

  distanceCalculationFailed <- isDistanceCalculationFailed requestorId
  when distanceCalculationFailed $ logWarning $ "Failed to calculate distance for this ride: " <> ride.id.getId

  let mbTripStartLoc = ride.tripStartPos
  -- for old trips with mbTripStartLoc = Nothing we always recalculate fare
  pickupDropOutsideOfThreshold <- case mbTripStartLoc of
    Nothing -> pure True
    Just tripStartLoc -> do
      mbThresholdConfig <- findConfigByOrgId booking.providerId
      defaultPickupLocThreshold <- getDefaultPickupLocThreshold
      defaultDropLocThreshold <- getDefaultDropLocThreshold
      let pickupLocThreshold = metersToHighPrecMeters . fromMaybe defaultPickupLocThreshold . join $ mbThresholdConfig <&> (.pickupLocThreshold)
      let dropLocThreshold = metersToHighPrecMeters . fromMaybe defaultDropLocThreshold . join $ mbThresholdConfig <&> (.dropLocThreshold)
      let pickupDifference = distanceBetweenInMeters (getCoordinates booking.fromLocation) tripStartLoc
      let dropDifference = distanceBetweenInMeters (getCoordinates booking.toLocation) req.point
      let pickupDropOutsideOfThreshold = (pickupDifference >= pickupLocThreshold) || (dropDifference >= dropLocThreshold)
      logTagInfo "Locations differences" $
        "Pickup difference: "
          <> show pickupDifference
          <> ", Drop difference: "
          <> show dropDifference
          <> ", Locations outside of thresholds: "
          <> show pickupDropOutsideOfThreshold
      pure pickupDropOutsideOfThreshold

  (chargeableDistance, finalFare) <-
    if not distanceCalculationFailed && pickupDropOutsideOfThreshold
      then recalculateFare booking ride
      else pure (booking.estimatedDistance, booking.estimatedFare)

  let updRide =
        ride{tripEndTime = Just now,
             chargeableDistance = Just chargeableDistance,
             fare = Just finalFare,
             tripEndPos = Just req.point
            }

  endRide booking.id updRide (cast driverId)

  notifyCompleteToBAP booking updRide booking.fareParams booking.estimatedFare

  return APISuccess.Success
  where
    recalculateFare booking ride = do
      let transporterId = booking.providerId
          actualDistance = roundToIntegral ride.traveledDistance
          oldDistance = booking.estimatedDistance

      -- maybe compare only distance fare?
      let estimatedFare = Fare.fareSum booking.fareParams
      farePolicy <- getFarePolicy transporterId booking.vehicleVariant >>= fromMaybeM NoFarePolicy
      fareParams <- calculateFare transporterId farePolicy actualDistance booking.startTime booking.fareParams.driverSelectedFare
      let updatedFare = Fare.fareSum fareParams
      let distanceDiff = actualDistance - oldDistance
      let fareDiff = updatedFare - estimatedFare
      logTagInfo "Fare recalculation" $
        "Fare difference: "
          <> show (realToFrac @_ @Double fareDiff)
          <> ", Distance difference: "
          <> show distanceDiff
      putDiffMetric transporterId fareDiff distanceDiff
      return (actualDistance, updatedFare)
