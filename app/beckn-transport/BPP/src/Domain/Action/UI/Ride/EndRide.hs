module Domain.Action.UI.Ride.EndRide where

import Beckn.Prelude (ToSchema, roundToIntegral)
import Beckn.Product.MapSearch.GoogleMaps (HasCoordinates (getCoordinates))
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.MapSearch
import Beckn.Utils.CalculateDistance (distanceBetweenInMeters)
import Beckn.Utils.Common
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.DriverLocation as DrLoc
import qualified Domain.Types.FarePolicy.FareBreakup as DFareBreakup
import qualified Domain.Types.FarePolicy.RentalFarePolicy as DRentalFP
import Domain.Types.Organization (Organization)
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.TransporterConfig as DTConf
import qualified Domain.Types.Vehicle as Vehicle
import EulerHS.Prelude
import qualified SharedLogic.FareCalculator.OneWayFareCalculator.Flow as Fare
import qualified SharedLogic.FareCalculator.RentalFareCalculator.Flow as RentalFare
import Tools.Error

data ServiceHandle m = ServiceHandle
  { findById :: Id Person.Person -> m (Maybe Person.Person),
    findBookingById :: Id SRB.Booking -> m (Maybe SRB.Booking),
    findRideById :: Id Ride.Ride -> m (Maybe Ride.Ride),
    endRideTransaction :: Id SRB.Booking -> Ride.Ride -> Id Person.Driver -> [DFareBreakup.FareBreakup] -> m (),
    notifyCompleteToBAP :: SRB.Booking -> Ride.Ride -> [DFareBreakup.FareBreakup] -> m (),
    calculateFare ::
      Id Organization ->
      Vehicle.Variant ->
      Meters ->
      UTCTime ->
      m Fare.OneWayFareParameters,
    calculateRentalFare ::
      Id DRentalFP.RentalFarePolicy ->
      Meters ->
      UTCTime ->
      UTCTime ->
      m RentalFare.RentalFareParameters,
    getRentalFarePolicy ::
      Id DRentalFP.RentalFarePolicy ->
      m DRentalFP.RentalFarePolicy,
    buildOneWayFareBreakups :: Fare.OneWayFareParameters -> Id SRB.Booking -> m [DFareBreakup.FareBreakup],
    buildRentalFareBreakups :: RentalFare.RentalFareParameters -> Id SRB.Booking -> m [DFareBreakup.FareBreakup],
    recalculateFareEnabled :: m Bool,
    putDiffMetric :: Id Organization -> Money -> Meters -> m (),
    findDriverLocById :: Id Person.Person -> m (Maybe DrLoc.DriverLocation),
    finalDistanceCalculation :: Id Person.Person -> LatLong -> m (),
    isDistanceCalculationFailed :: Id Person.Person -> m Bool,
    getDefaultPickupLocThreshold :: m Meters,
    getDefaultDropLocThreshold :: m Meters,
    getDefaultRideTravelledDistanceThreshold :: m Meters,
    getDefaultRideTimeEstimatedThreshold :: m Seconds,
    findConfigByOrgIdAndKey :: Id Organization -> DTConf.ConfigKey -> m (Maybe DTConf.TransporterConfig)
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
endRideHandler handle@ServiceHandle {..} requestorId rideId req = do
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

  (chargeableDistance, fare, totalFare, fareBreakups) <- do
    case booking.bookingDetails of
      SRB.OneWayDetails oneWayDetails -> recalculateFare booking ride oneWayDetails now
      SRB.RentalDetails rentalDetails -> calcRentalFare booking ride rentalDetails now

  let updRide =
        ride{chargeableDistance = Just chargeableDistance,
             fare = Just fare,
             totalFare = Just totalFare,
             tripEndTime = Just now,
             tripEndPos = Just req.point
            }

  endRideTransaction booking.id updRide (cast driverId) fareBreakups

  notifyCompleteToBAP booking updRide fareBreakups

  return APISuccess.Success
  where
    recalculateFare booking ride oneWayDetails now = do
      let transporterId = booking.providerId
          vehicleVariant = booking.vehicleVariant

          actualDistance = roundToIntegral ride.traveledDistance.getHighPrecMeters
          oldDistance = oneWayDetails.estimatedDistance

          estimatedFare = booking.estimatedFare
      shouldRecalculateFare <- recalculateFareEnabled
      distanceCalculationFailed <- isDistanceCalculationFailed requestorId

      let mbTripStartLoc = ride.tripStartPos
      -- for old trips with mbTripStartLoc = Nothing we always recalculate fare
      pickupDropOutsideOfThreshold <- case mbTripStartLoc of
        Nothing -> pure True
        Just tripStartLoc -> do
          pickupLocThreshold <- metersToHighPrecMeters <$> getLocThreshold handle transporterId PICKUP
          dropLocThreshold <- metersToHighPrecMeters <$> getLocThreshold handle transporterId DROP
          let pickupDifference = distanceBetweenInMeters (getCoordinates booking.fromLocation) tripStartLoc
          let dropDifference = distanceBetweenInMeters (getCoordinates oneWayDetails.toLocation) req.point
          let pickupDropOutsideOfThreshold = (pickupDifference >= pickupLocThreshold) || (dropDifference >= dropLocThreshold)

          logTagInfo "Locations differences" $
            "Pickup difference: "
              <> show pickupDifference
              <> ", Drop difference: "
              <> show dropDifference
              <> ", Locations outside of thresholds: "
              <> show pickupDropOutsideOfThreshold
          pure pickupDropOutsideOfThreshold

      distanceOutsideOfThreshold <- do
        rideTravelledDistanceThreshold <- metersToHighPrecMeters <$> getRideDistanceThreshold handle transporterId
        let rideDistanceDifference = ride.traveledDistance - metersToHighPrecMeters oneWayDetails.estimatedDistance
        let distanceOutsideOfThreshold = rideDistanceDifference >= rideTravelledDistanceThreshold
        logTagInfo "endRide" ("distanceOutsideOfThreshold" <> show distanceOutsideOfThreshold)
        logTagInfo "RideDistance differences" $
          "Distance Difference: "
            <> show rideDistanceDifference
            <> "rideTravelledDistanceThreshold "
            <> show rideTravelledDistanceThreshold
        pure distanceOutsideOfThreshold

      timeOutsideOfThreshold <- do
        rideTimeEstimatedThreshold <- getRideTimeThreshold handle transporterId
        let tripStartTime = fromJust ride.tripStartTime
        let actualRideDuration = nominalDiffTimeToSeconds $ tripStartTime `diffUTCTime` now
        let estimatedRideDuration = oneWayDetails.estimatedDuration
        let rideTimeDifference = estimatedRideDuration - actualRideDuration
        let timeOutsideOfThreshold = (rideTimeDifference >= rideTimeEstimatedThreshold) && (ride.traveledDistance > metersToHighPrecMeters oneWayDetails.estimatedDistance)
        logTagInfo "RideTime differences" $
          "Distance Difference: "
            <> show rideTimeDifference
            <> "estimatedRideDuration "
            <> show estimatedRideDuration
            <> "actualRideDuration"
            <> show actualRideDuration
        pure timeOutsideOfThreshold

      if shouldRecalculateFare && (not distanceCalculationFailed && pickupDropOutsideOfThreshold)
        || shouldRecalculateFare && (not distanceCalculationFailed && distanceOutsideOfThreshold)
        || shouldRecalculateFare && (not distanceCalculationFailed && timeOutsideOfThreshold)
        then do
          fareParams <- calculateFare transporterId vehicleVariant actualDistance oneWayDetails.estimatedFinishTime
          let updatedFare = Fare.fareSum fareParams
              totalFare = Fare.fareSumWithDiscount fareParams
          let distanceDiff = actualDistance - oldDistance
          let fareDiff = updatedFare - estimatedFare
          logTagInfo "Fare recalculation" $
            "Fare difference: "
              <> show fareDiff
              <> ", Distance difference: "
              <> show distanceDiff
          putDiffMetric transporterId fareDiff distanceDiff
          fareBreakups <- buildOneWayFareBreakups fareParams booking.id
          return (actualDistance, updatedFare, totalFare, fareBreakups)
        else do
          when distanceCalculationFailed $
            logWarning "Failed to calculate actual distance for this ride, using estimated distance"
          -- calculate fare again with old data for creating fare breakup
          fareParams <- calculateFare transporterId vehicleVariant oldDistance booking.startTime
          fareBreakups <- buildOneWayFareBreakups fareParams booking.id
          pure (oldDistance, estimatedFare, booking.estimatedTotalFare, fareBreakups)

    calcRentalFare booking ride rentalDetails now = do
      distanceCalculationFailed <- isDistanceCalculationFailed requestorId
      actualDistance <-
        if not distanceCalculationFailed
          then pure $ roundToIntegral ride.traveledDistance.getHighPrecMeters
          else do
            logWarning "Failed to calculate distance for this ride, using booking distance"
            rentalFPBaseDistance <- (.baseDistance) <$> getRentalFarePolicy rentalDetails.rentalFarePolicyId
            pure $ kilometersToMeters rentalFPBaseDistance

      fareParams <- calculateRentalFare rentalDetails.rentalFarePolicyId actualDistance booking.startTime now
      let fare = RentalFare.rentalFareSum fareParams
          totalFare = RentalFare.rentalFareSumWithDiscount fareParams
      logTagInfo "Rental fare calculation" $
        "Base fare: "
          <> show fareParams.baseFare
          <> ", Extra distance fare: "
          <> show fareParams.extraDistanceFare
          <> ", Extra time fare: "
          <> show fareParams.extraTimeFare
          <> ", Next days fare: "
          <> show (fromMaybe 0 fareParams.nextDaysFare)
          <> ", Discount: "
          <> show (fromMaybe 0 fareParams.discount)
      -- Do we need this metrics in rental case?
      -- putDiffMetric fareDiff distanceDiff
      fareBreakups <- buildRentalFareBreakups fareParams booking.id
      pure (actualDistance, fare, totalFare, fareBreakups)

data Stop = PICKUP | DROP

getLocThreshold ::
  (MonadThrow m, Log m) =>
  ServiceHandle m ->
  Id DOrg.Organization ->
  Stop ->
  m Meters
getLocThreshold ServiceHandle {..} orgId stop = do
  (paramName, defaultThreshold) <- case stop of
    PICKUP -> (DTConf.ConfigKey "pickup_loc_threshold",) <$> getDefaultPickupLocThreshold
    DROP -> (DTConf.ConfigKey "drop_loc_threshold",) <$> getDefaultDropLocThreshold
  mbThresholdConfig <- findConfigByOrgIdAndKey orgId paramName
  mbThreshold <- thresholdFromConfig paramName `mapM` mbThresholdConfig
  pure $ fromMaybe defaultThreshold mbThreshold
  where
    -- TODO derive instance Read Meters
    thresholdFromConfig paramName conf =
      fromMaybeM (InternalError $ show paramName <> " is not a number.")
        . (Meters <$>)
        . (readMaybe @Int)
        . toString
        $ conf.value

getRideDistanceThreshold ::
  (MonadThrow m, Log m) =>
  ServiceHandle m ->
  Id DOrg.Organization ->
  m Meters
getRideDistanceThreshold ServiceHandle {..} orgId = do
  (paramName, defaultThreshold) <- (DTConf.ConfigKey "ride_travelled_distance",) <$> getDefaultRideTravelledDistanceThreshold
  mbThresholdConfig <- findConfigByOrgIdAndKey orgId paramName
  mbThreshold <- thresholdFromConfigMeter paramName `mapM` mbThresholdConfig
  pure $ fromMaybe defaultThreshold mbThreshold
  where
    -- TODO derive instance Read Meters
    thresholdFromConfigMeter paramName conf =
      fromMaybeM (InternalError $ show paramName <> " is not a number.")
        . (Meters <$>)
        . (readMaybe @Int)
        . toString
        $ conf.value

getRideTimeThreshold ::
  (MonadThrow m, Log m) =>
  ServiceHandle m ->
  Id DOrg.Organization ->
  m Seconds
getRideTimeThreshold ServiceHandle {..} orgId = do
  (paramName, defaultThreshold) <- (DTConf.ConfigKey "ride_estimated_time",) <$> getDefaultRideTimeEstimatedThreshold
  mbThresholdConfig <- findConfigByOrgIdAndKey orgId paramName
  mbThreshold <- thresholdFromConfigSeconds paramName `mapM` mbThresholdConfig
  pure $ fromMaybe defaultThreshold mbThreshold
  where
    thresholdFromConfigSeconds paramName conf =
      fromMaybeM (InternalError $ show paramName <> " is not a number.")
        . (Seconds <$>)
        . (readMaybe @Int)
        . toString
        $ conf.value
