module Domain.Action.UI.Ride.EndRide
  ( ServiceHandle (..),
    DriverEndRideReq (..),
    DashboardEndRideReq (..),
    buildEndRideHandle,
    driverEndRide,
    dashboardEndRide,
  )
where

import Beckn.External.Maps.HasCoordinates
import Beckn.External.Maps.Types
import Beckn.Prelude (roundToIntegral)
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.CalculateDistance (distanceBetweenInMeters)
import Beckn.Utils.Common
import qualified Domain.Action.UI.Ride.EndRide.Internal as EInternal
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.DriverLocation as DrLoc
import qualified Domain.Types.FarePolicy.FareBreakup as DFareBreakup
import qualified Domain.Types.FarePolicy.RentalFarePolicy as DRentalFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.TransporterConfig as DTConf
import qualified Domain.Types.Vehicle as Vehicle
import Environment (Flow)
import EulerHS.Prelude
import qualified Lib.LocationUpdates as LocUpd
import SharedLogic.CallBAP (sendRideCompletedUpdateToBAP)
import qualified SharedLogic.DriverLocation as DrLoc
import qualified SharedLogic.FareCalculator.OneWayFareCalculator.Flow as Fare
import qualified SharedLogic.FareCalculator.RentalFareCalculator.Flow as RentalFare
import qualified Storage.CachedQueries.FarePolicy.RentalFarePolicy as QRentalFP
import qualified Storage.CachedQueries.TransporterConfig as QTConf
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Tools.Error

data ServiceHandle m = ServiceHandle
  { fetchRide :: m DRide.Ride,
    findBookingById :: Id SRB.Booking -> m (Maybe SRB.Booking),
    endRideTransaction :: Id DP.Driver -> Id SRB.Booking -> DRide.Ride -> [DFareBreakup.FareBreakup] -> m (),
    notifyCompleteToBAP :: SRB.Booking -> DRide.Ride -> [DFareBreakup.FareBreakup] -> m (),
    calculateFare ::
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
    getRentalFarePolicy :: Id DRentalFP.RentalFarePolicy -> m DRentalFP.RentalFarePolicy,
    buildOneWayFareBreakups :: Fare.OneWayFareParameters -> Id SRB.Booking -> m [DFareBreakup.FareBreakup],
    buildRentalFareBreakups :: RentalFare.RentalFareParameters -> Id SRB.Booking -> m [DFareBreakup.FareBreakup],
    recalculateFareEnabled :: m Bool,
    putDiffMetric :: Money -> Meters -> m (),
    findDriverLoc :: Id DP.Person -> m (Maybe DrLoc.DriverLocation),
    finalDistanceCalculation :: Id DRide.Ride -> Id DP.Person -> LatLong -> m (),
    isDistanceCalculationFailed :: Id DP.Person -> m Bool,
    getDefaultPickupLocThreshold :: m Meters,
    getDefaultDropLocThreshold :: m Meters,
    getDefaultRideTravelledDistanceThreshold :: m Meters,
    getDefaultRideTimeEstimatedThreshold :: m Seconds,
    findConfigByKey :: DTConf.ConfigKey -> m (Maybe DTConf.TransporterConfig),
    whenWithLocationUpdatesLock :: Id DP.Person -> m () -> m ()
  }

data EndRideReq = DriverReq DriverEndRideReq | DashboardReq DashboardEndRideReq

data DriverEndRideReq = DriverEndRideReq
  { point :: LatLong,
    requestor :: DP.Person
  }

data DashboardEndRideReq = DashboardEndRideReq
  { point :: Maybe LatLong,
    merchantId :: Id DM.Merchant
  }

buildEndRideHandle :: Id DM.Merchant -> Id DRide.Ride -> Flow (ServiceHandle Flow)
buildEndRideHandle merchantId rideId = do
  defaultRideInterpolationHandler <- LocUpd.buildRideInterpolationHandler merchantId True
  return $
    ServiceHandle
      { fetchRide = QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId),
        findBookingById = QRB.findById,
        notifyCompleteToBAP = sendRideCompletedUpdateToBAP,
        endRideTransaction = EInternal.endRideTransaction,
        calculateFare = Fare.calculateFare merchantId,
        calculateRentalFare = RentalFare.calculateRentalFare,
        getRentalFarePolicy = QRentalFP.findById >=> fromMaybeM NoRentalFarePolicy,
        buildRentalFareBreakups = RentalFare.buildRentalFareBreakups,
        buildOneWayFareBreakups = Fare.buildOneWayFareBreakups,
        recalculateFareEnabled = asks (.recalculateFareEnabled),
        putDiffMetric = EInternal.putDiffMetric merchantId,
        findDriverLoc = DrLoc.findById,
        finalDistanceCalculation = LocUpd.finalDistanceCalculation defaultRideInterpolationHandler,
        isDistanceCalculationFailed = LocUpd.isDistanceCalculationFailed defaultRideInterpolationHandler,
        getDefaultPickupLocThreshold = asks (.defaultPickupLocThreshold),
        getDefaultDropLocThreshold = asks (.defaultDropLocThreshold),
        getDefaultRideTravelledDistanceThreshold = asks (.defaultRideTravelledDistanceThreshold),
        getDefaultRideTimeEstimatedThreshold = asks (.defaultRideTimeEstimatedThreshold),
        findConfigByKey = QTConf.findValueByMerchantIdAndKey merchantId,
        whenWithLocationUpdatesLock = LocUpd.whenWithLocationUpdatesLock
      }

driverEndRide ::
  (MonadThrow m, Log m, MonadTime m) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  DriverEndRideReq ->
  m APISuccess.APISuccess
driverEndRide handle rideId = endRide handle rideId . DriverReq

dashboardEndRide ::
  (MonadThrow m, Log m, MonadTime m) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  DashboardEndRideReq ->
  m APISuccess.APISuccess
dashboardEndRide handle rideId = endRide handle rideId . DashboardReq

endRide ::
  (MonadThrow m, Log m, MonadTime m) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  EndRideReq ->
  m APISuccess.APISuccess
endRide handle@ServiceHandle {..} rideId req = do
  rideOld <- fetchRide
  let driverId = rideOld.driverId
  booking <- findBookingById rideOld.bookingId >>= fromMaybeM (BookingNotFound rideOld.bookingId.getId)

  case req of
    DriverReq driverReq -> do
      let requestor = driverReq.requestor
      case requestor.role of
        DP.DRIVER -> unless (requestor.id == driverId) $ throwError NotAnExecutor
        _ -> throwError AccessDenied
    DashboardReq dashboardReq -> do
      unless (booking.providerId == dashboardReq.merchantId) $ throwError (RideDoesNotExist rideId.getId)

  unless (rideOld.status == DRide.INPROGRESS) $ throwError $ RideInvalidStatus "This ride cannot be ended"

  point <- case req of
    DriverReq driverReq -> do
      logTagInfo "driver -> endRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId rideId)
      pure driverReq.point
    DashboardReq dashboardReq -> do
      logTagInfo "dashboard -> endRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId rideId)
      case dashboardReq.point of
        Just point -> pure point
        Nothing -> do
          driverLocation <- findDriverLoc driverId >>= fromMaybeM LocationNotFound
          pure $ getCoordinates driverLocation

  whenWithLocationUpdatesLock driverId $ do
    -- here we update the current ride, so below we fetch the updated version
    finalDistanceCalculation rideId driverId point
    ride <- fetchRide

    now <- getCurrentTime
    (chargeableDistance, fare, totalFare, fareBreakups) <- do
      case booking.bookingDetails of
        SRB.OneWayDetails oneWayDetails -> recalculateFare booking oneWayDetails now ride point driverId
        SRB.RentalDetails rentalDetails -> calcRentalFare booking rentalDetails now ride driverId
    let updRide =
          ride{chargeableDistance = Just chargeableDistance,
               fare = Just fare,
               totalFare = Just totalFare,
               tripEndTime = Just now,
               tripEndPos = Just point
              }

    endRideTransaction (cast @DP.Person @DP.Driver driverId) booking.id updRide fareBreakups

    notifyCompleteToBAP booking updRide fareBreakups
  return APISuccess.Success
  where
    recalculateFare booking oneWayDetails now ride point driverId = do
      let vehicleVariant = booking.vehicleVariant

          actualDistance = roundToIntegral ride.traveledDistance.getHighPrecMeters
          oldDistance = oneWayDetails.estimatedDistance

          estimatedFare = booking.estimatedFare
      shouldRecalculateFare <- recalculateFareEnabled
      distanceCalculationFailed <- isDistanceCalculationFailed driverId

      let mbTripStartLoc = ride.tripStartPos
      -- for old trips with mbTripStartLoc = Nothing we always recalculate fare
      pickupDropOutsideOfThreshold <- case mbTripStartLoc of
        Nothing -> pure True
        Just tripStartLoc -> do
          pickupLocThreshold <- metersToHighPrecMeters <$> getLocThreshold handle PICKUP
          dropLocThreshold <- metersToHighPrecMeters <$> getLocThreshold handle DROP
          let pickupDifference = distanceBetweenInMeters (getCoordinates booking.fromLocation) tripStartLoc
          let dropDifference = distanceBetweenInMeters (getCoordinates oneWayDetails.toLocation) point
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
        rideTravelledDistanceThreshold <- metersToHighPrecMeters <$> getRideDistanceThreshold handle
        let rideDistanceDifference = ride.traveledDistance - metersToHighPrecMeters oneWayDetails.estimatedDistance
        let distanceOutsideOfThreshold = rideDistanceDifference >= rideTravelledDistanceThreshold
        logTagInfo "endRide" ("distanceOutsideOfThreshold: " <> show distanceOutsideOfThreshold)
        logTagInfo "RideDistance differences" $
          "Distance Difference: "
            <> show rideDistanceDifference
            <> ", rideTravelledDistanceThreshold: "
            <> show rideTravelledDistanceThreshold
        pure distanceOutsideOfThreshold

      timeOutsideOfThreshold <- do
        rideTimeEstimatedThreshold <- getRideTimeThreshold handle
        let tripStartTime = fromJust ride.tripStartTime
        let actualRideDuration = nominalDiffTimeToSeconds $ tripStartTime `diffUTCTime` now
        let estimatedRideDuration = oneWayDetails.estimatedDuration
        let rideTimeDifference = actualRideDuration - estimatedRideDuration
        let timeOutsideOfThreshold = (rideTimeDifference >= rideTimeEstimatedThreshold) && (ride.traveledDistance > metersToHighPrecMeters oneWayDetails.estimatedDistance)
        logTagInfo "endRide" ("timeOutsideOfThreshold: " <> " " <> show timeOutsideOfThreshold)
        logTagInfo "RideTime differences" $
          "Time Difference: "
            <> show rideTimeDifference
            <> ", estimatedRideDuration: "
            <> show estimatedRideDuration
            <> ", actualRideDuration: "
            <> show actualRideDuration
        pure timeOutsideOfThreshold

      if shouldRecalculateFare && not distanceCalculationFailed && (pickupDropOutsideOfThreshold || distanceOutsideOfThreshold || timeOutsideOfThreshold)
        then do
          fareParams <- calculateFare vehicleVariant actualDistance oneWayDetails.estimatedFinishTime
          let updatedFare = Fare.fareSum fareParams
              totalFare = Fare.fareSumWithDiscount fareParams
          let distanceDiff = actualDistance - oldDistance
          let fareDiff = updatedFare - estimatedFare
          logTagInfo "Fare recalculation" $
            "Fare difference: "
              <> show fareDiff
              <> ", Distance difference: "
              <> show distanceDiff
          putDiffMetric fareDiff distanceDiff
          fareBreakups <- buildOneWayFareBreakups fareParams booking.id
          return (actualDistance, updatedFare, totalFare, fareBreakups)
        else do
          when distanceCalculationFailed $
            logWarning "Failed to calculate actual distance for this ride, using estimated distance"
          -- calculate fare again with old data for creating fare breakup
          fareParams <- calculateFare vehicleVariant oldDistance booking.startTime
          fareBreakups <- buildOneWayFareBreakups fareParams booking.id
          pure (oldDistance, estimatedFare, booking.estimatedTotalFare, fareBreakups)

    calcRentalFare booking rentalDetails now ride driverId = do
      distanceCalculationFailed <- isDistanceCalculationFailed driverId
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
  Stop ->
  m Meters
getLocThreshold ServiceHandle {..} stop = do
  (paramName, defaultThreshold) <- case stop of
    PICKUP -> (DTConf.ConfigKey "pickup_loc_threshold",) <$> getDefaultPickupLocThreshold
    DROP -> (DTConf.ConfigKey "drop_loc_threshold",) <$> getDefaultDropLocThreshold
  mbThresholdConfig <- findConfigByKey paramName
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
  m Meters
getRideDistanceThreshold ServiceHandle {..} = do
  (paramName, defaultThreshold) <- (DTConf.ConfigKey "ride_travelled_distance",) <$> getDefaultRideTravelledDistanceThreshold
  mbThresholdConfig <- findConfigByKey paramName
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
  m Seconds
getRideTimeThreshold ServiceHandle {..} = do
  (paramName, defaultThreshold) <- (DTConf.ConfigKey "ride_estimated_time",) <$> getDefaultRideTimeEstimatedThreshold
  mbThresholdConfig <- findConfigByKey paramName
  mbThreshold <- thresholdFromConfigSeconds paramName `mapM` mbThresholdConfig
  pure $ fromMaybe defaultThreshold mbThreshold
  where
    thresholdFromConfigSeconds paramName conf =
      fromMaybeM (InternalError $ show paramName <> " is not a number.")
        . (Seconds <$>)
        . (readMaybe @Int)
        . toString
        $ conf.value