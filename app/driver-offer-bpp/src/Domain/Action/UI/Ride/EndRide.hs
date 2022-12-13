module Domain.Action.UI.Ride.EndRide
  ( ServiceHandle (..),
    DriverEndRideReq (..),
    DashboardEndRideReq (..),
    buildEndRideHandle,
    driverEndRide,
    dashboardEndRide,
  )
where

import Beckn.External.Maps
import Beckn.Prelude (roundToIntegral)
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Tools.Metrics.CoreMetrics
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.CalculateDistance (distanceBetweenInMeters)
import Beckn.Utils.Common
import qualified Domain.Action.UI.Ride.EndRide.Internal as RideEndInt
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.DriverLocation as DrLoc
import Domain.Types.FareParams as Fare
import Domain.Types.FarePolicy (FarePolicy)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.TransporterConfig as DTConf
import Domain.Types.Vehicle.Variant (Variant)
import Environment (Flow)
import EulerHS.Prelude hiding (pi)
import qualified Lib.LocationUpdates as LocUpd
import qualified SharedLogic.CallBAP as CallBAP
import qualified SharedLogic.FareCalculator as Fare
import qualified Storage.CachedQueries.FarePolicy as FarePolicyS
import qualified Storage.CachedQueries.TransporterConfig as QTConf
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverLocation as DrLoc
import qualified Storage.Queries.Ride as QRide
import Tools.Error

data EndRideReq = DriverReq DriverEndRideReq | DashboardReq DashboardEndRideReq

data DriverEndRideReq = DriverEndRideReq
  { point :: LatLong,
    requestor :: DP.Person
  }

data DashboardEndRideReq = DashboardEndRideReq
  { point :: Maybe LatLong,
    merchantId :: Id DM.Merchant
  }

data ServiceHandle m = ServiceHandle
  { findBookingById :: Id SRB.Booking -> m (Maybe SRB.Booking),
    findRideById :: Id DRide.Ride -> m (Maybe DRide.Ride),
    endRideTransaction :: Id DP.Driver -> Id SRB.Booking -> DRide.Ride -> m (),
    notifyCompleteToBAP :: SRB.Booking -> DRide.Ride -> Fare.FareParameters -> Money -> m (),
    getFarePolicy :: Id DM.Merchant -> Variant -> m (Maybe FarePolicy),
    calculateFare ::
      Id DM.Merchant ->
      FarePolicy ->
      Meters ->
      UTCTime ->
      Maybe Money ->
      m Fare.FareParameters,
    putDiffMetric :: Id DM.Merchant -> Money -> Meters -> m (),
    findDriverLoc :: Id DP.Person -> m (Maybe DrLoc.DriverLocation),
    isDistanceCalculationFailed :: Id DP.Person -> m Bool,
    finalDistanceCalculation :: Id DRide.Ride -> Id DP.Person -> LatLong -> m (),
    getDefaultPickupLocThreshold :: m Meters,
    getDefaultDropLocThreshold :: m Meters,
    getDefaultRideTravelledDistanceThreshold :: m Meters,
    getDefaultRideTimeEstimatedThreshold :: m Seconds,
    findConfig :: m (Maybe DTConf.TransporterConfig)
  }

buildEndRideHandle :: Id DM.Merchant -> Flow (ServiceHandle Flow)
buildEndRideHandle merchantId = do
  defaultRideInterpolationHandler <- LocUpd.buildRideInterpolationHandler merchantId True
  return $
    ServiceHandle
      { findBookingById = QRB.findById,
        findRideById = QRide.findById,
        getFarePolicy = FarePolicyS.findByMerchantIdAndVariant,
        notifyCompleteToBAP = CallBAP.sendRideCompletedUpdateToBAP,
        endRideTransaction = RideEndInt.endRideTransaction,
        calculateFare = Fare.calculateFare,
        putDiffMetric = RideEndInt.putDiffMetric,
        findDriverLoc = DrLoc.findById,
        isDistanceCalculationFailed = LocUpd.isDistanceCalculationFailed defaultRideInterpolationHandler,
        finalDistanceCalculation = LocUpd.finalDistanceCalculation defaultRideInterpolationHandler,
        getDefaultPickupLocThreshold = asks (.defaultPickupLocThreshold),
        getDefaultDropLocThreshold = asks (.defaultDropLocThreshold),
        getDefaultRideTravelledDistanceThreshold = asks (.defaultRideTravelledDistanceThreshold),
        getDefaultRideTimeEstimatedThreshold = asks (.defaultRideTimeEstimatedThreshold),
        findConfig = QTConf.findByMerchantId merchantId
      }

driverEndRide ::
  (Redis.HedisFlow m r, CoreMetrics m, MonadFlow m) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  DriverEndRideReq ->
  m APISuccess.APISuccess
driverEndRide handle rideId = endRideHandler handle rideId . DriverReq

dashboardEndRide ::
  (Redis.HedisFlow m r, CoreMetrics m, MonadFlow m) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  DashboardEndRideReq ->
  m APISuccess.APISuccess
dashboardEndRide handle rideId = endRideHandler handle rideId . DashboardReq

--TODO make it the same as in beckn transport
endRideHandler ::
  (Redis.HedisFlow m r, CoreMetrics m, MonadFlow m) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  EndRideReq ->
  m APISuccess.APISuccess
endRideHandler ServiceHandle {..} rideId req = do
  rideOld <- findRideById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  let driverId = rideOld.driverId
  booking <- findBookingById rideOld.bookingId >>= fromMaybeM (BookingNotFound rideOld.bookingId.getId)

  case req of
    DriverReq driverReq -> do
      let requestor = driverReq.requestor
      case requestor.role of
        DP.DRIVER -> unless (requestor.id == driverId) $ throwError NotAnExecutor
        _ -> throwError AccessDenied
    DashboardReq dashboardReq -> do
      unless (booking.providerId == dashboardReq.merchantId) $ throwError (RideDoesNotExist rideOld.id.getId)

  unless (rideOld.status == DRide.INPROGRESS) $ throwError $ RideInvalidStatus "This ride cannot be ended"
  LocUpd.whenWithLocationUpdatesLock driverId $ do
    point <- case req of
      DriverReq driverReq -> do
        logTagInfo "driver -> endRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId rideOld.id)
        pure driverReq.point
      DashboardReq dashboardReq -> do
        logTagInfo "dashboard -> endRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId rideOld.id)
        case dashboardReq.point of
          Just point -> pure point
          Nothing -> do
            driverLocation <- findDriverLoc driverId >>= fromMaybeM LocationNotFound
            pure $ getCoordinates driverLocation

    -- here we update the current ride, so below we fetch the updated version
    finalDistanceCalculation rideOld.id driverId point
    ride <- findRideById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)

    now <- getCurrentTime

    distanceCalculationFailed <- isDistanceCalculationFailed driverId
    when distanceCalculationFailed $ logWarning $ "Failed to calculate distance for this ride: " <> ride.id.getId

    let mbTripStartLoc = ride.tripStartPos
    -- for old trips with mbTripStartLoc = Nothing we always recalculate fare
    pickupDropOutsideOfThreshold <- case mbTripStartLoc of
      Nothing -> pure True
      Just tripStartLoc -> do
        mbThresholdConfig <- findConfig
        defaultPickupLocThreshold <- getDefaultPickupLocThreshold
        defaultDropLocThreshold <- getDefaultDropLocThreshold
        let pickupLocThreshold = metersToHighPrecMeters . fromMaybe defaultPickupLocThreshold . join $ mbThresholdConfig <&> (.pickupLocThreshold)
        let dropLocThreshold = metersToHighPrecMeters . fromMaybe defaultDropLocThreshold . join $ mbThresholdConfig <&> (.dropLocThreshold)
        let pickupDifference = distanceBetweenInMeters (getCoordinates booking.fromLocation) tripStartLoc
        let dropDifference = distanceBetweenInMeters (getCoordinates booking.toLocation) point
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
      mbThresholdConfig <- findConfig
      defaultRideTravelledDistanceThreshold <- getDefaultRideTravelledDistanceThreshold
      let rideTravelledDistanceThreshold = metersToHighPrecMeters . fromMaybe defaultRideTravelledDistanceThreshold . join $ mbThresholdConfig <&> (.rideTravelledDistanceThreshold)
      let rideDistanceDifference = ride.traveledDistance - metersToHighPrecMeters booking.estimatedDistance
      let distanceOutsideOfThreshold = rideDistanceDifference >= rideTravelledDistanceThreshold
      logTagInfo "endRide" ("distanceOutsideOfThreshold: " <> show distanceOutsideOfThreshold)
      logTagInfo "RideDistance differences" $
        "Distance Difference: "
          <> show rideDistanceDifference
          <> ", rideTravelledDistanceThreshold: "
          <> show rideTravelledDistanceThreshold
      pure distanceOutsideOfThreshold

    timeOutsideOfThreshold <- do
      mbThresholdConfig <- findConfig
      defaultRideTimeEstimatedThreshold <- getDefaultRideTimeEstimatedThreshold
      let rideTimeEstimatedThreshold = fromMaybe defaultRideTimeEstimatedThreshold . join $ mbThresholdConfig <&> (.rideTimeEstimatedThreshold)
      let tripStartTime = fromJust ride.tripStartTime
      let estimatedRideDuration = booking.estimatedDuration
      let actualRideDuration = nominalDiffTimeToSeconds $ tripStartTime `diffUTCTime` now
      let rideTimeDifference = actualRideDuration - estimatedRideDuration
      let timeOutsideOfThreshold = (rideTimeDifference >= rideTimeEstimatedThreshold) && (ride.traveledDistance > metersToHighPrecMeters booking.estimatedDistance)
      logTagInfo "endRide" ("timeOutsideOfThreshold: " <> show timeOutsideOfThreshold)
      logTagInfo "RideTime differences" $
        "Time Difference: "
          <> show rideTimeDifference
          <> ", estimatedRideDuration: "
          <> show estimatedRideDuration
          <> ", actualRideDuration: "
          <> show actualRideDuration
      pure timeOutsideOfThreshold

    (chargeableDistance, finalFare) <-
      if not distanceCalculationFailed && (pickupDropOutsideOfThreshold || distanceOutsideOfThreshold || timeOutsideOfThreshold)
        then recalculateFare booking ride
        else pure (booking.estimatedDistance, booking.estimatedFare)

    let updRide =
          ride{tripEndTime = Just now,
               chargeableDistance = Just chargeableDistance,
               fare = Just finalFare,
               tripEndPos = Just point
              }
    endRideTransaction (cast @DP.Person @DP.Driver driverId) booking.id updRide

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
