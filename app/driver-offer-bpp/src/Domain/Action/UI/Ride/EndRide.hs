module Domain.Action.UI.Ride.EndRide
  ( EndRideReq (..),
    ServiceHandle (..),
    buildEndRideHandle,
    endRideHandler,
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
import Data.OpenApi
import qualified Domain.Action.UI.Ride.EndRide.Internal as RideEndInt
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.DriverLocation as DrLoc
import Domain.Types.FareParams as Fare
import Domain.Types.FarePolicy (FarePolicy)
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
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
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import Tools.Error

newtype EndRideReq = EndRideReq
  { point :: LatLong
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data ServiceHandle m = ServiceHandle
  { requestor :: Person.Person,
    findBookingById :: Id SRB.Booking -> m (Maybe SRB.Booking),
    findRideById :: Id Ride.Ride -> m (Maybe Ride.Ride),
    endRideTransaction :: Id SRB.Booking -> Ride.Ride -> m (),
    notifyCompleteToBAP :: SRB.Booking -> Ride.Ride -> Fare.FareParameters -> Money -> m (),
    getFarePolicy :: Id Merchant -> Variant -> m (Maybe FarePolicy),
    calculateFare ::
      Id Merchant ->
      FarePolicy ->
      Meters ->
      UTCTime ->
      Maybe Money ->
      m Fare.FareParameters,
    putDiffMetric :: Id Merchant -> Money -> Meters -> m (),
    findDriverLoc :: m (Maybe DrLoc.DriverLocation),
    isDistanceCalculationFailed :: m Bool,
    finalDistanceCalculation :: LatLong -> m (),
    getDefaultPickupLocThreshold :: m Meters,
    getDefaultDropLocThreshold :: m Meters,
    getDefaultRideTravelledDistanceThreshold :: m Meters,
    getDefaultRideTimeEstimatedThreshold :: m Seconds,
    findConfig :: m (Maybe DTConf.TransporterConfig)
  }

buildEndRideHandle ::
  Id Person.Person ->
  Id Ride.Ride ->
  Flow (ServiceHandle Flow)
buildEndRideHandle requestorId rideId = do
  requestor <-
    QP.findById requestorId
      >>= fromMaybeM (PersonNotFound requestorId.getId)
  orgId <- requestor.merchantId & fromMaybeM (PersonFieldNotPresent "merchantId")
  defaultRideInterpolationHandler <- LocUpd.buildRideInterpolationHandler orgId
  return $
    ServiceHandle
      { requestor,
        findBookingById = QRB.findById,
        findRideById = QRide.findById,
        getFarePolicy = FarePolicyS.findByMerchantIdAndVariant,
        notifyCompleteToBAP = CallBAP.sendRideCompletedUpdateToBAP,
        endRideTransaction = RideEndInt.endRideTransaction (cast requestorId),
        calculateFare = Fare.calculateFare,
        putDiffMetric = RideEndInt.putDiffMetric,
        findDriverLoc = DrLoc.findById requestorId,
        isDistanceCalculationFailed = LocUpd.isDistanceCalculationFailed defaultRideInterpolationHandler requestorId,
        finalDistanceCalculation = LocUpd.finalDistanceCalculation defaultRideInterpolationHandler rideId requestorId,
        getDefaultPickupLocThreshold = asks (.defaultPickupLocThreshold),
        getDefaultDropLocThreshold = asks (.defaultDropLocThreshold),
        getDefaultRideTravelledDistanceThreshold = asks (.defaultRideTravelledDistanceThreshold),
        getDefaultRideTimeEstimatedThreshold = asks (.defaultRideTimeEstimatedThreshold),
        findConfig = QTConf.findByMerchantId orgId
      }

endRideHandler ::
  (MonadThrow m, Log m, MonadTime m, Redis.HedisFlow m r, CoreMetrics m, MonadFlow m, MonadTime m) =>
  ServiceHandle m ->
  Id Ride.Ride ->
  EndRideReq ->
  m APISuccess.APISuccess
endRideHandler ServiceHandle {..} rideId req = do
  rideOld <- findRideById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  let driverId = rideOld.driverId
  case requestor.role of
    Person.DRIVER -> unless (requestor.id == driverId) $ throwError NotAnExecutor
    _ -> throwError AccessDenied
  unless (rideOld.status == Ride.INPROGRESS) $ throwError $ RideInvalidStatus "This ride cannot be ended"
  booking <- findBookingById rideOld.bookingId >>= fromMaybeM (BookingNotFound rideOld.bookingId.getId)
  redisLockDriverId <- Redis.tryLockRedis (lockKey driverId) 60
  if redisLockDriverId
    then do
      logDebug $ "DriverId: " <> show driverId <> " Locked"
      finalDistanceCalculation req.point
      -- here we update the current ride, so below we fetch the updated version

      ride <- findRideById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)

      logTagInfo "endRide" ("DriverId " <> getId requestor.id <> ", RideId " <> getId rideId)

      now <- getCurrentTime

      distanceCalculationFailed <- isDistanceCalculationFailed
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
                 tripEndPos = Just req.point
                }
      endRideTransaction booking.id updRide

      notifyCompleteToBAP booking updRide booking.fareParams booking.estimatedFare
      Redis.unlockRedis (lockKey driverId)
      logDebug $ "DriverId: " <> show driverId <> " Unlocked"
    else logDebug $ "DriverId: " <> getId driverId <> " unable to get lock"

  return APISuccess.Success
  where
    lockKey driverId = LocUpd.makeLockKey driverId
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