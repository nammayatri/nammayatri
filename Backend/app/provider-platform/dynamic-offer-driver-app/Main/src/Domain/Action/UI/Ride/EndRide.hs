{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Ride.EndRide
  ( ServiceHandle (..),
    DriverEndRideReq (..),
    DashboardEndRideReq (..),
    buildEndRideHandle,
    driverEndRide,
    dashboardEndRide,
  )
where

import qualified Domain.Action.UI.Ride.EndRide.Internal as RideEndInt
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.DriverLocation as DrLoc
import Domain.Types.FareParameters as Fare
import Domain.Types.FarePolicy.FarePolicy (FarePolicy)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.TransporterConfig as DTConf
import Domain.Types.Vehicle.Variant (Variant)
import Environment (Flow)
import EulerHS.Prelude hiding (pi)
import Kernel.External.Maps
import Kernel.Prelude (roundToIntegral)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified Lib.LocationUpdates as LocUpd
import qualified SharedLogic.CallBAP as CallBAP
import qualified SharedLogic.DriverLocation as DrLoc
import qualified SharedLogic.FareCalculator as Fare
import qualified Storage.CachedQueries.FarePolicy.FarePolicy as FarePolicyS
import qualified Storage.CachedQueries.TransporterConfig as QTConf
import qualified Storage.Queries.Booking as QRB
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
    endRideTransaction :: Id DP.Driver -> Id SRB.Booking -> DRide.Ride -> Maybe FareParameters -> m (),
    notifyCompleteToBAP :: SRB.Booking -> DRide.Ride -> Fare.FareParameters -> m (),
    getFarePolicy :: Id DM.Merchant -> Variant -> Maybe Meters -> m (Maybe FarePolicy),
    calculateFare ::
      Id DM.Merchant ->
      FarePolicy ->
      Meters ->
      UTCTime ->
      Maybe Money ->
      Maybe Money ->
      m Fare.FareParameters,
    putDiffMetric :: Id DM.Merchant -> Money -> Meters -> m (),
    findDriverLoc :: Id DP.Person -> m (Maybe DrLoc.DriverLocation),
    isDistanceCalculationFailed :: Id DP.Person -> m Bool,
    finalDistanceCalculation :: Id DRide.Ride -> Id DP.Person -> LatLong -> m (),
    getDefaultPickupLocThreshold :: m Meters,
    getDefaultDropLocThreshold :: m Meters,
    getDefaultrideTravelledDistThresholdWhenPickupOrDestIsDiff :: m Meters,
    getDefaultrideTravelledDistThresholdWhenPickupAndDestIsSame :: m Meters,
    getDefaultRideTimeEstimatedThreshold :: m Seconds,
    getDefaultWaitingTimeEstimatedThreshold :: m Seconds,
    findConfig :: m (Maybe DTConf.TransporterConfig),
    whenWithLocationUpdatesLock :: Id DP.Person -> m () -> m ()
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
        getDefaultrideTravelledDistThresholdWhenPickupOrDestIsDiff = asks (.defaultrideTravelledDistThresholdWhenPickupOrDestIsDiff),
        getDefaultrideTravelledDistThresholdWhenPickupAndDestIsSame = asks (.defaultrideTravelledDistThresholdWhenPickupAndDestIsSame),
        getDefaultRideTimeEstimatedThreshold = asks (.defaultRideTimeEstimatedThreshold),
        getDefaultWaitingTimeEstimatedThreshold = asks (.defaultWaitingTimeEstimatedThreshold),
        findConfig = QTConf.findByMerchantId merchantId,
        whenWithLocationUpdatesLock = LocUpd.whenWithLocationUpdatesLock
      }

driverEndRide ::
  (MonadThrow m, Log m, MonadTime m, MonadGuid m) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  DriverEndRideReq ->
  m APISuccess.APISuccess
driverEndRide handle rideId req =
  withLogTag ("requestorId-" <> req.requestor.id.getId)
    . endRide handle rideId
    $ DriverReq req

dashboardEndRide ::
  (MonadThrow m, Log m, MonadTime m, MonadGuid m) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  DashboardEndRideReq ->
  m APISuccess.APISuccess
dashboardEndRide handle rideId req =
  withLogTag ("merchantId-" <> req.merchantId.getId)
    . endRide handle rideId
    $ DashboardReq req

endRide ::
  (MonadThrow m, Log m, MonadTime m, MonadGuid m) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  EndRideReq ->
  m APISuccess.APISuccess
endRide handle@ServiceHandle {..} rideId req = withLogTag ("rideId-" <> rideId.getId) do
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

  whenWithLocationUpdatesLock driverId $ do
    -- here we update the current ride, so below we fetch the updated version
    finalDistanceCalculation rideOld.id driverId point
    ride <- findRideById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)

    now <- getCurrentTime

    distanceCalculationFailed <- isDistanceCalculationFailed driverId
    when distanceCalculationFailed $ logWarning $ "Failed to calculate distance for this ride: " <> ride.id.getId

    let mbTripStartLoc = ride.tripStartPos
    mbThresholdConfig <- findConfig
    -- for old trips with mbTripStartLoc = Nothing we always recalculate fare
    pickupDropOutsideOfThreshold <- case mbTripStartLoc of
      Nothing -> pure True
      Just tripStartLoc -> do
        defaultPickupLocThreshold <- getDefaultPickupLocThreshold
        defaultDropLocThreshold <- getDefaultDropLocThreshold
        let pickupLocThreshold = metersToHighPrecMeters . fromMaybe defaultPickupLocThreshold . join $ mbThresholdConfig <&> (.pickupLocThreshold)
        let dropLocThreshold = metersToHighPrecMeters . fromMaybe defaultDropLocThreshold . join $ mbThresholdConfig <&> (.dropLocThreshold)
        let pickupDifference = abs $ distanceBetweenInMeters (getCoordinates booking.fromLocation) tripStartLoc
        let dropDifference = abs $ distanceBetweenInMeters (getCoordinates booking.toLocation) point
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
      rideTravelledDistanceThreshold <-
        if pickupDropOutsideOfThreshold
          then metersToHighPrecMeters <$> getRideDistanceThresholdWhenPickupOrDestIsDiff handle
          else metersToHighPrecMeters <$> getRideDistanceThresholdWhenPickupAndDestIsSame handle
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
      case ride.tripStartTime of
        Nothing -> pure False
        Just tripStartTime -> do
          defaultRideTimeEstimatedThreshold <- getDefaultRideTimeEstimatedThreshold
          let rideTimeEstimatedThreshold = fromMaybe defaultRideTimeEstimatedThreshold . join $ mbThresholdConfig <&> (.rideTimeEstimatedThreshold)
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

    (chargeableDistance, finalFare, mbUpdatedFareParams) <-
      case (distanceCalculationFailed, pickupDropOutsideOfThreshold, distanceOutsideOfThreshold || timeOutsideOfThreshold) of
        (False, False, True) -> recalculateFareForDistance booking ride (max (roundToIntegral ride.traveledDistance) booking.estimatedDistance)
        (False, True, True) -> recalculateFareForDistance booking ride (roundToIntegral ride.traveledDistance)
        _ -> do
          waitingCharge <- getWaitingFare ride.tripStartTime ride.driverArrivalTime booking.fareParams.waitingChargePerMin
          pure (booking.estimatedDistance, waitingCharge + booking.estimatedFare, Nothing)

    let newFareParams = fromMaybe booking.fareParams mbUpdatedFareParams
    let updRide =
          ride{tripEndTime = Just now,
               chargeableDistance = Just chargeableDistance,
               fare = Just finalFare,
               tripEndPos = Just point,
               fareParametersId = Just newFareParams.id
              }
    -- we need to store fareParams only when they changed
    endRideTransaction (cast @DP.Person @DP.Driver driverId) booking.id updRide mbUpdatedFareParams

    notifyCompleteToBAP booking updRide newFareParams
  return APISuccess.Success
  where
    recalculateFareForDistance booking ride recalcDistance = do
      let transporterId = booking.providerId
          oldDistance = booking.estimatedDistance

      -- maybe compare only distance fare?
      let estimatedFare = Fare.fareSum booking.fareParams
      farePolicy <- getFarePolicy transporterId booking.vehicleVariant (Just booking.estimatedDistance) >>= fromMaybeM NoFarePolicy
      fareParams <-
        calculateFare transporterId farePolicy recalcDistance booking.startTime booking.fareParams.driverSelectedFare
          booking.fareParams.customerExtraFee
      waitingCharge <- getWaitingFare ride.tripStartTime ride.driverArrivalTime farePolicy.waitingChargePerMin
      let updatedFare = Fare.fareSum fareParams
          finalFare = updatedFare + waitingCharge
          distanceDiff = recalcDistance - oldDistance
          fareDiff = finalFare - estimatedFare
      logTagInfo "Fare recalculation" $
        "Fare difference: "
          <> show (realToFrac @_ @Double fareDiff)
          <> ", Distance difference: "
          <> show distanceDiff
      putDiffMetric transporterId fareDiff distanceDiff
      return (recalcDistance, finalFare, Just fareParams)

    getWaitingFare mbTripStartTime mbDriverArrivalTime waitingChargePerMin = do
      mbThresholdConfig <- findConfig
      defaultThreshold <- getDefaultWaitingTimeEstimatedThreshold
      let waitingTimeThreshold = fromMaybe defaultThreshold . join $ mbThresholdConfig <&> (.waitingTimeEstimatedThreshold)
          driverWaitingTime = fromMaybe 0 (diffUTCTime <$> mbTripStartTime <*> mbDriverArrivalTime)
          fareableWaitingTime = max 0 (driverWaitingTime / 60 - fromIntegral waitingTimeThreshold)
      pure $ roundToIntegral fareableWaitingTime * fromMaybe 0 waitingChargePerMin

getRideDistanceThresholdWhenPickupOrDestIsDiff ::
  (MonadThrow m, Log m) =>
  ServiceHandle m ->
  m Meters
getRideDistanceThresholdWhenPickupOrDestIsDiff ServiceHandle {..} = do
  mbThresholdConfig <- findConfig
  defaultThreshold <- getDefaultrideTravelledDistThresholdWhenPickupOrDestIsDiff
  pure $ fromMaybe defaultThreshold . join $ mbThresholdConfig <&> (.rideTravelledDistThresholdWhenPickupOrDestIsDiff)

getRideDistanceThresholdWhenPickupAndDestIsSame ::
  (MonadThrow m, Log m) =>
  ServiceHandle m ->
  m Meters
getRideDistanceThresholdWhenPickupAndDestIsSame ServiceHandle {..} = do
  mbThresholdConfig <- findConfig
  defaultThreshold <- getDefaultrideTravelledDistThresholdWhenPickupAndDestIsSame
  pure $ fromMaybe defaultThreshold . join $ mbThresholdConfig <&> (.rideTravelledDistThresholdWhenPickupAndDestIsSame)
