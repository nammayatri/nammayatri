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
    CallBasedEndRideReq (..),
    DashboardEndRideReq (..),
    callBasedEndRide,
    buildEndRideHandle,
    driverEndRide,
    dashboardEndRide,
  )
where

import qualified Domain.Action.UI.Ride.EndRide.Internal as RideEndInt
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.DriverLocation as DrLoc
import Domain.Types.FareParameters as Fare
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FareProduct as DFareProduct
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Merchant.TransporterConfig as DTConf
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderDetails as RD
import qualified Domain.Types.Vehicle as DVeh
import Environment (Flow)
import EulerHS.Prelude hiding (id, pi)
import Kernel.External.Maps
import Kernel.Prelude (roundToIntegral)
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Kernel.Utils.DatastoreLatencyCalculator
import qualified Lib.LocationUpdates as LocUpd
import qualified SharedLogic.CallBAP as CallBAP
import qualified SharedLogic.DriverLocation as DrLoc
import qualified SharedLogic.FareCalculator as Fare
import qualified SharedLogic.FarePolicy as FarePolicy
import qualified Storage.CachedQueries.Merchant as MerchantS
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.TransporterConfig as QTConf
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.SMS as Sms

data EndRideReq = DriverReq DriverEndRideReq | DashboardReq DashboardEndRideReq | CallBasedReq CallBasedEndRideReq

data DriverEndRideReq = DriverEndRideReq
  { point :: LatLong,
    requestor :: DP.Person
  }

data DashboardEndRideReq = DashboardEndRideReq
  { point :: Maybe LatLong,
    merchantId :: Id DM.Merchant
  }

newtype CallBasedEndRideReq = CallBasedEndRideReq
  { requestor :: DP.Person
  }

data ServiceHandle m = ServiceHandle
  { findBookingById :: Id SRB.Booking -> m (Maybe SRB.Booking),
    findRideById :: Id DRide.Ride -> m (Maybe DRide.Ride),
    getMerchant :: Id DM.Merchant -> m (Maybe DM.Merchant),
    endRideTransaction :: Id DP.Driver -> SRB.Booking -> DRide.Ride -> Maybe FareParameters -> Maybe (Id RD.RiderDetails) -> FareParameters -> DTConf.TransporterConfig -> Id DM.Merchant -> m (),
    notifyCompleteToBAP :: SRB.Booking -> DRide.Ride -> Fare.FareParameters -> Maybe DMPM.PaymentMethodInfo -> Maybe Text -> m (),
    getFarePolicy :: Id DM.Merchant -> DVeh.Variant -> Maybe DFareProduct.Area -> m DFP.FullFarePolicy,
    calculateFareParameters :: Fare.CalculateFareParametersParams -> m Fare.FareParameters,
    putDiffMetric :: Id DM.Merchant -> Money -> Meters -> m (),
    findDriverLoc :: Id DM.Merchant -> Id DP.Person -> m (Maybe DrLoc.DriverLocation),
    isDistanceCalculationFailed :: Id DP.Person -> m Bool,
    finalDistanceCalculation :: Id DRide.Ride -> Id DP.Person -> LatLong -> Meters -> Bool -> m (),
    getInterpolatedPoints :: Id DP.Person -> m [LatLong],
    clearInterpolatedPoints :: Id DP.Person -> m (),
    findConfig :: m (Maybe DTConf.TransporterConfig),
    whenWithLocationUpdatesLock :: Id DP.Person -> m () -> m (),
    getDistanceBetweenPoints :: LatLong -> LatLong -> [LatLong] -> m Meters,
    findPaymentMethodByIdAndMerchantId :: Id DMPM.MerchantPaymentMethod -> Id DM.Merchant -> m (Maybe DMPM.MerchantPaymentMethod),
    sendDashboardSms :: Id DM.Merchant -> Sms.DashboardMessageType -> Maybe DRide.Ride -> Id DP.Person -> Maybe SRB.Booking -> HighPrecMoney -> m ()
  }

buildEndRideHandle :: Id DM.Merchant -> Flow (ServiceHandle Flow)
buildEndRideHandle merchantId = do
  defaultRideInterpolationHandler <- LocUpd.buildRideInterpolationHandler merchantId True
  return $
    ServiceHandle
      { findBookingById = QRB.findById,
        findRideById = QRide.findById,
        getMerchant = MerchantS.findById,
        notifyCompleteToBAP = CallBAP.sendRideCompletedUpdateToBAP,
        endRideTransaction = RideEndInt.endRideTransaction,
        getFarePolicy = FarePolicy.getFarePolicy,
        calculateFareParameters = Fare.calculateFareParameters,
        putDiffMetric = RideEndInt.putDiffMetric,
        findDriverLoc = DrLoc.findById,
        isDistanceCalculationFailed = LocUpd.isDistanceCalculationFailed defaultRideInterpolationHandler,
        finalDistanceCalculation = LocUpd.finalDistanceCalculation defaultRideInterpolationHandler,
        getInterpolatedPoints = LocUpd.getInterpolatedPoints defaultRideInterpolationHandler,
        clearInterpolatedPoints = LocUpd.clearInterpolatedPoints defaultRideInterpolationHandler,
        findConfig = QTConf.findByMerchantId merchantId,
        whenWithLocationUpdatesLock = LocUpd.whenWithLocationUpdatesLock,
        getDistanceBetweenPoints = RideEndInt.getDistanceBetweenPoints merchantId,
        findPaymentMethodByIdAndMerchantId = CQMPM.findByIdAndMerchantId,
        sendDashboardSms = Sms.sendDashboardSms
      }

type EndRideFlow m r = (MonadFlow m, CoreMetrics m, MonadReader r m, HasField "enableAPILatencyLogging" r Bool, HasField "enableAPIPrometheusMetricLogging" r Bool)

driverEndRide ::
  EndRideFlow m r =>
  ServiceHandle m ->
  Id DRide.Ride ->
  DriverEndRideReq ->
  m APISuccess.APISuccess
driverEndRide handle rideId req = do
  withLogTag ("requestorId-" <> req.requestor.id.getId)
    . endRide handle rideId
    $ DriverReq req

callBasedEndRide ::
  EndRideFlow m r =>
  ServiceHandle m ->
  Id DRide.Ride ->
  CallBasedEndRideReq ->
  m APISuccess.APISuccess
callBasedEndRide handle rideId = endRide handle rideId . CallBasedReq

dashboardEndRide ::
  EndRideFlow m r =>
  ServiceHandle m ->
  Id DRide.Ride ->
  DashboardEndRideReq ->
  m APISuccess.APISuccess
dashboardEndRide handle rideId req =
  withLogTag ("merchantId-" <> req.merchantId.getId)
    . endRide handle rideId
    $ DashboardReq req

endRide ::
  EndRideFlow m r =>
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
    CallBasedReq callBasedEndRideReq -> do
      let requestor = callBasedEndRideReq.requestor
      case requestor.role of
        DP.DRIVER -> unless (requestor.id == driverId) $ throwError NotAnExecutor
        _ -> throwError AccessDenied

  unless (rideOld.status == DRide.INPROGRESS) $ throwError $ RideInvalidStatus "This ride cannot be ended"

  tripEndPoint <- case req of
    DriverReq driverReq -> do
      logTagInfo "driver -> endRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId rideOld.id)
      pure driverReq.point
    DashboardReq dashboardReq -> do
      logTagInfo "dashboard -> endRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId rideOld.id)
      case dashboardReq.point of
        Just point -> pure point
        Nothing -> do
          pure $ getCoordinates booking.toLocation
    CallBasedReq _ -> do
      pure $ getCoordinates booking.toLocation

  whenWithLocationUpdatesLock driverId $ do
    -- here we update the current ride, so below we fetch the updated version
    thresholdConfig <- findConfig >>= fromMaybeM (InternalError "TransportConfigNotFound")
    pickupDropOutsideOfThreshold <- isPickupDropOutsideOfThreshold booking rideOld tripEndPoint thresholdConfig
    withTimeAPI "endRide" "finalDistanceCalculation" $ finalDistanceCalculation rideOld.id driverId tripEndPoint booking.estimatedDistance pickupDropOutsideOfThreshold

    ride <- findRideById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)

    now <- getCurrentTime

    distanceCalculationFailed <- withTimeAPI "endRide" "isDistanceCalculationFailed" $ isDistanceCalculationFailed driverId
    when distanceCalculationFailed $ logWarning $ "Failed to calculate distance for this ride: " <> ride.id.getId

    (chargeableDistance, finalFare, mbUpdatedFareParams) <-
      if distanceCalculationFailed
        then calculateFinalValuesForFailedDistanceCalculations handle booking ride tripEndPoint pickupDropOutsideOfThreshold thresholdConfig
        else calculateFinalValuesForCorrectDistanceCalculations handle booking ride booking.maxEstimatedDistance pickupDropOutsideOfThreshold thresholdConfig
    let newFareParams = fromMaybe booking.fareParams mbUpdatedFareParams
    let updRide =
          ride{tripEndTime = Just now,
               chargeableDistance = Just chargeableDistance,
               fare = Just finalFare,
               tripEndPos = Just tripEndPoint,
               fareParametersId = Just newFareParams.id,
               distanceCalculationFailed = Just distanceCalculationFailed,
               pickupDropOutsideOfThreshold = Just pickupDropOutsideOfThreshold
              }
    -- we need to store fareParams only when they changed
    withTimeAPI "endRide" "endRideTransaction" $ endRideTransaction (cast @DP.Person @DP.Driver driverId) booking updRide mbUpdatedFareParams booking.riderId newFareParams thresholdConfig booking.providerId
    withTimeAPI "endRide" "clearInterpolatedPoints" $ clearInterpolatedPoints driverId

    mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId -> do
      findPaymentMethodByIdAndMerchantId paymentMethodId booking.providerId
        >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
    let mbPaymentUrl = DMPM.getPostpaidPaymentUrl =<< mbPaymentMethod
    let mbPaymentMethodInfo = DMPM.mkPaymentMethodInfo <$> mbPaymentMethod
    withTimeAPI "endRide" "notifyCompleteToBAP" $ notifyCompleteToBAP booking updRide newFareParams mbPaymentMethodInfo mbPaymentUrl

    fork "sending dashboardSMS - CallbasedEndRide " $ do
      case req of
        CallBasedReq callBasedEndRideReq -> do
          let requestor = callBasedEndRideReq.requestor
          sendDashboardSms requestor.merchantId Sms.ENDRIDE (Just ride) driverId (Just booking) (fromIntegral finalFare)
        _ -> pure ()

  return APISuccess.Success

recalculateFareForDistance :: (MonadThrow m, Log m, MonadTime m, MonadGuid m) => ServiceHandle m -> SRB.Booking -> DRide.Ride -> Meters -> m (Meters, Money, Maybe FareParameters)
recalculateFareForDistance ServiceHandle {..} booking ride recalcDistance = do
  let merchantId = booking.providerId
      oldDistance = booking.estimatedDistance

  -- maybe compare only distance fare?
  let estimatedFare = Fare.fareSum booking.fareParams
  farePolicy <- getFarePolicy merchantId booking.vehicleVariant booking.area
  fareParams <-
    calculateFareParameters
      Fare.CalculateFareParametersParams
        { farePolicy = farePolicy,
          distance = recalcDistance,
          rideTime = booking.startTime,
          waitingTime = secondsToMinutes . roundToIntegral <$> (diffUTCTime <$> ride.tripStartTime <*> ride.driverArrivalTime),
          driverSelectedFare = booking.fareParams.driverSelectedFare,
          customerExtraFee = booking.fareParams.customerExtraFee,
          nightShiftCharge = booking.fareParams.nightShiftCharge
        }
  let finalFare = Fare.fareSum fareParams
      distanceDiff = recalcDistance - oldDistance
      fareDiff = finalFare - estimatedFare
  logTagInfo "Fare recalculation" $
    "Fare difference: "
      <> show (realToFrac @_ @Double fareDiff)
      <> ", Distance difference: "
      <> show distanceDiff
  putDiffMetric merchantId fareDiff distanceDiff
  return (recalcDistance, finalFare, Just fareParams)

isPickupDropOutsideOfThreshold :: (MonadThrow m, Log m, MonadTime m, MonadGuid m) => SRB.Booking -> DRide.Ride -> LatLong -> DTConf.TransporterConfig -> m Bool
isPickupDropOutsideOfThreshold booking ride tripEndPoint thresholdConfig = do
  let mbTripStartLoc = ride.tripStartPos
  -- for old trips with mbTripStartLoc = Nothing we always recalculate fare
  case mbTripStartLoc of
    Nothing -> pure True
    Just tripStartLoc -> do
      let pickupLocThreshold = metersToHighPrecMeters thresholdConfig.pickupLocThreshold
      let dropLocThreshold = metersToHighPrecMeters thresholdConfig.dropLocThreshold
      let pickupDifference = abs $ distanceBetweenInMeters (getCoordinates booking.fromLocation) tripStartLoc
      let dropDifference = abs $ distanceBetweenInMeters (getCoordinates booking.toLocation) tripEndPoint
      let pickupDropOutsideOfThreshold = (pickupDifference >= pickupLocThreshold) || (dropDifference >= dropLocThreshold)

      logTagInfo "Locations differences" $
        "Pickup difference: "
          <> show pickupDifference
          <> ", Drop difference: "
          <> show dropDifference
          <> ", Locations outside of thresholds: "
          <> show pickupDropOutsideOfThreshold
      pure pickupDropOutsideOfThreshold

getDistanceDiff :: (MonadThrow m, Log m, MonadTime m, MonadGuid m) => SRB.Booking -> Meters -> m HighPrecMeters
getDistanceDiff booking distance = do
  let rideDistanceDifference = distance - booking.estimatedDistance
  logTagInfo "RideDistance differences" $
    "Distance Difference: "
      <> show rideDistanceDifference
  pure $ metersToHighPrecMeters rideDistanceDifference

calculateFinalValuesForCorrectDistanceCalculations ::
  (MonadThrow m, Log m, MonadTime m, MonadGuid m) => ServiceHandle m -> SRB.Booking -> DRide.Ride -> Maybe HighPrecMeters -> Bool -> DTConf.TransporterConfig -> m (Meters, Money, Maybe FareParameters)
calculateFinalValuesForCorrectDistanceCalculations handle booking ride mbMaxDistance pickupDropOutsideOfThreshold thresholdConfig = do
  distanceDiff <- getDistanceDiff booking (highPrecMetersToMeters ride.traveledDistance)
  let maxDistance = fromMaybe ride.traveledDistance mbMaxDistance + thresholdConfig.actualRideDistanceDiffThreshold
  if not pickupDropOutsideOfThreshold
    then
      if distanceDiff > thresholdConfig.actualRideDistanceDiffThreshold
        then recalculateFareForDistance handle booking ride (roundToIntegral $ min ride.traveledDistance maxDistance)
        else recalculateFareForDistance handle booking ride booking.estimatedDistance
    else
      if distanceDiff < 0
        then recalculateFareForDistance handle booking ride $ roundToIntegral ride.traveledDistance
        else
          if distanceDiff < thresholdConfig.actualRideDistanceDiffThreshold
            then recalculateFareForDistance handle booking ride booking.estimatedDistance
            else recalculateFareForDistance handle booking ride (roundToIntegral ride.traveledDistance)

calculateFinalValuesForFailedDistanceCalculations ::
  (MonadThrow m, Log m, MonadTime m, MonadGuid m) => ServiceHandle m -> SRB.Booking -> DRide.Ride -> LatLong -> Bool -> DTConf.TransporterConfig -> m (Meters, Money, Maybe FareParameters)
calculateFinalValuesForFailedDistanceCalculations handle@ServiceHandle {..} booking ride tripEndPoint pickupDropOutsideOfThreshold thresholdConfig = do
  let tripStartPoint = case ride.tripStartPos of
        Nothing -> getCoordinates booking.fromLocation
        Just tripStartPos -> tripStartPos
  interpolatedPoints <- getInterpolatedPoints ride.driverId
  approxTraveledDistance <- getDistanceBetweenPoints tripStartPoint tripEndPoint interpolatedPoints
  logTagInfo "endRide" $ "approxTraveledDistance: " <> show approxTraveledDistance
  distanceDiff <- getDistanceDiff booking approxTraveledDistance

  if not pickupDropOutsideOfThreshold
    then recalculateFareForDistance handle booking ride booking.estimatedDistance
    else
      if distanceDiff < 0
        then recalculateFareForDistance handle booking ride approxTraveledDistance
        else
          if distanceDiff < thresholdConfig.actualRideDistanceDiffThreshold
            then do
              recalculateFareForDistance handle booking ride booking.estimatedDistance
            else do
              if distanceDiff < thresholdConfig.upwardsRecomputeBuffer
                then recalculateFareForDistance handle booking ride approxTraveledDistance
                else do
                  logTagInfo "Inaccurate Location Updates and Pickup/Drop Deviated." ("DistanceDiff: " <> show distanceDiff)
                  recalculateFareForDistance handle booking ride (booking.estimatedDistance + highPrecMetersToMeters thresholdConfig.upwardsRecomputeBuffer)
