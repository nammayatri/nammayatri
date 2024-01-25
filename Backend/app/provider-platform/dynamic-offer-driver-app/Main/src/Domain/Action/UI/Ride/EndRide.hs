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
    CronJobEndRideReq (..),
    EndRideResp (..),
    callBasedEndRide,
    buildEndRideHandle,
    driverEndRide,
    dashboardEndRide,
    cronJobEndRide,
  )
where

import Data.OpenApi.Internal.Schema (ToSchema)
import qualified Domain.Action.UI.Ride.EndRide.Internal as RideEndInt
import Domain.Action.UI.Route as DMaps
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as DDGR
import Domain.Types.FareParameters as Fare
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FareProduct as DFareProduct
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Merchant.TransporterConfig as DTConf
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderDetails as RD
import qualified Domain.Types.Vehicle as DVeh
import Environment (Flow)
import EulerHS.Prelude hiding (id, pi)
import Kernel.External.Maps
import qualified Kernel.External.Maps.Interface.Types as Maps
import qualified Kernel.External.Maps.Types as Maps
import Kernel.Prelude (roundToIntegral)
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Kernel.Utils.DatastoreLatencyCalculator
import qualified Lib.DriverCoins.Coins as DC
import qualified Lib.DriverCoins.Types as DCT
import qualified Lib.LocationUpdates as LocUpd
import qualified SharedLogic.CallBAP as CallBAP
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.FareCalculator as Fare
import qualified SharedLogic.FarePolicy as FarePolicy
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.GoHomeConfig as CQGHC
import qualified Storage.CachedQueries.Merchant as MerchantS
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.TransporterConfig as QTConf
import qualified Storage.Queries.Booking as QRB
import Storage.Queries.Driver.GoHomeFeature.DriverGoHomeRequest as QDGR
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.SMS as Sms

data EndRideReq = DriverReq DriverEndRideReq | DashboardReq DashboardEndRideReq | CallBasedReq CallBasedEndRideReq | CronJobReq CronJobEndRideReq

data EndRideResp = EndRideResp
  { result :: Text,
    homeLocationReached :: Maybe Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data DriverEndRideReq = DriverEndRideReq
  { endRideOtp :: Maybe Text,
    point :: LatLong,
    requestor :: DP.Person,
    uiDistanceCalculationWithAccuracy :: Maybe Int,
    uiDistanceCalculationWithoutAccuracy :: Maybe Int,
    odometer :: Maybe DRide.OdometerReading
  }

data DashboardEndRideReq = DashboardEndRideReq
  { point :: Maybe LatLong,
    merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    odometer :: Maybe DRide.OdometerReading
  }

data CronJobEndRideReq = CronJobEndRideReq
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
    endRideTransaction :: Id DP.Driver -> SRB.Booking -> DRide.Ride -> Maybe FareParameters -> Maybe (Id RD.RiderDetails) -> FareParameters -> DTConf.TransporterConfig -> m (),
    notifyCompleteToBAP :: SRB.Booking -> DRide.Ride -> Fare.FareParameters -> Maybe DMPM.PaymentMethodInfo -> Maybe Text -> Maybe LatLong -> m (),
    getFarePolicy :: Id DMOC.MerchantOperatingCity -> DTC.TripCategory -> DVeh.Variant -> Maybe DFareProduct.Area -> m DFP.FullFarePolicy,
    calculateFareParameters :: Fare.CalculateFareParametersParams -> m Fare.FareParameters,
    putDiffMetric :: Id DM.Merchant -> Money -> Meters -> m (),
    isDistanceCalculationFailed :: Id DP.Person -> m Bool,
    finalDistanceCalculation :: Id DRide.Ride -> Id DP.Person -> NonEmpty LatLong -> Meters -> Bool -> m (),
    getInterpolatedPoints :: Id DP.Person -> m [LatLong],
    clearInterpolatedPoints :: Id DP.Person -> m (),
    findConfig :: m (Maybe DTConf.TransporterConfig),
    whenWithLocationUpdatesLock :: Id DP.Person -> m () -> m (),
    getDistanceBetweenPoints :: LatLong -> LatLong -> [LatLong] -> m Meters,
    findPaymentMethodByIdAndMerchantId :: Id DMPM.MerchantPaymentMethod -> Id DMOC.MerchantOperatingCity -> m (Maybe DMPM.MerchantPaymentMethod),
    sendDashboardSms :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Sms.DashboardMessageType -> Maybe DRide.Ride -> Id DP.Person -> Maybe SRB.Booking -> HighPrecMoney -> m (),
    uiDistanceCalculation :: Id DRide.Ride -> Maybe Int -> Maybe Int -> m ()
  }

buildEndRideHandle :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow (ServiceHandle Flow)
buildEndRideHandle merchantId merchantOpCityId = do
  defaultRideInterpolationHandler <- LocUpd.buildRideInterpolationHandler merchantId merchantOpCityId True
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
        isDistanceCalculationFailed = LocUpd.isDistanceCalculationFailed defaultRideInterpolationHandler,
        finalDistanceCalculation = LocUpd.finalDistanceCalculation defaultRideInterpolationHandler,
        getInterpolatedPoints = LocUpd.getInterpolatedPoints defaultRideInterpolationHandler,
        clearInterpolatedPoints = LocUpd.clearInterpolatedPoints defaultRideInterpolationHandler,
        findConfig = QTConf.findByMerchantOpCityId merchantOpCityId,
        whenWithLocationUpdatesLock = LocUpd.whenWithLocationUpdatesLock,
        getDistanceBetweenPoints = RideEndInt.getDistanceBetweenPoints merchantId merchantOpCityId,
        findPaymentMethodByIdAndMerchantId = CQMPM.findByIdAndMerchantOpCityId,
        sendDashboardSms = Sms.sendDashboardSms,
        uiDistanceCalculation = QRide.updateUiDistanceCalculation
      }

type EndRideFlow m r =
  ( MonadFlow m,
    CoreMetrics m,
    MonadReader r m,
    HasField "enableAPILatencyLogging" r Bool,
    HasField "enableAPIPrometheusMetricLogging" r Bool,
    LT.HasLocationService m r,
    HasField
      "minTripDistanceForReferralCfg"
      r
      (Maybe HighPrecMeters)
  )

driverEndRide ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  DriverEndRideReq ->
  m EndRideResp
driverEndRide handle rideId req = do
  withLogTag ("requestorId-" <> req.requestor.id.getId)
    . endRide handle rideId
    $ DriverReq req

callBasedEndRide ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  CallBasedEndRideReq ->
  m EndRideResp
callBasedEndRide handle rideId = endRide handle rideId . CallBasedReq

dashboardEndRide ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  DashboardEndRideReq ->
  m APISuccess.APISuccess
dashboardEndRide handle rideId req = do
  void $
    withLogTag ("merchantId-" <> req.merchantId.getId)
      . endRide handle rideId
      $ DashboardReq req
  return APISuccess.Success

cronJobEndRide ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  CronJobEndRideReq ->
  m APISuccess.APISuccess
cronJobEndRide handle rideId req = do
  void $
    withLogTag ("merchantId-" <> req.merchantId.getId)
      . endRide handle rideId
      $ CronJobReq req
  return APISuccess.Success

endRide ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  EndRideReq ->
  m EndRideResp
endRide handle@ServiceHandle {..} rideId req = withLogTag ("rideId-" <> rideId.getId) do
  rideOld <- findRideById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  let driverId = rideOld.driverId
  booking <- findBookingById rideOld.bookingId >>= fromMaybeM (BookingNotFound rideOld.bookingId.getId)
  case req of
    DriverReq driverReq -> do
      let requestor = driverReq.requestor
      when (DTC.isEndOtpRequired booking.tripCategory) $ do
        case driverReq.endRideOtp of
          Just endRideOtp -> do
            unless ((Just endRideOtp) == rideOld.endOtp) $ throwError IncorrectOTP
          Nothing -> throwError $ EndRideOtpRequired (show booking.tripCategory)
      uiDistanceCalculation rideOld.id driverReq.uiDistanceCalculationWithAccuracy driverReq.uiDistanceCalculationWithoutAccuracy
      case requestor.role of
        DP.DRIVER -> unless (requestor.id == driverId) $ throwError NotAnExecutor
        _ -> throwError AccessDenied
    DashboardReq dashboardReq -> do
      unless (booking.providerId == dashboardReq.merchantId && booking.merchantOperatingCityId == dashboardReq.merchantOperatingCityId) $ throwError (RideDoesNotExist rideOld.id.getId)
    CronJobReq cronJobReq -> do
      unless (booking.providerId == cronJobReq.merchantId) $ throwError (RideDoesNotExist rideOld.id.getId)
    CallBasedReq callBasedEndRideReq -> do
      let requestor = callBasedEndRideReq.requestor
      case requestor.role of
        DP.DRIVER -> unless (requestor.id == driverId) $ throwError NotAnExecutor
        _ -> throwError AccessDenied

  unless (rideOld.status == DRide.INPROGRESS) $ throwError $ RideInvalidStatus "This ride cannot be ended"

  (tripEndPoint, mbOdometer) <- case req of
    DriverReq driverReq -> do
      when (DTC.isOdometerReadingsRequired booking.tripCategory && isNothing driverReq.odometer) $ throwError $ OdometerReadingRequired (show booking.tripCategory)
      logTagInfo "driver -> endRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId rideOld.id)
      pure (driverReq.point, driverReq.odometer)
    DashboardReq dashboardReq -> do
      when (DTC.isOdometerReadingsRequired booking.tripCategory && isNothing dashboardReq.odometer) $ throwError $ OdometerReadingRequired (show booking.tripCategory)
      logTagInfo "dashboard -> endRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId rideOld.id)
      case dashboardReq.point of
        Just point -> pure (point, dashboardReq.odometer)
        Nothing -> do
          -- FIX THIS
          toLocation <- booking.toLocation & fromMaybeM (InvalidRequest "Trip end location is required")
          pure $ (getCoordinates toLocation, dashboardReq.odometer)
    CronJobReq cronJobReq -> do
      logTagInfo "cron job -> endRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId rideOld.id)
      case cronJobReq.point of
        Just point -> pure (point, Nothing)
        Nothing -> do
          toLocation <- booking.toLocation & fromMaybeM (InvalidRequest "Trip end location is required")
          pure $ (getCoordinates toLocation, Nothing)
    CallBasedReq _ -> do
      toLocation <- booking.toLocation & fromMaybeM (InvalidRequest "Trip end location is required")
      pure $ (getCoordinates toLocation, Nothing)

  goHomeConfig <- CQGHC.findByMerchantOpCityId booking.merchantOperatingCityId
  ghInfo <- CQDGR.getDriverGoHomeRequestInfo driverId booking.merchantOperatingCityId (Just goHomeConfig)

  homeLocationReached' <-
    if ghInfo.status == Just DDGR.ACTIVE && goHomeConfig.enableGoHome && DTC.isGoHomeAvailable booking.tripCategory
      then do
        case ghInfo.driverGoHomeRequestId of
          Nothing -> do
            logError "DriverGoHomeRequestId not present even though status is active."
            return Nothing
          Just ghrId -> do
            mbDriverGoHomeReq <- QDGR.findById ghrId
            case mbDriverGoHomeReq of
              Just driverGoHomeReq -> do
                let driverHomeLocation = Maps.LatLong {lat = driverGoHomeReq.lat, lon = driverGoHomeReq.lon}
                routesResp <- DMaps.getTripRoutes (driverId, booking.providerId, booking.merchantOperatingCityId) (buildRoutesReq tripEndPoint driverHomeLocation)
                logDebug $ "Routes resp for EndRide API :" <> show routesResp <> "(source, dest) :" <> show (tripEndPoint, driverHomeLocation)
                let driverHomeDists = mapMaybe (.distance) routesResp
                if any ((<= goHomeConfig.destRadiusMeters) . getMeters) driverHomeDists
                  then do
                    CQDGR.deactivateDriverGoHomeRequest booking.merchantOperatingCityId driverId DDGR.SUCCESS ghInfo (Just True)
                    return $ Just True
                  else do
                    CQDGR.resetDriverGoHomeRequest booking.merchantOperatingCityId driverId goHomeConfig ghInfo
                    return $ Just False
              Nothing -> return Nothing
      else return Nothing

  whenWithLocationUpdatesLock driverId $ do
    now <- getCurrentTime
    thresholdConfig <- findConfig >>= fromMaybeM (InternalError "TransportConfigNotFound")
    let estimatedDistance = fromMaybe 0 booking.estimatedDistance -- TODO: Fix later with rentals
    tripEndPoints <- do
      res <- LF.rideEnd rideId tripEndPoint.lat tripEndPoint.lon booking.providerId driverId
      pure $ toList res.loc
    (chargeableDistance, finalFare, mbUpdatedFareParams, ride, pickupDropOutsideOfThreshold, distanceCalculationFailed) <-
      case req of
        CronJobReq _ -> do
          logTagInfo "cron job -> endRide : " "Do not call snapToRoad, return estimates as final values."
          (chargeableDistance, finalFare, mbUpdatedFareParams) <- recalculateFareForDistance handle booking rideOld estimatedDistance thresholdConfig
          pure (chargeableDistance, finalFare, mbUpdatedFareParams, rideOld, Nothing, Nothing)
        _ -> do
          if (DTC.isOdometerReadingsRequired booking.tripCategory)
            then do
              case mbOdometer of
                Just odometer -> do
                  let odometerCalculatedDistance = Meters $ round (odometer.value - maybe 0 (.value) rideOld.startOdometerReading) * 1000
                  (recalcDistance, finalFare, mbUpdatedFareParams) <- recalculateFareForDistance handle booking rideOld odometerCalculatedDistance thresholdConfig
                  pure (recalcDistance, finalFare, mbUpdatedFareParams, rideOld, Nothing, Nothing)
                Nothing -> throwError $ OdometerReadingRequired (show booking.tripCategory)
            else do
              -- here we update the current ride, so below we fetch the updated version
              pickupDropOutsideOfThreshold <- isPickupDropOutsideOfThreshold booking rideOld tripEndPoint thresholdConfig
              whenJust (nonEmpty tripEndPoints) \tripEndPoints' -> do
                withTimeAPI "endRide" "finalDistanceCalculation" $ finalDistanceCalculation rideOld.id driverId tripEndPoints' estimatedDistance pickupDropOutsideOfThreshold

              ride <- findRideById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)

              distanceCalculationFailed <- withTimeAPI "endRide" "isDistanceCalculationFailed" $ isDistanceCalculationFailed driverId
              when distanceCalculationFailed $ logWarning $ "Failed to calculate distance for this ride: " <> ride.id.getId
              (chargeableDistance, finalFare, mbUpdatedFareParams) <-
                if distanceCalculationFailed
                  then calculateFinalValuesForFailedDistanceCalculations handle booking ride tripEndPoint pickupDropOutsideOfThreshold thresholdConfig
                  else calculateFinalValuesForCorrectDistanceCalculations handle booking ride booking.maxEstimatedDistance pickupDropOutsideOfThreshold thresholdConfig
              pure (chargeableDistance, finalFare, mbUpdatedFareParams, ride, Just pickupDropOutsideOfThreshold, Just distanceCalculationFailed)
    let newFareParams = fromMaybe booking.fareParams mbUpdatedFareParams
    let updRide =
          ride{tripEndTime = Just now,
               chargeableDistance = Just chargeableDistance,
               fare = Just finalFare,
               tripEndPos = Just tripEndPoint,
               fareParametersId = Just newFareParams.id,
               distanceCalculationFailed = distanceCalculationFailed,
               pickupDropOutsideOfThreshold = pickupDropOutsideOfThreshold,
               endOdometerReading = mbOdometer
              }
    -- we need to store fareParams only when they changed
    withTimeAPI "endRide" "endRideTransaction" $ endRideTransaction (cast @DP.Person @DP.Driver driverId) booking updRide mbUpdatedFareParams booking.riderId newFareParams thresholdConfig
    withTimeAPI "endRide" "clearInterpolatedPoints" $ clearInterpolatedPoints driverId

    mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId -> do
      findPaymentMethodByIdAndMerchantId paymentMethodId booking.merchantOperatingCityId
        >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
    let mbPaymentUrl = DMPM.getPostpaidPaymentUrl =<< mbPaymentMethod
    let mbPaymentMethodInfo = DMPM.mkPaymentMethodInfo <$> mbPaymentMethod
    withTimeAPI "endRide" "notifyCompleteToBAP" $ notifyCompleteToBAP booking updRide newFareParams mbPaymentMethodInfo mbPaymentUrl (Just tripEndPoint)

    fork "sending dashboardSMS - CallbasedEndRide " $ do
      case req of
        CallBasedReq callBasedEndRideReq -> do
          let requestor = callBasedEndRideReq.requestor
          sendDashboardSms requestor.merchantId booking.merchantOperatingCityId Sms.ENDRIDE (Just ride) driverId (Just booking) (fromIntegral finalFare)
        _ -> pure ()

    logDebug "RideCompleted Coin Event"
    fork "DriverRideCompletedCoin Event : " $ do
      expirationPeriod <- DC.getExpirationSeconds thresholdConfig.timeDiffFromUtc
      validRideTaken <- DC.checkHasTakenValidRide (Just chargeableDistance)
      when validRideTaken $ DC.incrementValidRideCount driverId expirationPeriod 1
      DC.driverCoinsEvent driverId booking.providerId booking.merchantOperatingCityId (DCT.EndRide (isJust booking.disabilityTag) chargeableDistance)

  return $ EndRideResp {result = "Success", homeLocationReached = homeLocationReached'}
  where
    buildRoutesReq tripEndPoint driverHomeLocation =
      Maps.GetRoutesReq
        { waypoints = tripEndPoint :| [driverHomeLocation],
          mode = Nothing,
          calcPoints = True
        }

recalculateFareForDistance :: (MonadThrow m, Log m, MonadTime m, MonadGuid m) => ServiceHandle m -> SRB.Booking -> DRide.Ride -> Meters -> DTConf.TransporterConfig -> m (Meters, Money, Maybe FareParameters)
recalculateFareForDistance ServiceHandle {..} booking ride recalcDistance thresholdConfig = do
  let merchantId = booking.providerId
      oldDistance = fromMaybe 0 booking.estimatedDistance -- TODO: Fix later with rentals

  -- maybe compare only distance fare?
  let estimatedFare = Fare.fareSum booking.fareParams
  tripEndTime <- getCurrentTime
  farePolicy <- getFarePolicy booking.merchantOperatingCityId booking.tripCategory booking.vehicleVariant booking.area
  fareParams <-
    calculateFareParameters
      Fare.CalculateFareParametersParams
        { farePolicy = farePolicy,
          actualDistance = Just recalcDistance,
          estimatedDistance = Just oldDistance,
          rideTime = booking.startTime,
          waitingTime = secondsToMinutes . roundToIntegral <$> (diffUTCTime <$> ride.tripStartTime <*> ride.driverArrivalTime),
          actualRideDuration = roundToIntegral <$> (diffUTCTime <$> Just tripEndTime <*> ride.tripStartTime),
          estimatedRideDuration = booking.estimatedDuration,
          avgSpeedOfVehicle = thresholdConfig.avgSpeedOfVehicle,
          driverSelectedFare = booking.fareParams.driverSelectedFare,
          customerExtraFee = booking.fareParams.customerExtraFee,
          nightShiftCharge = booking.fareParams.nightShiftCharge,
          customerCancellationDues = booking.fareParams.customerCancellationDues,
          nightShiftOverlapChecking = True, -- make true only for rental
          timeDiffFromUtc = Just thresholdConfig.timeDiffFromUtc
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
      let dropDifference = maybe 0 (\toLocation -> abs $ distanceBetweenInMeters (getCoordinates toLocation) tripEndPoint) booking.toLocation
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
  let rideDistanceDifference = distance - (fromMaybe 0 booking.estimatedDistance) -- TODO: Fix with rentals
  logTagInfo "RideDistance differences" $
    "Distance Difference: "
      <> show rideDistanceDifference
  pure $ metersToHighPrecMeters rideDistanceDifference

calculateFinalValuesForCorrectDistanceCalculations ::
  (MonadThrow m, Log m, MonadTime m, MonadGuid m) => ServiceHandle m -> SRB.Booking -> DRide.Ride -> Maybe HighPrecMeters -> Bool -> DTConf.TransporterConfig -> m (Meters, Money, Maybe FareParameters)
calculateFinalValuesForCorrectDistanceCalculations handle booking ride mbMaxDistance pickupDropOutsideOfThreshold thresholdConfig = do
  distanceDiff <- getDistanceDiff booking (highPrecMetersToMeters ride.traveledDistance)
  let maxDistance = fromMaybe ride.traveledDistance mbMaxDistance + thresholdConfig.upwardsRecomputeBuffer
  let estimatedDistance = fromMaybe 0 booking.estimatedDistance -- TODO: Fix with rentals
  if not pickupDropOutsideOfThreshold
    then
      if distanceDiff > thresholdConfig.actualRideDistanceDiffThreshold
        then recalculateFareForDistance handle booking ride (roundToIntegral $ min ride.traveledDistance maxDistance) thresholdConfig
        else recalculateFareForDistance handle booking ride estimatedDistance thresholdConfig
    else
      if distanceDiff < 0
        then recalculateFareForDistance handle booking ride (roundToIntegral ride.traveledDistance) thresholdConfig
        else
          if distanceDiff < thresholdConfig.actualRideDistanceDiffThreshold
            then recalculateFareForDistance handle booking ride estimatedDistance thresholdConfig
            else recalculateFareForDistance handle booking ride (roundToIntegral ride.traveledDistance) thresholdConfig

calculateFinalValuesForFailedDistanceCalculations ::
  (MonadThrow m, Log m, MonadTime m, MonadGuid m) => ServiceHandle m -> SRB.Booking -> DRide.Ride -> LatLong -> Bool -> DTConf.TransporterConfig -> m (Meters, Money, Maybe FareParameters)
calculateFinalValuesForFailedDistanceCalculations handle@ServiceHandle {..} booking ride tripEndPoint pickupDropOutsideOfThreshold thresholdConfig = do
  let tripStartPoint = case ride.tripStartPos of
        Nothing -> getCoordinates booking.fromLocation
        Just tripStartPos -> tripStartPos
  interpolatedPoints <- getInterpolatedPoints ride.driverId
  let estimatedDistance = fromMaybe 0 booking.estimatedDistance -- TODO: Fix with rentals
  if not pickupDropOutsideOfThreshold
    then recalculateFareForDistance handle booking ride estimatedDistance thresholdConfig -- TODO: Fix with rentals
    else do
      approxTraveledDistance <- getDistanceBetweenPoints tripStartPoint tripEndPoint interpolatedPoints
      logTagInfo "endRide" $ "approxTraveledDistance when pickup and drop are not outside threshold: " <> show approxTraveledDistance
      distanceDiff <- getDistanceDiff booking approxTraveledDistance
      if distanceDiff < 0
        then recalculateFareForDistance handle booking ride approxTraveledDistance thresholdConfig
        else
          if distanceDiff < thresholdConfig.actualRideDistanceDiffThreshold
            then do
              recalculateFareForDistance handle booking ride estimatedDistance thresholdConfig
            else do
              if distanceDiff < thresholdConfig.upwardsRecomputeBuffer
                then recalculateFareForDistance handle booking ride approxTraveledDistance thresholdConfig
                else do
                  logTagInfo "Inaccurate Location Updates and Pickup/Drop Deviated." ("DistanceDiff: " <> show distanceDiff)
                  recalculateFareForDistance handle booking ride (estimatedDistance + highPrecMetersToMeters thresholdConfig.upwardsRecomputeBuffer) thresholdConfig
