{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

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
import qualified Data.Text as Text
import qualified Data.Tuple.Extra as Tuple
import qualified Domain.Action.UI.Ride.EndRide.Internal as RideEndInt
import Domain.Action.UI.Route as DMaps
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.DriverGoHomeRequest as DDGR
import Domain.Types.FareParameters as Fare
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderDetails as RD
import qualified Domain.Types.ServiceTierType as DVST
import qualified Domain.Types.TransporterConfig as DTConf
import Environment (Flow)
import EulerHS.Prelude hiding (id, pi)
import Kernel.External.Maps
import qualified Kernel.External.Maps.Interface.Types as Maps
import qualified Kernel.External.Maps.Types as Maps
import Kernel.Prelude (roundToIntegral)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Common hiding (Days)
import Kernel.Types.Confidence
import Kernel.Types.Id
import Kernel.Types.SlidingWindowCounters
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common hiding (Days)
import Kernel.Utils.DatastoreLatencyCalculator
import Kernel.Utils.GenericPretty (PrettyShow)
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import qualified Lib.DriverCoins.Coins as DC
import qualified Lib.DriverCoins.Types as DCT
import qualified Lib.LocationUpdates as LocUpd
import qualified Lib.Types.SpecialLocation as SL
import qualified SharedLogic.CallBAP as CallBAP
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.FareCalculator as Fare
import qualified SharedLogic.FarePolicy as FarePolicy
import qualified SharedLogic.MerchantPaymentMethod as DMPM
import qualified Storage.Cac.GoHomeConfig as CGHC
import qualified Storage.Cac.TransporterConfig as QTC
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Merchant as MerchantS
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.Queries.Booking as QRB
import Storage.Queries.DriverGoHomeRequest as QDGR
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRD
import Tools.Error
import qualified Tools.Maps as TM
import qualified Tools.Notifications as TN
import qualified Tools.SMS as Sms
import Utils.Common.Cac.KeyNameConstants

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
    getFarePolicyByEstOrQuoteId :: Maybe LatLong -> Id DMOC.MerchantOperatingCity -> DTC.TripCategory -> DVST.ServiceTierType -> Maybe SL.Area -> Text -> Maybe Bool -> Maybe CacKey -> m DFP.FullFarePolicy,
    calculateFareParameters :: Fare.CalculateFareParametersParams -> m Fare.FareParameters,
    putDiffMetric :: Id DM.Merchant -> HighPrecMoney -> Meters -> m (),
    isDistanceCalculationFailed :: Id DP.Person -> m Bool,
    finalDistanceCalculation :: Maybe MapsServiceConfig -> Bool -> Bool -> Id DRide.Ride -> Id DP.Person -> NonEmpty LatLong -> Meters -> Maybe HighPrecMoney -> Maybe [Text] -> Bool -> m (),
    getInterpolatedPoints :: Id DP.Person -> m [LatLong],
    clearInterpolatedPoints :: Id DP.Person -> m (),
    findConfig :: Maybe CacKey -> m (Maybe DTConf.TransporterConfig),
    whenWithLocationUpdatesLock :: Id DP.Person -> m () -> m (),
    getRouteAndDistanceBetweenPoints :: LatLong -> LatLong -> [LatLong] -> Meters -> m ([LatLong], Meters),
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
        getFarePolicyByEstOrQuoteId = FarePolicy.getFarePolicyByEstOrQuoteId,
        calculateFareParameters = Fare.calculateFareParameters,
        putDiffMetric = RideEndInt.putDiffMetric,
        isDistanceCalculationFailed = LocUpd.isDistanceCalculationFailed defaultRideInterpolationHandler,
        finalDistanceCalculation = LocUpd.finalDistanceCalculation defaultRideInterpolationHandler,
        getInterpolatedPoints = LocUpd.getInterpolatedPoints defaultRideInterpolationHandler,
        clearInterpolatedPoints = LocUpd.clearInterpolatedPoints defaultRideInterpolationHandler,
        findConfig = QTC.findByMerchantOpCityId merchantOpCityId,
        whenWithLocationUpdatesLock = LocUpd.whenWithLocationUpdatesLock,
        getRouteAndDistanceBetweenPoints = RideEndInt.getRouteAndDistanceBetweenPoints merchantId merchantOpCityId,
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
            unless (Just endRideOtp == rideOld.endOtp) $ throwError IncorrectOTP
          Nothing -> pure ()
      -- throwError $ EndRideOtpRequired (show booking.tripCategory)
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

  unless (rideOld.status == DRide.INPROGRESS) $ throwError $ RideInvalidStatus ("This ride cannot be ended" <> Text.pack (show rideOld.status))

  (tripEndPoint, mbOdometer, rideEndedBy') <- case req of
    DriverReq driverReq -> do
      when (DTC.isOdometerReadingsRequired booking.tripCategory && isNothing driverReq.odometer) $ throwError $ OdometerReadingRequired (show booking.tripCategory)
      logTagInfo "driver -> endRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId rideOld.id)
      pure (driverReq.point, driverReq.odometer, DRide.Driver)
    DashboardReq dashboardReq -> do
      when (DTC.isOdometerReadingsRequired booking.tripCategory && isNothing dashboardReq.odometer) $ throwError $ OdometerReadingRequired (show booking.tripCategory)
      logTagInfo "dashboard -> endRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId rideOld.id)
      case dashboardReq.point of
        Just point -> pure (point, dashboardReq.odometer, DRide.Dashboard)
        Nothing -> do
          -- FIX THIS
          toLocation <- booking.toLocation & fromMaybeM (InvalidRequest "Trip end location is required")
          pure (getCoordinates toLocation, dashboardReq.odometer, DRide.Dashboard)
    CronJobReq cronJobReq -> do
      logTagInfo "cron job -> endRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId rideOld.id)
      case cronJobReq.point of
        Just point -> pure (point, Nothing, DRide.CronJob)
        Nothing -> do
          toLocation <- booking.toLocation & fromMaybeM (InvalidRequest "Trip end location is required")
          pure (getCoordinates toLocation, Nothing, DRide.CronJob)
    CallBasedReq _ -> do
      toLocation <- booking.toLocation & fromMaybeM (InvalidRequest "Trip end location is required")
      pure (getCoordinates toLocation, Nothing, DRide.CallBased)

  goHomeConfig <- CGHC.findByMerchantOpCityId booking.merchantOperatingCityId (Just (TransactionId (Id booking.transactionId)))
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
    thresholdConfig <- findConfig (Just (TransactionId (Id booking.transactionId))) >>= fromMaybeM (InternalError "TransportConfigNotFound")
    let estimatedDistance = fromMaybe 0 booking.estimatedDistance -- TODO: Fix later with rentals
        estimatedTollCharges = rideOld.estimatedTollCharges
        estimatedTollNames = rideOld.estimatedTollNames
        shouldRectifyDistantPointsSnapToRoadFailure = DTC.shouldRectifyDistantPointsSnapToRoadFailure booking.tripCategory
    tripEndPoints <- do
      res <- LF.rideEnd rideId tripEndPoint.lat tripEndPoint.lon booking.providerId driverId
      pure $ toList res.loc
    (chargeableDistance, chargeableRideDuration, finalFare, mbUpdatedFareParams, ride, pickupDropOutsideOfThreshold, distanceCalculationFailed) <-
      case req of
        CronJobReq _ -> do
          logTagInfo "cron job -> endRide : " "Do not call snapToRoad, return estimates as final values."
          res <- try @_ @SomeException $ recalculateFareForDistanceAndDuration handle booking rideOld estimatedDistance thresholdConfig
          fareRecalcResult <-
            case res of
              Left err -> do
                logTagError "recalculateFareForDistanceAndDuration" $ "Failed to recalculate fare : " <> show err
                return $
                  FareRecalculationResult
                    { chargeableDistance = fromMaybe 0 booking.estimatedDistance,
                      chargeableRideDuration = secondsToMinutesCeiling <$> booking.estimatedDuration,
                      finalFare = booking.estimatedFare,
                      updatedFareParams = Nothing
                    }
              Right response -> return response

          pure (fareRecalcResult.chargeableDistance, fareRecalcResult.chargeableRideDuration, fareRecalcResult.finalFare, fareRecalcResult.updatedFareParams, rideOld, Nothing, Nothing)
        _ -> do
          withFallback (fromMaybe 0 booking.estimatedDistance, secondsToMinutesCeiling <$> booking.estimatedDuration, booking.estimatedFare, Nothing, rideOld, Nothing, Nothing) $ do
            if DTC.isOdometerReadingsRequired booking.tripCategory
              then do
                case mbOdometer of
                  Just odometer -> do
                    unless (odometer.value >= maybe 0 (.value) rideOld.startOdometerReading) $ throwError InvalidEndOdometerReading
                    let odometerCalculatedDistance = Meters $ round (odometer.value - maybe 0 (.value) rideOld.startOdometerReading) * 1000
                    fareRecalcResult <- recalculateFareForDistanceAndDuration handle booking rideOld odometerCalculatedDistance thresholdConfig
                    pure (fareRecalcResult.chargeableDistance, fareRecalcResult.chargeableRideDuration, fareRecalcResult.finalFare, fareRecalcResult.updatedFareParams, rideOld, Nothing, Nothing)
                  Nothing -> throwError $ OdometerReadingRequired (show booking.tripCategory)
              else do
                -- here we update the current ride, so below we fetch the updated version
                pickupDropOutsideOfThreshold <- isPickupDropOutsideOfThreshold booking rideOld tripEndPoint thresholdConfig
                whenJust (nonEmpty tripEndPoints) \tripEndPoints' -> do
                  rectificationMapsConfig <-
                    if shouldRectifyDistantPointsSnapToRoadFailure
                      then Just <$> TM.getServiceConfigForRectifyingSnapToRoadDistantPointsFailure booking.providerId booking.merchantOperatingCityId
                      else pure Nothing
                  withTimeAPI "endRide" "finalDistanceCalculation" $ finalDistanceCalculation rectificationMapsConfig (DTC.isTollApplicableForTrip booking.vehicleServiceTier booking.tripCategory) thresholdConfig.enableTollCrossedNotifications rideOld.id driverId tripEndPoints' estimatedDistance estimatedTollCharges estimatedTollNames pickupDropOutsideOfThreshold

                updRide <- findRideById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)

                distanceCalculationFailed <- withTimeAPI "endRide" "isDistanceCalculationFailed" $ isDistanceCalculationFailed driverId

                when distanceCalculationFailed $ do
                  logWarning $ "Failed to calculate distance for this ride: " <> rideId.getId

                let (tollCharges, tollNames, tollConfidence) = do
                      let distanceCalculationFailure = distanceCalculationFailed || (maybe False (> 0) updRide.numberOfSelfTuned)
                          driverDeviationToTollRoute = fromMaybe False updRide.driverDeviatedToTollRoute
                      if isJust updRide.tollCharges && driverDeviationToTollRoute && distanceCalculationFailure
                        then (updRide.tollCharges, updRide.tollNames, Just Neutral)
                        else
                          if (isJust updRide.estimatedTollCharges || isJust updRide.tollCharges) && distanceCalculationFailure
                            then (Nothing, Nothing, Just Unsure)
                            else (updRide.tollCharges, updRide.tollNames, Just Sure)

                let ride = updRide{tollCharges = tollCharges, tollNames = tollNames, tollConfidence = tollConfidence}

                fareRecalcResult <-
                  if shouldRectifyDistantPointsSnapToRoadFailure
                    then recalculateFareForDistanceAndDuration handle booking ride (max (roundToIntegral ride.traveledDistance) (fromMaybe 0 booking.estimatedDistance)) thresholdConfig
                    else
                      if distanceCalculationFailed
                        then calculateFinalValuesForFailedDistanceCalculations handle booking ride tripEndPoint pickupDropOutsideOfThreshold thresholdConfig
                        else calculateFinalValuesForCorrectDistanceCalculations handle booking ride booking.maxEstimatedDistance pickupDropOutsideOfThreshold thresholdConfig
                pure (fareRecalcResult.chargeableDistance, fareRecalcResult.chargeableRideDuration, fareRecalcResult.finalFare, fareRecalcResult.updatedFareParams, ride, Just pickupDropOutsideOfThreshold, Just distanceCalculationFailed)
    let newFareParams = fromMaybe booking.fareParams mbUpdatedFareParams
    let updRide =
          ride{tripEndTime = Just now,
               chargeableRideDuration,
               chargeableDistance = Just chargeableDistance,
               fare = Just finalFare,
               status = DRide.COMPLETED,
               tripEndPos = Just tripEndPoint,
               rideEndedBy = Just rideEndedBy',
               fareParametersId = Just newFareParams.id,
               distanceCalculationFailed = distanceCalculationFailed,
               pickupDropOutsideOfThreshold = pickupDropOutsideOfThreshold,
               endOdometerReading = mbOdometer
              }
    fork "updating time and latlong in advance ride if any" $ do
      advanceRide <- QRide.getActiveAdvancedRideByDriverId driverId
      whenJust advanceRide $ \advanceRide' -> do
        QRide.updatePreviousRideTripEndPosAndTime (Just tripEndPoint) (Just now) advanceRide'.id

    -- we need to store fareParams only when they changed
    withTimeAPI "endRide" "endRideTransaction" $ endRideTransaction (cast @DP.Person @DP.Driver driverId) booking updRide mbUpdatedFareParams booking.riderId newFareParams thresholdConfig
    withTimeAPI "endRide" "clearInterpolatedPoints" $ clearInterpolatedPoints driverId

    logDebug $ "RideCompleted Coin Event" <> show chargeableDistance
    fork "DriverRideCompletedCoin Event : " $ do
      expirationPeriod <- DC.getExpirationSeconds thresholdConfig.timeDiffFromUtc
      validRideTaken <- DC.checkHasTakenValidRide (Just chargeableDistance)
      when (DTC.isDynamicOfferTrip booking.tripCategory && validRideTaken) $ do
        DC.incrementValidRideCount driverId expirationPeriod 1
        DC.driverCoinsEvent driverId booking.providerId booking.merchantOperatingCityId (DCT.EndRide (isJust booking.disabilityTag) chargeableDistance)

    mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId -> do
      findPaymentMethodByIdAndMerchantId paymentMethodId booking.merchantOperatingCityId
        >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
    let mbPaymentMethodInfo = DMPM.mkPaymentMethodInfo <$> mbPaymentMethod
    withTimeAPI "endRide" "notifyCompleteToBAP" $ notifyCompleteToBAP booking updRide newFareParams mbPaymentMethodInfo Nothing (Just tripEndPoint)

    fork "sending dashboardSMS - CallbasedEndRide " $ do
      case req of
        CallBasedReq callBasedEndRideReq -> do
          let requestor = callBasedEndRideReq.requestor
          sendDashboardSms requestor.merchantId booking.merchantOperatingCityId Sms.ENDRIDE (Just ride) driverId (Just booking) finalFare
        _ -> pure ()

  return $ EndRideResp {result = "Success", homeLocationReached = homeLocationReached'}
  where
    buildRoutesReq tripEndPoint driverHomeLocation =
      Maps.GetRoutesReq
        { waypoints = tripEndPoint :| [driverHomeLocation],
          mode = Nothing,
          calcPoints = True
        }

    withFallback defaultVal action = do
      res <- try @_ @SomeException action
      case res of
        Left someException ->
          case fromException someException of
            Just NoFareProduct -> return defaultVal
            _ -> throwError $ InternalError (Text.pack $ displayException someException)
        Right resp -> return resp

data RideDurationFareRecomputeParams = RideDurationFareRecomputeParams
  { isTimeBasedFareToBeRecomputed :: Bool,
    diffThreshold :: Maybe Minutes,
    upwardsRecomputeBuffer :: Maybe Minutes,
    downwardsRecomputeBuffer :: Maybe Minutes
  }
  deriving (Generic, Show)
  deriving anyclass (PrettyShow)

-- | Below are the conditions required for ride duration based fare re-computation to happen:
-- |  1. `diffThresholdInMins` is `Just` (acts like a flag to enable re-computation)
-- |  2. `isTimeBasedFareToBeRecomputed` should be `True` (to control when to recompute)
-- |  3. `abs durationDiff` >= `diffThresholdInMins`
-- |      a. if `durationDiff` is negative and `downwardsRecomputeBuffer` is `Just` (acts like a flag to enable downwards re-computation)
-- |          > rideDuration = max (estimatedDuration - downwardsRecomputeBuffer) actualRideDuration
-- |      b. if `durationDiff` is positive and `upwardsRecomputeBuffer` is `Just` (acts like a flag to enable upwards re-computation)
-- |          > rideDuration = min (estimatedDuration + upwardsRecomputeBuffer) actualRideDuration
calculateRideDurationForFare :: RideDurationFareRecomputeParams -> (Minutes, Minutes) -> Minutes
calculateRideDurationForFare (RideDurationFareRecomputeParams {..}) (estimatedDuration, actualRideDuration) =
  maybe
    estimatedDuration
    ( \threshold -> do
        {- NOTE: `diffThresholdInMins` should always be less than `min(upwardsRecomputeBuffer, downwardsRecomputeBuffer)` -}
        if not isTimeBasedFareToBeRecomputed
          then estimatedDuration
          else do
            let durationDiff = actualRideDuration - estimatedDuration
                upwardRecomputedRideDuration = maybe estimatedDuration (\buffer -> min (estimatedDuration + buffer) actualRideDuration) upwardsRecomputeBuffer
                downwardRecomputedRideDuration = maybe estimatedDuration (\buffer -> max (estimatedDuration - buffer) actualRideDuration) downwardsRecomputeBuffer
            if abs durationDiff < threshold
              then estimatedDuration
              else
                if durationDiff < 0
                  then downwardRecomputedRideDuration
                  else upwardRecomputedRideDuration
    )
    diffThreshold

data FareRecalculationResult = FareRecalculationResult
  { chargeableDistance :: Meters,
    chargeableRideDuration :: Maybe Minutes,
    finalFare :: HighPrecMoney,
    updatedFareParams :: Maybe FareParameters
  }
  deriving (Generic, Show)
  deriving anyclass (PrettyShow)

recalculateFareForDistanceAndDuration :: (MonadThrow m, Log m, MonadTime m, MonadGuid m, EsqDBFlow m r, CacheFlow m r) => ServiceHandle m -> SRB.Booking -> DRide.Ride -> Meters -> DTConf.TransporterConfig -> m FareRecalculationResult
recalculateFareForDistanceAndDuration ServiceHandle {..} booking ride recalcDistance thresholdConfig = do
  let merchantId = booking.providerId
      oldDistance = fromMaybe 0 booking.estimatedDistance -- TODO: Fix later with rentals

  -- maybe compare only distance fare?
  let estimatedFare = Fare.fareSum booking.fareParams
  vehicleAge <-
    if DTC.isAmbulanceTrip booking.tripCategory
      then do
        rideDetail <- QRD.findById ride.id -- replica?
        pure $ (.vehicleAge) =<< rideDetail
      else pure Nothing
  tripEndTime <- getCurrentTime
  let actualRideDuration = roundToIntegral <$> (diffUTCTime <$> Just tripEndTime <*> ride.tripStartTime)
      estimatedDuration = booking.estimatedDuration
      isDistanceChanged = recalcDistance /= oldDistance
      fareRecomputeParams =
        RideDurationFareRecomputeParams
          { isTimeBasedFareToBeRecomputed = isDistanceChanged,
            diffThreshold = thresholdConfig.rideDurationDiffThreshold,
            upwardsRecomputeBuffer = thresholdConfig.rideDurationUpwardsRecomputeBuffer,
            downwardsRecomputeBuffer = thresholdConfig.rideDurationDownwardsRecomputeBuffer
          }
      rideDurationForFareCalc = calculateRideDurationForFare fareRecomputeParams . Tuple.both secondsToMinutesCeiling <$> liftA2 (,) estimatedDuration actualRideDuration

  farePolicy <- getFarePolicyByEstOrQuoteId (Just $ getCoordinates booking.fromLocation) booking.merchantOperatingCityId booking.tripCategory booking.vehicleServiceTier booking.area booking.quoteId (Just booking.isDashboardRequest) (Just (TransactionId (Id booking.transactionId)))
  fareParams <-
    calculateFareParameters
      Fare.CalculateFareParametersParams
        { farePolicy = farePolicy,
          actualDistance = Just recalcDistance,
          estimatedDistance = Just oldDistance,
          rideTime = booking.startTime,
          returnTime = booking.returnTime,
          roundTrip = fromMaybe False booking.roundTrip,
          waitingTime = if isNothing ride.driverArrivalTime then Nothing else fmap (max 0) (secondsToMinutes . roundToIntegral <$> (diffUTCTime <$> ride.tripStartTime <*> (liftA2 max ride.driverArrivalTime (Just booking.startTime)))),
          actualRideDuration,
          estimatedRideDuration = estimatedDuration,
          rideDurationForFareCalc,
          avgSpeedOfVehicle = thresholdConfig.avgSpeedOfVehicle,
          driverSelectedFare = booking.fareParams.driverSelectedFare,
          customerExtraFee = booking.fareParams.customerExtraFee,
          nightShiftCharge = booking.fareParams.nightShiftCharge,
          customerCancellationDues = booking.fareParams.customerCancellationDues,
          nightShiftOverlapChecking = DTC.isFixedNightCharge booking.tripCategory,
          timeDiffFromUtc = Just thresholdConfig.timeDiffFromUtc,
          tollCharges = ride.tollCharges,
          vehicleAge = vehicleAge,
          currency = booking.currency,
          distanceUnit = booking.distanceUnit
        }
  let finalFare = Fare.fareSum fareParams
      distanceDiff = recalcDistance - oldDistance
      fareDiff = finalFare - estimatedFare
      recalcRideDuration = rideDurationForFareCalc
  logTagInfo "Fare recalculation" $
    "Fare difference: "
      <> show (realToFrac @_ @Double fareDiff)
      <> ", Distance difference: "
      <> show distanceDiff
      <> maybe "" (\duration -> ", Ride Duration difference in mins: " <> show duration) (liftA2 (-) recalcRideDuration $ secondsToMinutesCeiling <$> estimatedDuration)
  putDiffMetric merchantId fareDiff distanceDiff
  return $
    FareRecalculationResult
      { chargeableDistance = recalcDistance,
        chargeableRideDuration = recalcRideDuration,
        finalFare,
        updatedFareParams = Just fareParams
      }

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
  (MonadFlow m, MonadThrow m, Log m, MonadTime m, MonadGuid m, EsqDBFlow m r, CacheFlow m r) => ServiceHandle m -> SRB.Booking -> DRide.Ride -> Maybe HighPrecMeters -> Bool -> DTConf.TransporterConfig -> m FareRecalculationResult
calculateFinalValuesForCorrectDistanceCalculations handle booking ride mbMaxDistance pickupDropOutsideOfThreshold thresholdConfig = do
  distanceDiff <- getDistanceDiff booking (highPrecMetersToMeters ride.traveledDistance)
  let thresholdChecks = distanceDiff > thresholdConfig.actualRideDistanceDiffThresholdIfWithinPickupDrop && thresholdConfig.recomputeIfPickupDropNotOutsideOfThreshold
  (mbDailyExtraKms, mbWeeklyExtraKms) <- if thresholdChecks then handleExtraKmsRecomputation distanceDiff else return (Nothing, Nothing)
  fork "Send Extra Kms Limit Exceeded Overlay" $
    when (thresholdConfig.toNotifyDriverForExtraKmsLimitExceed && not (checkExtraKmsThreshold mbDailyExtraKms mbWeeklyExtraKms)) notifyDriverOnExtraKmsLimitExceed
  let maxDistance = fromMaybe ride.traveledDistance mbMaxDistance + thresholdConfig.upwardsRecomputeBuffer
  let estimatedDistance = fromMaybe 0 booking.estimatedDistance -- TODO: Fix with rentals
  if not pickupDropOutsideOfThreshold
    then
      if thresholdChecks && checkExtraKmsThreshold mbDailyExtraKms mbWeeklyExtraKms
        then recalculateFareForDistanceAndDuration handle booking ride (roundToIntegral $ min ride.traveledDistance maxDistance) thresholdConfig
        else recalculateFareForDistanceAndDuration handle booking ride estimatedDistance thresholdConfig
    else
      if distanceDiff < 0
        then recalculateFareForDistanceAndDuration handle booking ride (roundToIntegral ride.traveledDistance) thresholdConfig
        else
          if distanceDiff < thresholdConfig.actualRideDistanceDiffThreshold
            then recalculateFareForDistanceAndDuration handle booking ride estimatedDistance thresholdConfig
            else recalculateFareForDistanceAndDuration handle booking ride (roundToIntegral ride.traveledDistance) thresholdConfig
  where
    makeDailyAndWeeklyExtraKmsKey personId = ("DailyExtraKms:PersonId-" <> personId, "WeeklyExtraKms:PersonId-" <> personId)

    checkExtraKmsThreshold (Just dailyExtraKms) (Just weeklyExtraKms) = thresholdConfig.fareRecomputeDailyExtraKmsThreshold >= dailyExtraKms && thresholdConfig.fareRecomputeWeeklyExtraKmsThreshold >= weeklyExtraKms
    checkExtraKmsThreshold _ _ = True

    handleExtraKmsRecomputation distanceDiff = do
      expirationPeriodForDay <- DC.getExpirationSeconds thresholdConfig.timeDiffFromUtc
      let (dailyExtraKmsKey, weeklyExtraKmsKey) = makeDailyAndWeeklyExtraKmsKey ride.driverId.getId
      prevDailyExtraKms <- Redis.get dailyExtraKmsKey
      prevWeeklyExtraKms <- fromIntegral <$> SWC.getCurrentWindowCount weeklyExtraKmsKey SlidingWindowOptions {period = 7, periodType = Days}
      let dailyExtraKms = fromMaybe 0 prevDailyExtraKms + distanceDiff
          weeklyExtraKms = (prevWeeklyExtraKms :: HighPrecMeters) + distanceDiff
      Redis.setExp dailyExtraKmsKey dailyExtraKms expirationPeriodForDay
      SWC.incrementByValue (round distanceDiff) weeklyExtraKmsKey SlidingWindowOptions {period = 7, periodType = Days}
      pure (Just dailyExtraKms, Just weeklyExtraKms)

    notifyDriverOnExtraKmsLimitExceed = do
      driver <- QP.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
      overlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdf booking.merchantOperatingCityId "EXTRA_KMS_LIMIT_EXCEEDED" (fromMaybe ENGLISH driver.language) Nothing >>= fromMaybeM (OverlayKeyNotFound "EXTRA_KMS_LIMIT_EXCEEDED")
      TN.sendOverlay booking.merchantOperatingCityId driver $ TN.mkOverlayReq overlay

calculateFinalValuesForFailedDistanceCalculations ::
  (MonadThrow m, Log m, MonadTime m, MonadGuid m, EsqDBFlow m r, CacheFlow m r) => ServiceHandle m -> SRB.Booking -> DRide.Ride -> LatLong -> Bool -> DTConf.TransporterConfig -> m FareRecalculationResult
calculateFinalValuesForFailedDistanceCalculations handle@ServiceHandle {..} booking ride tripEndPoint pickupDropOutsideOfThreshold thresholdConfig = do
  let tripStartPoint = case ride.tripStartPos of
        Nothing -> getCoordinates booking.fromLocation
        Just tripStartPos -> tripStartPos
  interpolatedPoints <- getInterpolatedPoints ride.driverId
  let estimatedDistance = fromMaybe 0 booking.estimatedDistance -- TODO: Fix with rentals
  if not pickupDropOutsideOfThreshold
    then recalculateFareForDistanceAndDuration handle booking ride estimatedDistance thresholdConfig -- TODO: Fix with rentals
    else do
      (_routePoints, approxTraveledDistance) <- getRouteAndDistanceBetweenPoints tripStartPoint tripEndPoint interpolatedPoints estimatedDistance
      logTagInfo "endRide" $ "approxTraveledDistance when pickup and drop are not outside threshold: " <> show approxTraveledDistance
      distanceDiff <- getDistanceDiff booking approxTraveledDistance
      if distanceDiff < 0
        then do
          recalculateFareForDistanceAndDuration handle booking ride approxTraveledDistance thresholdConfig -- TODO :: Recompute Toll Charges Here ?
        else
          if distanceDiff < thresholdConfig.actualRideDistanceDiffThreshold
            then do
              recalculateFareForDistanceAndDuration handle booking ride estimatedDistance thresholdConfig
            else do
              if distanceDiff < thresholdConfig.upwardsRecomputeBuffer
                then recalculateFareForDistanceAndDuration handle booking ride approxTraveledDistance thresholdConfig -- TODO :: Recompute Toll Charges Here ?
                else do
                  logTagInfo "Inaccurate Location Updates and Pickup/Drop Deviated." ("DistanceDiff: " <> show distanceDiff)
                  recalculateFareForDistanceAndDuration handle booking ride (estimatedDistance + highPrecMetersToMeters thresholdConfig.upwardsRecomputeBuffer) thresholdConfig
