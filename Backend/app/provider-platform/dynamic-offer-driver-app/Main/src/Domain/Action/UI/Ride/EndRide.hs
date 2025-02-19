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

import qualified Beckn.OnDemand.Utils.Common as BODUC
import Data.Either.Extra (eitherToMaybe)
import Data.Maybe (listToMaybe)
import Data.OpenApi.Internal.Schema (ToSchema)
import qualified Data.Text as Text
import qualified Domain.Action.UI.Ride.EndRide.Internal as RideEndInt
import Domain.Action.UI.Route as DMaps
import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.DriverGoHomeRequest as DDGR
import Domain.Types.FareParameters as Fare
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderDetails as RD
import qualified Domain.Types.TransporterConfig as DTConf
import qualified Domain.Types.Yudhishthira as Y
import Environment (Flow)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id, pi)
import Kernel.Beam.Lib.Utils (pushToKafka)
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
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import qualified Lib.DriverCoins.Coins as DC
import qualified Lib.DriverCoins.Types as DCT
import qualified Lib.LocationUpdates as LocUpd
import qualified Lib.LocationUpdates.Internal as LocUpdInternal
import qualified Lib.Types.SpecialLocation as SL
import qualified Lib.Yudhishthira.Event as Yudhishthira
import qualified Lib.Yudhishthira.Types as Yudhishthira
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
import qualified Storage.Queries.StopInformation as QSI
import Tools.Error
import qualified Tools.Maps as TM
import qualified Tools.Notifications as TN
import qualified Tools.SMS as Sms
import Tools.Utils
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

data RideInterpolationData = RideInterpolationData
  { interpolatedPoints :: [LatLong],
    rideId :: Id DRide.Ride
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype CallBasedEndRideReq = CallBasedEndRideReq
  { requestor :: DP.Person
  }

data ServiceHandle m = ServiceHandle
  { findBookingById :: Id SRB.Booking -> m (Maybe SRB.Booking),
    findRideById :: Id DRide.Ride -> m (Maybe DRide.Ride),
    getMerchant :: Id DM.Merchant -> m (Maybe DM.Merchant),
    endRideTransaction :: Id DP.Driver -> SRB.Booking -> DRide.Ride -> Maybe FareParameters -> Maybe (Id RD.RiderDetails) -> FareParameters -> DTConf.TransporterConfig -> m (),
    notifyCompleteToBAP :: SRB.Booking -> DRide.Ride -> Fare.FareParameters -> Maybe DMPM.PaymentMethodInfo -> Maybe Text -> Maybe LatLong -> m (),
    getFarePolicyByEstOrQuoteId :: Maybe LatLong -> Maybe Text -> Maybe Text -> Maybe Meters -> Maybe Seconds -> Id DMOC.MerchantOperatingCity -> DTC.TripCategory -> DVST.ServiceTierType -> Maybe SL.Area -> Text -> Maybe UTCTime -> Maybe Bool -> Maybe Int -> Maybe CacKey -> m DFP.FullFarePolicy,
    getFarePolicyOnEndRide :: Maybe LatLong -> Maybe Text -> Maybe Text -> Maybe Meters -> Maybe Seconds -> LatLong -> Id DMOC.MerchantOperatingCity -> DTC.TripCategory -> DVST.ServiceTierType -> Maybe SL.Area -> Text -> Maybe UTCTime -> Maybe Bool -> Maybe Int -> Maybe CacKey -> m DFP.FullFarePolicy,
    calculateFareParameters :: Fare.CalculateFareParametersParams -> m Fare.FareParameters,
    putDiffMetric :: Id DM.Merchant -> HighPrecMoney -> Meters -> m (),
    isDistanceCalculationFailed :: Id DP.Person -> m Bool,
    finalDistanceCalculation :: Maybe MapsServiceConfig -> Bool -> Bool -> Id DRide.Ride -> Id DP.Person -> NonEmpty LatLong -> Meters -> Maybe HighPrecMoney -> Maybe [Text] -> Bool -> Bool -> m (),
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
        getFarePolicyOnEndRide = FarePolicy.getFarePolicyOnEndRide,
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
    LT.HasLocationService m r
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
endRide handle rideId req = withLogTag ("rideId-" <> rideId.getId) do
  isLocked <- withLockRideId
  if isLocked
    then do
      finally
        (endRideHandler handle rideId req)
        ( do
            Redis.unlockRedis mkLockKey
            logDebug $ "End ride for RideId: " <> rideId.getId <> " Unlocked"
        )
    else throwError (InternalError $ "End ride inprogress")
  where
    withLockRideId = do
      isLocked <- Redis.tryLockRedis mkLockKey 60
      return isLocked
    mkLockKey = "EndTransaction:RID:-" <> rideId.getId

endRideHandler ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  EndRideReq ->
  m EndRideResp
endRideHandler handle@ServiceHandle {..} rideId req = do
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
    advanceRide <- QRide.getActiveAdvancedRideByDriverId driverId
    tripEndPoints <- do
      let mbAdvanceRideId = (.id) <$> advanceRide
      res <- LF.rideEnd rideId tripEndPoint.lat tripEndPoint.lon booking.providerId driverId mbAdvanceRideId
      pure $ toList res.loc
    (chargeableDistance, finalFare, mbUpdatedFareParams, ride, pickupDropOutsideOfThreshold, distanceCalculationFailed) <-
      case req of
        CronJobReq _ -> do
          logTagInfo "cron job -> endRide : " "Do not call snapToRoad, return estimates as final values."
          res <- try @_ @SomeException $ recalculateFareForDistance handle booking rideOld estimatedDistance thresholdConfig False tripEndPoint
          (chargeableDistance, finalFare, mbUpdatedFareParams) <-
            case res of
              Left err -> do
                logTagError "recalculateFareForDistance" $ "Failed to recalculate fare : " <> show err
                return (fromMaybe 0 booking.estimatedDistance, booking.estimatedFare, Nothing)
              Right response -> return response

          pure (chargeableDistance, finalFare, mbUpdatedFareParams, rideOld, Nothing, Nothing)
        _ -> do
          withFallback (fromMaybe 0 booking.estimatedDistance, booking.estimatedFare, Nothing, rideOld, Nothing, Nothing) $ do
            if DTC.isOdometerReadingsRequired booking.tripCategory
              then do
                case mbOdometer of
                  Just odometer -> do
                    unless (odometer.value >= maybe 0 (.value) rideOld.startOdometerReading) $ throwError InvalidEndOdometerReading
                    let odometerCalculatedDistance = Meters $ round (odometer.value - maybe 0 (.value) rideOld.startOdometerReading) * 1000
                    (recalcDistance, finalFare, mbUpdatedFareParams) <- recalculateFareForDistance handle booking rideOld odometerCalculatedDistance thresholdConfig False tripEndPoint
                    pure (recalcDistance, finalFare, mbUpdatedFareParams, rideOld, Nothing, Nothing)
                  Nothing -> throwError $ OdometerReadingRequired (show booking.tripCategory)
              else do
                -- here we update the current ride, so below we fetch the updated version
                pickupDropOutsideOfThreshold <- isPickupDropOutsideOfThreshold booking rideOld tripEndPoint thresholdConfig
                whenJust (nonEmpty tripEndPoints) \tripEndPoints' -> do
                  rectificationMapsConfig <-
                    if shouldRectifyDistantPointsSnapToRoadFailure
                      then Just <$> TM.getServiceConfigForRectifyingSnapToRoadDistantPointsFailure booking.providerId booking.merchantOperatingCityId
                      else pure Nothing
                  let passedThroughDrop = any (isDropInsideThreshold booking thresholdConfig) tripEndPoints'
                  logDebug $ "Did we passed through drop yet in endRide" <> show passedThroughDrop <> " " <> show tripEndPoints'
                  withTimeAPI "endRide" "finalDistanceCalculation" $ finalDistanceCalculation rectificationMapsConfig (DTC.isTollApplicableForTrip booking.vehicleServiceTier booking.tripCategory) thresholdConfig.enableTollCrossedNotifications rideOld.id driverId tripEndPoints' estimatedDistance estimatedTollCharges estimatedTollNames pickupDropOutsideOfThreshold passedThroughDrop

                updRide <- findRideById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)

                distanceCalculationFailed <- withTimeAPI "endRide" "isDistanceCalculationFailed" $ isDistanceCalculationFailed driverId

                when distanceCalculationFailed $ do
                  logWarning $ "Failed to calculate distance for this ride: " <> rideId.getId

                let (tollCharges, tollNames, tollConfidence) = do
                      let distanceCalculationFailure = distanceCalculationFailed || (maybe False (> 0) updRide.numberOfSelfTuned)
                      if distanceCalculationFailure
                        then
                          if isJust updRide.estimatedTollCharges
                            then
                              if updRide.estimatedTollCharges == Just 0
                                then (Nothing, Nothing, Nothing)
                                else
                                  if isJust updRide.tollCharges
                                    then (updRide.tollCharges, updRide.tollNames, Just Neutral)
                                    else
                                      if updRide.driverDeviatedToTollRoute == Just True
                                        then (updRide.estimatedTollCharges, updRide.estimatedTollNames, Just Neutral)
                                        else (Nothing, Nothing, Just Unsure)
                            else (Nothing, Nothing, Nothing)
                        else (updRide.tollCharges, updRide.tollNames, Just Sure)

                fork "ride-interpolation" $ do
                  interpolatedPoints <- getInterpolatedPoints updRide.driverId
                  let rideInterpolationData = RideInterpolationData {interpolatedPoints = interpolatedPoints, rideId = updRide.id}
                  when (isJust updRide.driverDeviatedToTollRoute && tollConfidence == Just Sure && ((maybe True (== 0) tollCharges && isJust updRide.estimatedTollCharges) || fromMaybe False (((,) <$> tollCharges <*> updRide.estimatedTollCharges) <&> \(tollCharges', estimatedTollCharges') -> tollCharges' /= estimatedTollCharges'))) $ pushToKafka rideInterpolationData "ride-interpolated-waypoints" updRide.id.getId

                let ride = updRide{tollCharges = tollCharges, tollNames = tollNames, tollConfidence = tollConfidence, distanceCalculationFailed = Just distanceCalculationFailed}

                (chargeableDistance, finalFare, mbUpdatedFareParams) <-
                  if shouldRectifyDistantPointsSnapToRoadFailure
                    then recalculateFareForDistance handle booking ride (fromMaybe (roundToIntegral ride.traveledDistance) (bool (Just $ roundToIntegral ride.traveledDistance) booking.estimatedDistance distanceCalculationFailed)) thresholdConfig False tripEndPoint
                    else
                      if distanceCalculationFailed
                        then calculateFinalValuesForFailedDistanceCalculations handle booking ride tripEndPoint pickupDropOutsideOfThreshold thresholdConfig
                        else calculateFinalValuesForCorrectDistanceCalculations handle booking ride booking.maxEstimatedDistance pickupDropOutsideOfThreshold thresholdConfig tripEndPoint
                pure (chargeableDistance, finalFare, mbUpdatedFareParams, ride, Just pickupDropOutsideOfThreshold, Just distanceCalculationFailed)
    let newFareParams = fromMaybe booking.fareParams mbUpdatedFareParams
    clearEditDestinationWayAndSnappedPointsFork <- awaitableFork "endRide->clearEditDestinationWayAndSnappedPoints" $ withTimeAPI "endRide" "clearEditDestinationWayAndSnappedPoints" $ clearEditDestinationWayAndSnappedPoints driverId
    let updRide' =
          ride{tripEndTime = Just now,
               chargeableDistance = Just chargeableDistance,
               fare = Just finalFare,
               status = DRide.COMPLETED,
               tripEndPos = Just tripEndPoint,
               rideEndedBy = Just rideEndedBy',
               fareParametersId = Just newFareParams.id,
               tollCharges = mbUpdatedFareParams >>= (.tollCharges),
               distanceCalculationFailed = distanceCalculationFailed,
               pickupDropOutsideOfThreshold = pickupDropOutsideOfThreshold,
               endOdometerReading = mbOdometer
              }
    newRideTags <- try @_ @SomeException (Yudhishthira.computeNammaTags Yudhishthira.RideEnd (Y.EndRideTagData updRide' booking))
    let updRide = updRide' {DRide.rideTags = ride.rideTags <> eitherToMaybe newRideTags}
    fork "updating time and latlong in advance ride if any" $ do
      whenJust advanceRide $ \advanceRide' -> do
        QRide.updatePreviousRideTripEndPosAndTime (Just tripEndPoint) (Just now) advanceRide'.id

    -- we need to store fareParams only when they changed
    endRideTransactionFork <- awaitableFork "endRide->endRideTransaction" $ withTimeAPI "endRide" "endRideTransaction" $ endRideTransaction (cast @DP.Person @DP.Driver driverId) booking updRide mbUpdatedFareParams booking.riderId newFareParams thresholdConfig
    clearInterpolatedPointsFork <- awaitableFork "endRide->clearInterpolatedPoints" $ withTimeAPI "endRide" "clearInterpolatedPoints" $ clearInterpolatedPoints driverId

    logDebug $ "RideCompleted Coin Event" <> show chargeableDistance
    fork "DriverRideCompletedCoin Event : " $ do
      expirationPeriod <- DC.getExpirationSeconds thresholdConfig.timeDiffFromUtc
      let validRideTaken = isValidRide updRide
          metroRideType = determineMetroRideType booking.specialLocationTag "SureMetro" "SureWarriorMetro"
      logDebug $ "MetroRideType : " <> show metroRideType
      when (DCT.isMetroRideType metroRideType && validRideTaken) $ do
        DC.incrementMetroRideCount driverId metroRideType expirationPeriod 1
      when (DTC.isDynamicOfferTrip booking.tripCategory && validRideTaken) $ do
        DC.incrementValidRideCount driverId expirationPeriod 1
        DC.driverCoinsEvent driverId booking.providerId booking.merchantOperatingCityId (DCT.EndRide (isJust booking.disabilityTag) updRide metroRideType) (Just ride.id.getId) ride.vehicleVariant

    mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId -> do
      findPaymentMethodByIdAndMerchantId paymentMethodId booking.merchantOperatingCityId
        >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
    let mbPaymentMethodInfo = DMPM.mkPaymentMethodInfo <$> mbPaymentMethod
    notifyCompleteToBAPFork <- awaitableFork "endRide->notifyCompleteToBAP" $ withTimeAPI "endRide" "notifyCompleteToBAP" $ notifyCompleteToBAP booking updRide newFareParams mbPaymentMethodInfo Nothing (Just tripEndPoint)
    fork "sending dashboardSMS - CallbasedEndRide " $ do
      case req of
        CallBasedReq callBasedEndRideReq -> do
          let requestor = callBasedEndRideReq.requestor
          sendDashboardSms requestor.merchantId booking.merchantOperatingCityId Sms.ENDRIDE (Just ride) driverId (Just booking) finalFare
        _ -> pure ()

    awaitAll [clearEditDestinationWayAndSnappedPointsFork, endRideTransactionFork, clearInterpolatedPointsFork, notifyCompleteToBAPFork]
  return $ EndRideResp {result = "Success", homeLocationReached = homeLocationReached'}
  where
    clearEditDestinationWayAndSnappedPoints driverId = LocUpdInternal.deleteEditDestinationSnappedWaypoints driverId >> LocUpdInternal.deleteEditDestinationWaypoints driverId
    awaitAll = mapM_ (L.await Nothing)
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

determineMetroRideType :: Maybe Text -> Text -> Text -> DCT.MetroRideType
determineMetroRideType mbSplLocTag sureMetro sureWarriorMetro =
  case mbSplLocTag of
    Just splLocTag ->
      case (fromMetro, toMetro, priorityTag) of
        (True, True, Just "PriorityPickup") -> DCT.FromMetro
        (True, True, Just "PriorityDrop") -> DCT.ToMetro
        (True, _, Just "PriorityPickup") -> DCT.FromMetro
        (_, True, Just "PriorityDrop") -> DCT.ToMetro
        (True, _, _) -> DCT.FromMetro
        (_, True, _) -> DCT.ToMetro
        _ -> DCT.None
      where
        tagArr = Text.splitOn "_" splLocTag
        sourceTag = tagArr BODUC.!? 0
        destTag = tagArr BODUC.!? 1
        priorityTag = tagArr BODUC.!? 2
        fromMetro = sourceTag == Just sureMetro || sourceTag == Just sureWarriorMetro
        toMetro = destTag == Just sureMetro || destTag == Just sureWarriorMetro
    Nothing -> DCT.None

recalculateFareForDistance :: (MonadThrow m, Log m, MonadTime m, MonadGuid m, EsqDBFlow m r, CacheFlow m r) => ServiceHandle m -> SRB.Booking -> DRide.Ride -> Meters -> DTConf.TransporterConfig -> Bool -> LatLong -> m (Meters, HighPrecMoney, Maybe FareParameters)
recalculateFareForDistance ServiceHandle {..} booking ride recalcDistance' thresholdConfig recomputeWithLatestPricing tripEndPoint = do
  tripEndTime <- getCurrentTime
  let merchantId = booking.providerId
      oldDistance = fromMaybe 0 booking.estimatedDistance -- TODO: Fix later with rentals
  passedThroughDrop <- LocUpd.isPassedThroughDrop ride.driverId
  let actualDuration = ride.tripStartTime <&> \startTime -> roundToIntegral $ diffUTCTime tripEndTime startTime
  pickupDropOutsideOfThreshold <- isDropOutsideOfThreshold booking tripEndPoint thresholdConfig
  QRide.updatePassedThroughDestination ride.id passedThroughDrop
  let tripCategoryForNoRecalc = [DTC.OneWay DTC.OneWayRideOtp, DTC.OneWay DTC.OneWayOnDemandDynamicOffer]
      (recalcDistance, finalDuration) = bool (recalcDistance', actualDuration) (oldDistance, booking.estimatedDuration) (passedThroughDrop && pickupDropOutsideOfThreshold && booking.tripCategory `elem` tripCategoryForNoRecalc && ride.distanceCalculationFailed == Just False && maybe True (oldDistance >) thresholdConfig.minThresholdForPassThroughDestination)
  let estimatedFare = Fare.fareSum booking.fareParams
      destinationWaitingTime = fromMaybe 0 $ if isNothing ride.destinationReachedAt || (not $ isUnloadingTimeRequired booking.vehicleServiceTier) then Nothing else fmap (max 0) (secondsToMinutes . roundToIntegral <$> (diffUTCTime <$> ride.tripEndTime <*> ride.destinationReachedAt))
  vehicleAge <-
    if DTC.isAmbulanceTrip booking.tripCategory
      then do
        rideDetail <- QRD.findById ride.id -- replica?
        pure $ (.vehicleAge) =<< rideDetail
      else pure Nothing
  farePolicy <-
    if recomputeWithLatestPricing
      then getFarePolicyOnEndRide (Just $ getCoordinates booking.fromLocation) booking.fromLocGeohash booking.toLocGeohash (Just recalcDistance) finalDuration (getCoordinates tripEndPoint) booking.merchantOperatingCityId booking.tripCategory booking.vehicleServiceTier booking.area booking.quoteId (Just booking.startTime) (Just booking.isDashboardRequest) booking.dynamicPricingLogicVersion (Just (TransactionId (Id booking.transactionId)))
      else getFarePolicyByEstOrQuoteId (Just $ getCoordinates booking.fromLocation) booking.fromLocGeohash booking.toLocGeohash (Just recalcDistance) finalDuration booking.merchantOperatingCityId booking.tripCategory booking.vehicleServiceTier booking.area booking.quoteId (Just booking.startTime) (Just booking.isDashboardRequest) booking.dynamicPricingLogicVersion (Just (TransactionId (Id booking.transactionId)))
  if farePolicy.disableRecompute == Just True
    then return (fromMaybe 0 booking.estimatedDistance, booking.estimatedFare, Nothing)
    else do
      stopsInfo <- if fromMaybe False ride.hasStops then QSI.findAllByRideId ride.id else return []
      fareParams <-
        calculateFareParameters
          Fare.CalculateFareParametersParams
            { farePolicy = farePolicy,
              actualDistance = Just recalcDistance,
              estimatedDistance = Just oldDistance,
              rideTime = booking.startTime,
              returnTime = booking.returnTime,
              roundTrip = fromMaybe False booking.roundTrip,
              waitingTime = fmap (destinationWaitingTime +) $ if isNothing ride.driverArrivalTime then Nothing else fmap (max 0) (secondsToMinutes . roundToIntegral <$> (diffUTCTime <$> ride.tripStartTime <*> (liftA2 max ride.driverArrivalTime (Just booking.startTime)))),
              stopWaitingTimes = stopsInfo <&> (\stopInfo -> max 0 (secondsToMinutes $ roundToIntegral (diffUTCTime (fromMaybe stopInfo.waitingTimeStart stopInfo.waitingTimeEnd) stopInfo.waitingTimeStart))),
              actualRideDuration = finalDuration,
              estimatedRideDuration = booking.estimatedDuration,
              avgSpeedOfVehicle = thresholdConfig.avgSpeedOfVehicle,
              driverSelectedFare = booking.fareParams.driverSelectedFare,
              customerExtraFee = booking.fareParams.customerExtraFee,
              nightShiftCharge = booking.fareParams.nightShiftCharge,
              estimatedCongestionCharge = booking.estimatedCongestionCharge,
              customerCancellationDues = booking.fareParams.customerCancellationDues,
              nightShiftOverlapChecking = DTC.isFixedNightCharge booking.tripCategory,
              timeDiffFromUtc = Just thresholdConfig.timeDiffFromUtc,
              tollCharges = ride.tollCharges,
              vehicleAge = vehicleAge,
              currency = booking.currency,
              noOfStops = length ride.stops,
              distanceUnit = booking.distanceUnit,
              merchantOperatingCityId = Just booking.merchantOperatingCityId
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

isDropOutsideOfThreshold :: (MonadThrow m, Log m, MonadTime m, MonadGuid m) => SRB.Booking -> LatLong -> DTConf.TransporterConfig -> m Bool
isDropOutsideOfThreshold booking tripEndPoint thresholdConfig = do
  let dropLocThreshold = metersToHighPrecMeters thresholdConfig.dropLocThreshold
  let dropDifference = maybe 0 (\toLocation -> abs $ distanceBetweenInMeters (getCoordinates toLocation) tripEndPoint) booking.toLocation
  pure $ dropDifference >= dropLocThreshold

getDistanceDiff :: (MonadThrow m, Log m, MonadTime m, MonadGuid m) => SRB.Booking -> Meters -> m HighPrecMeters
getDistanceDiff booking distance = do
  let rideDistanceDifference = distance - (fromMaybe 0 booking.estimatedDistance) -- TODO: Fix with rentals
  logTagInfo "RideDistance differences" $
    "Distance Difference: "
      <> show rideDistanceDifference
  pure $ metersToHighPrecMeters rideDistanceDifference

calculateFinalValuesForCorrectDistanceCalculations ::
  (MonadFlow m, MonadThrow m, Log m, MonadTime m, MonadGuid m, EsqDBFlow m r, CacheFlow m r) => ServiceHandle m -> SRB.Booking -> DRide.Ride -> Maybe HighPrecMeters -> Bool -> DTConf.TransporterConfig -> LatLong -> m (Meters, HighPrecMoney, Maybe FareParameters)
calculateFinalValuesForCorrectDistanceCalculations handle booking ride mbMaxDistance pickupDropOutsideOfThreshold thresholdConfig tripEndPoint = do
  distanceDiff <- getDistanceDiff booking (highPrecMetersToMeters ride.traveledDistance)
  let estimatedDistance = fromMaybe 0 booking.estimatedDistance -- TODO: Fix with rentals
  shouldRecompute <- shouldUpwardRecompute thresholdConfig estimatedDistance (highPrecMetersToMeters distanceDiff)
  let thresholdChecks = thresholdConfig.recomputeIfPickupDropNotOutsideOfThreshold && shouldRecompute
  (mbDailyExtraKms, mbWeeklyExtraKms) <- if thresholdChecks then handleExtraKmsRecomputation distanceDiff else return (Nothing, Nothing)
  fork "Send Extra Kms Limit Exceeded Overlay" $
    when (thresholdConfig.toNotifyDriverForExtraKmsLimitExceed && not (checkExtraKmsThreshold mbDailyExtraKms mbWeeklyExtraKms)) notifyDriverOnExtraKmsLimitExceed
  let maxDistance = fromMaybe ride.traveledDistance mbMaxDistance + maxUpwardBuffer
  if not pickupDropOutsideOfThreshold
    then
      if thresholdChecks && checkExtraKmsThreshold mbDailyExtraKms mbWeeklyExtraKms
        then recalculateFareForDistance handle booking ride (roundToIntegral $ min ride.traveledDistance maxDistance) thresholdConfig False tripEndPoint
        else recalculateFareForDistance handle booking ride estimatedDistance thresholdConfig False tripEndPoint
    else
      if distanceDiff < 0
        then recalculateFareForDistance handle booking ride (roundToIntegral ride.traveledDistance) thresholdConfig True tripEndPoint
        else
          if distanceDiff < thresholdConfig.actualRideDistanceDiffThreshold
            then recalculateFareForDistance handle booking ride estimatedDistance thresholdConfig True tripEndPoint
            else recalculateFareForDistance handle booking ride (roundToIntegral ride.traveledDistance) thresholdConfig True tripEndPoint
  where
    maxUpwardBuffer = case (booking.estimatedDistance, thresholdConfig.upwardsRecomputeBufferPercentage) of
      (Just estDistance, Just percentage) -> HighPrecMeters (max (fromRational $ (toRational estDistance.getMeters) * (toRational percentage / 100)) thresholdConfig.upwardsRecomputeBuffer.getHighPrecMeters)
      _ -> thresholdConfig.upwardsRecomputeBuffer
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
      overlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory booking.merchantOperatingCityId "EXTRA_KMS_LIMIT_EXCEEDED" (fromMaybe ENGLISH driver.language) Nothing Nothing >>= fromMaybeM (OverlayKeyNotFound "EXTRA_KMS_LIMIT_EXCEEDED")
      TN.sendOverlay booking.merchantOperatingCityId driver $ TN.mkOverlayReq overlay

calculateFinalValuesForFailedDistanceCalculations ::
  (MonadThrow m, Log m, MonadTime m, MonadGuid m, EsqDBFlow m r, CacheFlow m r) => ServiceHandle m -> SRB.Booking -> DRide.Ride -> LatLong -> Bool -> DTConf.TransporterConfig -> m (Meters, HighPrecMoney, Maybe FareParameters)
calculateFinalValuesForFailedDistanceCalculations handle@ServiceHandle {..} booking ride tripEndPoint pickupDropOutsideOfThreshold thresholdConfig = do
  let tripStartPoint = case ride.tripStartPos of
        Nothing -> getCoordinates booking.fromLocation
        Just tripStartPos -> tripStartPos
  interpolatedPoints <- getInterpolatedPoints ride.driverId
  let estimatedDistance = fromMaybe 0 booking.estimatedDistance -- TODO: Fix with rentals
  if not pickupDropOutsideOfThreshold
    then recalculateFareForDistance handle booking ride estimatedDistance thresholdConfig False tripEndPoint -- TODO: Fix with rentals
    else do
      (_routePoints, approxTraveledDistance) <- getRouteAndDistanceBetweenPoints tripStartPoint tripEndPoint interpolatedPoints estimatedDistance
      logTagInfo "endRide" $ "approxTraveledDistance when pickup and drop are not outside threshold: " <> show approxTraveledDistance
      distanceDiff <- getDistanceDiff booking approxTraveledDistance
      shouldRecompute <- shouldUpwardRecompute thresholdConfig estimatedDistance (highPrecMetersToMeters distanceDiff)
      if distanceDiff < 0
        then do
          recalculateFareForDistance handle booking ride approxTraveledDistance thresholdConfig True tripEndPoint -- TODO :: Recompute Toll Charges Here ?
        else
          if distanceDiff < thresholdConfig.actualRideDistanceDiffThreshold
            then do
              recalculateFareForDistance handle booking ride estimatedDistance thresholdConfig True tripEndPoint
            else do
              if highPrecMetersToMeters distanceDiff < maxDistance && shouldRecompute
                then recalculateFareForDistance handle booking ride approxTraveledDistance thresholdConfig True tripEndPoint -- TODO :: Recompute Toll Charges Here ?
                else do
                  logTagInfo "Inaccurate Location Updates and Pickup/Drop Deviated." ("DistanceDiff: " <> show distanceDiff)
                  recalculateFareForDistance handle booking ride (estimatedDistance + maxDistance) thresholdConfig True tripEndPoint
  where
    maxDistance = case (booking.estimatedDistance, thresholdConfig.upwardsRecomputeBufferPercentage) of
      (Just estDistance, Just percentage) -> Meters $ max (round $ (toRational estDistance.getMeters) * (toRational percentage / 100)) (round thresholdConfig.upwardsRecomputeBuffer.getHighPrecMeters)
      _ -> highPrecMetersToMeters thresholdConfig.upwardsRecomputeBuffer

shouldUpwardRecompute :: (MonadFlow m, MonadThrow m, Log m) => DTConf.TransporterConfig -> Meters -> Meters -> m Bool
shouldUpwardRecompute thresholdConfig estimatedDistance distanceDiff = do
  let filteredThresholds = maybe [] (filter (\distanceThreshold -> distanceThreshold.estimatedDistanceUpper > estimatedDistance)) thresholdConfig.recomputeDistanceThresholds
      recomputeDistanceThreshold = listToMaybe $ sortBy (comparing \distanceThreshold -> distanceThreshold.estimatedDistanceUpper - estimatedDistance) filteredThresholds
  case recomputeDistanceThreshold of
    Just distanceThreshold -> do
      let shouldRecompute = distanceDiff > distanceThreshold.minThresholdDistance && distanceDiff.getMeters > (estimatedDistance.getMeters * distanceThreshold.minThresholdPercentage) `div` 100
      pure shouldRecompute
    Nothing -> pure False

isUnloadingTimeRequired :: DVST.ServiceTierType -> Bool
isUnloadingTimeRequired str =
  str
    `elem` [ DVST.DELIVERY_LIGHT_GOODS_VEHICLE,
             DVST.DELIVERY_TRUCK_MINI,
             DVST.DELIVERY_TRUCK_SMALL,
             DVST.DELIVERY_TRUCK_MEDIUM,
             DVST.DELIVERY_TRUCK_LARGE,
             DVST.DELIVERY_TRUCK_ULTRA_LARGE
           ]
