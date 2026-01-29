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
import qualified Data.Geohash as Geohash
import qualified Data.HashMap.Strict as HM
import Data.Maybe (listToMaybe)
import Data.OpenApi.Internal.Schema (ToSchema)
import qualified Data.Text as Text
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Action.Internal.ViolationDetection as VID
import qualified Domain.Action.UI.Ride.Common as DUIRideCommon
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
import Kernel.Beam.Functions (runInMasterDbAndRedis)
import Kernel.Beam.Lib.Utils (pushToKafka)
import Kernel.External.Encryption (decrypt)
import Kernel.External.Maps
import qualified Kernel.External.Maps.Interface.Types as Maps
import qualified Kernel.External.Maps.Types as Maps
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude (roundToIntegral)
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
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
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types as Yudhishthira
import qualified SharedLogic.BehaviourManagement.GpsTollBehavior as GpsTollBehavior
import qualified SharedLogic.CallBAP as CallBAP
import qualified SharedLogic.CallInternalMLPricing as ML
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.FareCalculator as Fare
import qualified SharedLogic.FareCalculatorV2 as FareV2
import qualified SharedLogic.FarePolicy as FarePolicy
import qualified SharedLogic.FarePolicy as SFP
import qualified SharedLogic.MerchantPaymentMethod as DMPM
import SharedLogic.RuleBasedTierUpgrade
import qualified SharedLogic.StateEntryPermitDetector as SEPD
import qualified SharedLogic.TollsDetector as TollsDetector
import qualified SharedLogic.Type as SLT
import qualified Storage.Cac.GoHomeConfig as CGHC
import qualified Storage.Cac.TransporterConfig as QTC
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Merchant as MerchantS
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QRB
import Storage.Queries.DriverGoHomeRequest as QDGR
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRD
import qualified Storage.Queries.StopInformation as QSI
import Tools.DynamicLogic (getAppDynamicLogic)
import Tools.Error
import qualified Tools.Maps as TM
import qualified Tools.Notifications as TN
import qualified Tools.SMS as Sms
import Tools.Utils
import Utils.Common.Cac.KeyNameConstants

data EndRideReq = DriverReq DriverEndRideReq | DashboardReq DashboardEndRideReq | CallBasedReq CallBasedEndRideReq | CronJobReq CronJobEndRideReq

data EndRideResp = EndRideResp
  { result :: Text,
    homeLocationReached :: Maybe Bool,
    driverRideRes :: Maybe DUIRideCommon.DriverRideRes
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data DriverEndRideReq = DriverEndRideReq
  { endRideOtp :: Maybe Text,
    point :: LatLong,
    requestor :: DP.Person,
    uiDistanceCalculationWithAccuracy :: Maybe Int,
    uiDistanceCalculationWithoutAccuracy :: Maybe Int,
    odometer :: Maybe DRide.OdometerReading,
    driverGpsTurnedOff :: Maybe Bool
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
    getFarePolicyByEstOrQuoteId :: Maybe LatLong -> Maybe LatLong -> Maybe Text -> Maybe Text -> Maybe Meters -> Maybe Seconds -> Id DMOC.MerchantOperatingCity -> DTC.TripCategory -> DVST.ServiceTierType -> Maybe SL.Area -> Text -> Maybe UTCTime -> Maybe Bool -> Maybe Int -> Maybe CacKey -> [LYT.ConfigVersionMap] -> m DFP.FullFarePolicy,
    getFarePolicyOnEndRide :: Maybe LatLong -> Maybe LatLong -> Maybe Text -> Maybe Text -> Maybe Meters -> Maybe Seconds -> LatLong -> Id DMOC.MerchantOperatingCity -> DTC.TripCategory -> DVST.ServiceTierType -> Maybe SL.Area -> Text -> Maybe UTCTime -> Maybe Bool -> Maybe Int -> Maybe CacKey -> [LYT.ConfigVersionMap] -> m DFP.FullFarePolicy,
    calculateFareParameters :: Fare.CalculateFareParametersParams -> m Fare.FareParameters,
    putDiffMetric :: Id DM.Merchant -> HighPrecMoney -> Meters -> m (),
    isDistanceCalculationFailed :: Id DP.Person -> m Bool,
    finalDistanceCalculation :: Maybe MapsServiceConfig -> Bool -> Bool -> Id DRide.Ride -> Id DP.Person -> NonEmpty LatLong -> Meters -> Maybe HighPrecMoney -> Maybe [Text] -> Maybe [Text] -> Bool -> Bool -> Bool -> m (),
    getInterpolatedPoints :: Id DP.Person -> m [LatLong],
    clearInterpolatedPoints :: Id DP.Person -> m (),
    findConfig :: Maybe CacKey -> m (Maybe DTConf.TransporterConfig),
    whenWithLocationUpdatesLock :: forall a. Id DP.Person -> m a -> m a,
    getRouteAndDistanceBetweenPoints :: LatLong -> LatLong -> [LatLong] -> Meters -> m ([LatLong], Meters),
    findPaymentMethodByIdAndMerchantId :: Id DMPM.MerchantPaymentMethod -> Id DMOC.MerchantOperatingCity -> m (Maybe DMPM.MerchantPaymentMethod),
    sendDashboardSms :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Sms.DashboardMessageType -> Maybe DRide.Ride -> Id DP.Person -> Maybe SRB.Booking -> HighPrecMoney -> m (),
    uiDistanceCalculation :: Id DRide.Ride -> Maybe Int -> Maybe Int -> m ()
  }

buildEndRideHandle :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe (Id DRide.Ride) -> Flow (ServiceHandle Flow)
buildEndRideHandle merchantId merchantOpCityId rideId = do
  defaultRideInterpolationHandler <- LocUpd.buildRideInterpolationHandler merchantId merchantOpCityId rideId True Nothing
  return $
    ServiceHandle
      { findBookingById = QRB.findById,
        findRideById = QRide.findById,
        getMerchant = MerchantS.findById,
        notifyCompleteToBAP = CallBAP.sendRideCompletedUpdateToBAP,
        endRideTransaction = RideEndInt.endRideTransaction,
        getFarePolicyByEstOrQuoteId = FarePolicy.getFarePolicyByEstOrQuoteId,
        getFarePolicyOnEndRide = FarePolicy.getFarePolicyOnEndRide,
        calculateFareParameters = FareV2.calculateFareParametersV2,
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

-- Helper function to get driver number from Person record
getDriverNumberFromPerson :: EncFlow m r => DP.Person -> m (Maybe Text)
getDriverNumberFromPerson person = do
  decMobileNumber <- mapM decrypt person.mobileNumber
  return $ person.mobileCountryCode <> decMobileNumber

-- Helper function to get driver number either from request or by fetching Person record
getDriverNumberFromRequest :: (EsqDBFlow m r, CacheFlow m r, EncFlow m r) => EndRideReq -> Id DP.Person -> m (Maybe Text)
getDriverNumberFromRequest req driverId =
  case req of
    DriverReq driverReq -> getDriverNumberFromPerson driverReq.requestor
    CallBasedReq callBasedReq -> getDriverNumberFromPerson callBasedReq.requestor
    _ -> do
      -- For dashboard and cron job requests, we need to fetch the driver's Person record
      driver <- QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      getDriverNumberFromPerson driver

type EndRideFlow m r =
  ( MonadFlow m,
    CoreMetrics m,
    MonadReader r m,
    HasField "enableAPILatencyLogging" r Bool,
    HasField "enableAPIPrometheusMetricLogging" r Bool,
    LT.HasLocationService m r,
    HasKafkaProducer r
  )

driverEndRide ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c, HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal], HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  DriverEndRideReq ->
  m EndRideResp
driverEndRide handle rideId req = do
  withLogTag ("requestorId-" <> req.requestor.id.getId)
    . endRide handle rideId
    $ DriverReq req

callBasedEndRide ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c, HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal], HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  CallBasedEndRideReq ->
  m EndRideResp
callBasedEndRide handle rideId = endRide handle rideId . CallBasedReq

dashboardEndRide ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c, HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal], HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) =>
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
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c, HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal], HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) =>
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
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c, HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal], HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) =>
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
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c, HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal], HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  EndRideReq ->
  m EndRideResp
endRideHandler handle@ServiceHandle {..} rideId req = do
  rideOld <- runInMasterDbAndRedis $ findRideById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
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
      whenJust driverReq.driverGpsTurnedOff $ \gpsTurnedOff ->
        when (gpsTurnedOff && not (fromMaybe False rideOld.driverGpsTurnedOff)) $ void $ QRide.updateDriverGpsTurnedOff (Just True) rideOld.id
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
                routesResp <- DMaps.getTripRoutes (driverId, booking.providerId, booking.merchantOperatingCityId) (Just rideId.getId) (buildRoutesReq tripEndPoint driverHomeLocation)
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

  finalUpdatedRide <- whenWithLocationUpdatesLock driverId $ do
    now <- getCurrentTime
    thresholdConfig <- findConfig (Just (TransactionId (Id booking.transactionId))) >>= fromMaybeM (InternalError "TransportConfigNotFound")
    let estimatedDistance = fromMaybe 0 booking.estimatedDistance -- TODO: Fix later with rentals
        estimatedTollCharges = rideOld.estimatedTollCharges
        estimatedTollNames = rideOld.estimatedTollNames
        estimatedTollIds = rideOld.estimatedTollIds
        shouldRectifyDistantPointsSnapToRoadFailure = DTC.shouldRectifyDistantPointsSnapToRoadFailure booking.tripCategory
    advanceRide <- runInMasterDbAndRedis $ QRide.getActiveAdvancedRideByDriverId driverId
    tripEndPoints <- do
      let mbAdvanceRideId = (.id) <$> advanceRide
      res <- LF.rideEnd rideId tripEndPoint.lat tripEndPoint.lon booking.providerId driverId mbAdvanceRideId Nothing
      pure $ toList res.loc
    (chargeableDistance, finalFare, mbUpdatedFareParams, ride, pickupDropOutsideOfThreshold, distanceCalculationFailed) <-
      case req of
        CronJobReq _ -> do
          logTagInfo "cron job -> endRide : " "Do not call snapToRoad, return estimates as final values."
          res <- withTryCatch "recalculateFareForDistance:endRideHandler" $ recalculateFareForDistance handle booking rideOld estimatedDistance thresholdConfig False tripEndPoint
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
                  withTimeAPI "endRide" "finalDistanceCalculation" $ finalDistanceCalculation rectificationMapsConfig (DTC.isTollApplicableForTrip booking.vehicleServiceTier booking.tripCategory) thresholdConfig.enableTollCrossedNotifications rideOld.id driverId tripEndPoints' estimatedDistance estimatedTollCharges estimatedTollNames estimatedTollIds pickupDropOutsideOfThreshold passedThroughDrop (booking.tripCategory == DTC.OneWay DTC.MeterRide)

                updRide <- runInMasterDbAndRedis $ findRideById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)

                distanceCalculationFailed <- withTimeAPI "endRide" "isDistanceCalculationFailed" $ isDistanceCalculationFailed driverId

                when distanceCalculationFailed $ do
                  logWarning $ "Failed to calculate distance for this ride: " <> rideId.getId

                -- Check for pending tolls (entry detected but exit not found) and validate against estimate using IDs
                mbValidatedPendingToll <-
                  TollsDetector.checkAndValidatePendingTolls
                    updRide.driverId
                    updRide.estimatedTollCharges
                    updRide.estimatedTollNames
                    updRide.estimatedTollIds
                    updRide.tollCharges
                    updRide.tollIds

                -- Log if we have validated toll but can't apply due to route deviation
                when (isJust mbValidatedPendingToll && pickupDropOutsideOfThreshold) $ do
                  logWarning $ "Validated pending toll found but NOT applying due to pickup/drop outside threshold. RideId: " <> rideId.getId

                let (tollCharges, tollNames, tollIds, tollConfidence) = do
                      let distanceCalculationFailure = distanceCalculationFailed || (maybe False (> 0) updRide.numberOfSelfTuned)
                          -- Only apply validated pending toll if pickup/drop is within threshold (route was as expected)
                          canApplyValidatedPendingToll = not pickupDropOutsideOfThreshold
                      if distanceCalculationFailure
                        then
                          if isJust updRide.estimatedTollCharges
                            then
                              if updRide.estimatedTollCharges == Just 0
                                then (Nothing, Nothing, Nothing, Nothing)
                                else
                                  if isJust updRide.tollCharges
                                    then case (canApplyValidatedPendingToll, mbValidatedPendingToll) of
                                      (True, Just (pendingCharges, pendingNames, pendingIds)) ->
                                        -- Some detected + some pending (same as distance calc success case)
                                        let combinedCharges = fromMaybe 0 updRide.tollCharges + pendingCharges
                                            combinedNames = fromMaybe [] updRide.tollNames <> pendingNames
                                            combinedIds = fromMaybe [] updRide.tollIds <> pendingIds
                                         in (Just combinedCharges, Just combinedNames, Just combinedIds, Just Neutral)
                                      _ ->
                                        -- No pending tolls or route deviated
                                        (updRide.tollCharges, updRide.tollNames, updRide.tollIds, Just Neutral)
                                    else
                                      if updRide.driverDeviatedToTollRoute == Just True
                                        then (updRide.estimatedTollCharges, updRide.estimatedTollNames, updRide.estimatedTollIds, Just Neutral)
                                        else case (canApplyValidatedPendingToll, mbValidatedPendingToll) of
                                          (True, Just (pendingCharges, pendingNames, pendingIds)) ->
                                            -- Combine detected + pending tolls
                                            let combinedCharges = fromMaybe 0 updRide.tollCharges + pendingCharges
                                                combinedNames = fromMaybe [] updRide.tollNames <> pendingNames
                                                combinedIds = fromMaybe [] updRide.tollIds <> pendingIds
                                             in (Just combinedCharges, Just combinedNames, Just combinedIds, Just Neutral)
                                          _ -> (updRide.tollCharges, updRide.tollNames, updRide.tollIds, Just Unsure)
                            else case (canApplyValidatedPendingToll, mbValidatedPendingToll) of
                              (True, Just (pendingCharges, pendingNames, pendingIds)) ->
                                (Just pendingCharges, Just pendingNames, Just pendingIds, Just Unsure)
                              _ -> (updRide.tollCharges, updRide.tollNames, updRide.tollIds, Nothing)
                        else case (updRide.tollCharges, canApplyValidatedPendingToll, mbValidatedPendingToll) of
                          (Just charges, _, Nothing) ->
                            (Just charges, updRide.tollNames, updRide.tollIds, Just Sure)
                          (Just charges, True, Just (pendingCharges, pendingNames, pendingIds)) ->
                            -- Some detected + some pending
                            let combinedCharges = charges + pendingCharges
                                combinedNames = fromMaybe [] updRide.tollNames <> pendingNames
                                combinedIds = fromMaybe [] updRide.tollIds <> pendingIds
                             in (Just combinedCharges, Just combinedNames, Just combinedIds, Just Neutral)
                          (Nothing, True, Just (pendingCharges, pendingNames, pendingIds)) ->
                            (Just pendingCharges, Just pendingNames, Just pendingIds, Just Neutral)
                          _ ->
                            (updRide.tollCharges, updRide.tollNames, updRide.tollIds, Nothing)

                fork "ride-interpolation" $ do
                  interpolatedPoints <- getInterpolatedPoints updRide.driverId
                  let rideInterpolationData = RideInterpolationData {interpolatedPoints = interpolatedPoints, rideId = updRide.id}
                  when (isJust updRide.driverDeviatedToTollRoute && tollConfidence == Just Sure && ((maybe True (== 0) tollCharges && isJust updRide.estimatedTollCharges) || fromMaybe False (((,) <$> tollCharges <*> updRide.estimatedTollCharges) <&> \(tollCharges', estimatedTollCharges') -> tollCharges' /= estimatedTollCharges'))) $ pushToKafka rideInterpolationData "ride-interpolated-waypoints" updRide.id.getId

                let ride = updRide{tollCharges = tollCharges, tollNames = tollNames, tollIds = tollIds, tollConfidence = tollConfidence, distanceCalculationFailed = Just distanceCalculationFailed}

                (chargeableDistance, finalFare, mbUpdatedFareParams) <-
                  if shouldRectifyDistantPointsSnapToRoadFailure
                    then recalculateFareForDistance handle booking ride (fromMaybe (roundToIntegral ride.traveledDistance) (bool (Just $ roundToIntegral ride.traveledDistance) booking.estimatedDistance distanceCalculationFailed)) thresholdConfig False tripEndPoint
                    else
                      if distanceCalculationFailed
                        then calculateFinalValuesForFailedDistanceCalculations handle booking ride tripEndPoint pickupDropOutsideOfThreshold thresholdConfig
                        else calculateFinalValuesForCorrectDistanceCalculations handle booking ride booking.maxEstimatedDistance pickupDropOutsideOfThreshold thresholdConfig tripEndPoint
                pure (chargeableDistance, finalFare, mbUpdatedFareParams, ride, Just pickupDropOutsideOfThreshold, Just distanceCalculationFailed)
    let newFareParams = fromMaybe booking.fareParams mbUpdatedFareParams
    mbFarePolicy <- FarePolicy.getFarePolicyByEstOrQuoteIdWithoutFallback booking.quoteId
    finalCommission <- FareV2.calculateCommission newFareParams mbFarePolicy
    clearEditDestinationWayAndSnappedPointsFork <- awaitableFork "endRide->clearEditDestinationWayAndSnappedPoints" $ withTimeAPI "endRide" "clearEditDestinationWayAndSnappedPoints" $ clearEditDestinationWayAndSnappedPoints driverId
    clearReachedStopLocationsFork <- awaitableFork "endRide->clearReachedStopLocations" $ withTimeAPI "endRide" "clearReachedStopLocations" $ clearReachedStopLocations rideOld.id
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
               endOdometerReading = mbOdometer,
               driverGoHomeRequestId = ghInfo.driverGoHomeRequestId,
               hasStops = Just (not $ null ride.stops),
               commission = finalCommission
              }
    newRideTags <- withTryCatch "computeNammaTags:RideEnd" (Yudhishthira.computeNammaTags Yudhishthira.RideEnd (Y.EndRideTagData updRide' booking))
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
        DC.driverCoinsEvent driverId booking.providerId booking.merchantOperatingCityId (DCT.EndRide (isJust booking.disabilityTag) (booking.coinsRewardedOnGoldTierRide) updRide metroRideType) (Just ride.id.getId) ride.vehicleVariant (Just booking.configInExperimentVersions)

    -- GPS Toll Behavior Check - evaluate if driver intentionally turned off GPS on toll route
    fork "GpsTollBehavior Check" $ do
      let isTollRide = isJust updRide.estimatedTollCharges || isJust updRide.tollCharges
          gpsTurnedOff = fromMaybe False updRide.driverGpsTurnedOff
      when (isTollRide && gpsTurnedOff) $ do
        logInfo $ "Driver turned off GPS on toll ride. DriverId: " <> driverId.getId <> ", RideId: " <> updRide.id.getId
        -- Get window days from config (default 15 days if not configured)
        let windowDays = fromMaybe 15 thresholdConfig.gpsTollBehaviorWindowDays
        -- Get historical bad behavior count from sliding window for configured days
        badBehaviorCount <- GpsTollBehavior.getGpsTollBadBehaviorCount windowDays driverId
        -- Build input data for JSON logic evaluation
        let gpsTollBehaviorData =
              GpsTollBehavior.GpsTollBehaviorData
                { estimatedTollCharges = updRide.estimatedTollCharges,
                  estimatedTollNames = updRide.estimatedTollNames,
                  estimatedTollIds = updRide.estimatedTollIds,
                  detectedTollCharges = updRide.tollCharges,
                  detectedTollNames = updRide.tollNames,
                  detectedTollIds = updRide.tollIds,
                  gpsTurnedOffInCurrentRide = gpsTurnedOff,
                  badBehaviorInTollRouteCount = badBehaviorCount
                }
        -- Fetch rules from App Dynamic Logic (GPS_TOLL_BEHAVIOR domain)
        localTime <- getLocalCurrentTime thresholdConfig.timeDiffFromUtc
        (allLogics, _mbVersion) <- getAppDynamicLogic (cast booking.merchantOperatingCityId) LYT.GPS_TOLL_BEHAVIOR localTime Nothing Nothing
        -- Evaluate behavior using App Dynamic Logic
        output <- GpsTollBehavior.evaluateGpsTollBehavior allLogics gpsTollBehaviorData
        logInfo $ "GPS Toll Behavior evaluation result: " <> show output
        -- Increment bad behavior counter if needed
        when output.shouldIncrementBadBehaviorTollCounter $ do
          logInfo $ "Incrementing GPS toll bad behavior counter for driver: " <> driverId.getId
          GpsTollBehavior.incrementGpsTollBadBehaviorCount driverId
        -- Block driver for future toll rides if needed
        when output.shouldBlockForFutureTollRide $ do
          logWarning $ "Blocking driver for future toll rides: " <> driverId.getId <> ", duration: " <> show output.blockDurationInHours <> " hours"
          GpsTollBehavior.blockDriverForTollRoutes driverId output.blockDurationInHours

    computeEligibleUpgradeTiers ride thresholdConfig
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

    awaitAll [clearEditDestinationWayAndSnappedPointsFork, endRideTransactionFork, clearInterpolatedPointsFork, notifyCompleteToBAPFork, clearReachedStopLocationsFork]

    return updRide
  driverRideRes <- do
    mbRideDetail <- QRD.findById finalUpdatedRide.id
    case mbRideDetail of
      Nothing -> return Nothing
      Just rideDetail -> do
        driverNumber <- getDriverNumberFromRequest req finalUpdatedRide.driverId
        let rideRating = Nothing
            mbExophone = Nothing
            bapMetadata = Nothing
        isValueAddNP <- CQVAN.isValueAddNP booking.bapId
        stopsInfo <- if fromMaybe False finalUpdatedRide.hasStops then QSI.findAllByRideId finalUpdatedRide.id else return []
        let goHomeReqId = finalUpdatedRide.driverGoHomeRequestId
        Just <$> DUIRideCommon.mkDriverRideRes rideDetail driverNumber rideRating mbExophone (finalUpdatedRide, booking) bapMetadata goHomeReqId Nothing isValueAddNP stopsInfo Nothing

  return $
    EndRideResp
      { result = "Success",
        homeLocationReached = homeLocationReached',
        driverRideRes = driverRideRes
      }
  where
    clearEditDestinationWayAndSnappedPoints driverId = LocUpdInternal.deleteEditDestinationSnappedWaypoints driverId >> LocUpdInternal.deleteEditDestinationWaypoints driverId
    clearReachedStopLocations existingRideId = Redis.del (VID.mkReachedStopKey existingRideId)
    awaitAll = mapM_ (L.await Nothing)
    buildRoutesReq tripEndPoint driverHomeLocation =
      Maps.GetRoutesReq
        { waypoints = tripEndPoint :| [driverHomeLocation],
          mode = Nothing,
          calcPoints = True
        }

    withFallback defaultVal action = do
      res <- withTryCatch "withFallback:endRideHandler" action
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
        (True, _, _) -> DCT.FromOrToMetro
        (_, True, _) -> DCT.FromOrToMetro
        _ -> DCT.None
      where
        tagArr = Text.splitOn "_" splLocTag
        sourceTag = tagArr BODUC.!? 0
        destTag = tagArr BODUC.!? 1
        priorityTag = tagArr BODUC.!? 2
        fromMetro = sourceTag == Just sureMetro || sourceTag == Just sureWarriorMetro
        toMetro = destTag == Just sureMetro || destTag == Just sureWarriorMetro
    Nothing -> DCT.None

recalculateFareForDistance :: (MonadThrow m, Log m, MonadTime m, MonadGuid m, EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r, ServiceFlow m r, HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal], HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) => ServiceHandle m -> SRB.Booking -> DRide.Ride -> Meters -> DTConf.TransporterConfig -> Bool -> LatLong -> m (Meters, HighPrecMoney, Maybe FareParameters)
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
  let estimatedFare = Fare.fareSum booking.fareParams Nothing
      destinationWaitingTime = fromMaybe 0 $ if isNothing ride.destinationReachedAt || (not $ isUnloadingTimeRequired booking.vehicleServiceTier) then Nothing else fmap (max 0) (secondsToMinutes . roundToIntegral <$> (diffUTCTime <$> pure tripEndTime <*> ride.destinationReachedAt))
  vehicleAge <-
    if DTC.isAmbulanceTrip booking.tripCategory
      then do
        rideDetail <- QRD.findById ride.id -- replica?
        pure $ (.vehicleAge) =<< rideDetail
      else pure Nothing
  farePolicy <-
    if recomputeWithLatestPricing
      then getFarePolicyOnEndRide (Just $ getCoordinates booking.fromLocation) (Just . getCoordinates =<< booking.toLocation) booking.fromLocGeohash booking.toLocGeohash (Just recalcDistance) finalDuration (getCoordinates tripEndPoint) booking.merchantOperatingCityId booking.tripCategory booking.vehicleServiceTier booking.area booking.quoteId (Just booking.startTime) (Just booking.isDashboardRequest) booking.dynamicPricingLogicVersion (Just (TransactionId (Id booking.transactionId))) booking.configInExperimentVersions
      else getFarePolicyByEstOrQuoteId (Just $ getCoordinates booking.fromLocation) (Just . getCoordinates =<< booking.toLocation) booking.fromLocGeohash booking.toLocGeohash (Just recalcDistance) finalDuration booking.merchantOperatingCityId booking.tripCategory booking.vehicleServiceTier booking.area booking.quoteId (Just booking.startTime) (Just booking.isDashboardRequest) booking.dynamicPricingLogicVersion (Just (TransactionId (Id booking.transactionId))) booking.configInExperimentVersions
  QRide.updateFinalFarePolicyId (Just farePolicy.id) ride.id
  if farePolicy.disableRecompute == Just True
    then return (fromMaybe 0 booking.estimatedDistance, booking.estimatedFare, Nothing)
    else do
      stopsInfo <- if fromMaybe False ride.hasStops then QSI.findAllByRideId ride.id else return []
      -- Recalculate stateEntryPermitCharges based on actual stops taken
      -- Build segments from actual pickup, stops, and trip end point
      let sortedStops = sortOn (.stopOrder) stopsInfo
          actualStopLocations = map (.stopStartLatLng) sortedStops
          pickupLocation = getCoordinates booking.fromLocation
          journeySegments = SEPD.buildJourneySegmentsFromActualStops pickupLocation actualStopLocations tripEndPoint
      logDebug $
        "[STATE_ENTRY_PERMIT][EndRide] journeySegmentsFromActualStops=" <> show journeySegments
      -- Calculate segment-based stateEntryPermitCharges for stops crossing city boundaries
      -- Only apply for Intracity and Cross City rides (not for Intercity rides)
      -- Case 1: Intracity Ride - even though source and destination are in same city, if any stop
      --         lies outside the city, we check each segment that crosses cities and apply charges
      -- Case 2: Cross City Ride - apply charges for segments that cross cities
      -- Case 3: Intercity Ride - don't apply segment-based charges
      -- Calculate state entry permit charges for rides with stops
      -- Comment 3: Only run for rides with stops (> 1 segment)
      -- Comment 2: Wrap in error handling
      let (isOverallIntercity, isOverallCrossCity) = SEPD.determineOverallRideType [booking.tripCategory]
      merchant <- getMerchant merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
      mbSegmentStateEntryPermitCharges <-
        if length journeySegments <= 1
          then do
            -- Simple A-to-B (no stops): return Nothing to use fare policy's stateEntryPermitCharges
            logDebug "[STATE_ENTRY_PERMIT][EndRide] Single segment (no stops), using fare policy's stateEntryPermitCharges"
            return Nothing
          else
            if isOverallIntercity && not isOverallCrossCity
              then do
                logDebug "[STATE_ENTRY_PERMIT][EndRide] Overall intercity (non-cross-city), skipping segment-based stateEntryPermitCharges"
                return Nothing
              else do
                -- Comment 1: Extract to helper function for better maintainability
                -- Comment 2: Wrap in error handling
                result <- withTryCatch "calculateSegmentCharges:EndRide" $ do
                  segmentChargeResults <- calculateSegmentChargesForEndRide merchant merchantId thresholdConfig booking journeySegments
                  let totalCharges = SEPD.calculateTotalStateEntryPermitCharges segmentChargeResults
                  logInfo $ "[STATE_ENTRY_PERMIT][EndRide] Calculated segment charges: segments=" <> show (length journeySegments) <> ", totalCharges=" <> show totalCharges
                  return totalCharges
                case result of
                  Right charges -> return charges
                  Left err -> do
                    logError $ "[STATE_ENTRY_PERMIT][EndRide] Segment calculation failed: " <> show err <> ". Defaulting stateEntryPermitCharges to Nothing (zero charges)."
                    return Nothing
      fareParams <-
        calculateFareParameters
          Fare.CalculateFareParametersParams
            { farePolicy = farePolicy,
              actualDistance = Just recalcDistance,
              estimatedDistance = Just oldDistance,
              rideTime = booking.startTime,
              returnTime = booking.returnTime,
              roundTrip = fromMaybe False booking.roundTrip,
              waitingTime = fmap (destinationWaitingTime +) $ if isNothing ride.driverArrivalTime then Nothing else fmap (max 0) (secondsToMinutesCeil . roundToIntegral <$> (diffUTCTime <$> ride.tripStartTime <*> (liftA2 max ride.driverArrivalTime (Just booking.startTime)))),
              stopWaitingTimes = stopsInfo <&> (\stopInfo -> max 0 (secondsToMinutesCeil $ roundToIntegral (diffUTCTime (fromMaybe stopInfo.waitingTimeStart stopInfo.waitingTimeEnd) stopInfo.waitingTimeStart))),
              actualRideDuration = finalDuration,
              estimatedRideDuration = booking.estimatedDuration,
              driverSelectedFare = booking.fareParams.driverSelectedFare,
              customerExtraFee = booking.fareParams.customerExtraFee,
              nightShiftCharge = booking.fareParams.nightShiftCharge,
              petCharges = booking.fareParams.petCharges,
              estimatedCongestionCharge = booking.estimatedCongestionCharge,
              customerCancellationDues = booking.fareParams.customerCancellationDues,
              nightShiftOverlapChecking = DTC.isFixedNightCharge booking.tripCategory,
              timeDiffFromUtc = Just thresholdConfig.timeDiffFromUtc,
              tollCharges = ride.tollCharges,
              stateEntryPermitCharges = mbSegmentStateEntryPermitCharges,
              vehicleAge = vehicleAge,
              currency = booking.currency,
              noOfStops = length ride.stops,
              distanceUnit = booking.distanceUnit,
              shouldApplyBusinessDiscount = booking.billingCategory == SLT.BUSINESS,
              merchantOperatingCityId = Just booking.merchantOperatingCityId,
              mbAdditonalChargeCategories = Just $ map (.chargeCategory) booking.fareParams.conditionalCharges,
              numberOfLuggages = booking.numberOfLuggages
            }
      let finalFare = Fare.fareSum fareParams Nothing
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
  (MonadFlow m, MonadThrow m, Log m, MonadTime m, MonadGuid m, EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r, ServiceFlow m r, HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal], HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) => ServiceHandle m -> SRB.Booking -> DRide.Ride -> Maybe HighPrecMeters -> Bool -> DTConf.TransporterConfig -> LatLong -> m (Meters, HighPrecMoney, Maybe FareParameters)
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
      QDI.updateDailyAndWeeklyExtraKms (Just dailyExtraKms) (Just weeklyExtraKms) ride.driverId
      pure (Just dailyExtraKms, Just weeklyExtraKms)

    notifyDriverOnExtraKmsLimitExceed = do
      driver <- QP.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
      overlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategoryInRideFlow booking.merchantOperatingCityId "EXTRA_KMS_LIMIT_EXCEEDED" (fromMaybe ENGLISH driver.language) Nothing Nothing booking.configInExperimentVersions >>= fromMaybeM (OverlayKeyNotFound "EXTRA_KMS_LIMIT_EXCEEDED")
      TN.sendOverlay booking.merchantOperatingCityId driver $ TN.mkOverlayReq overlay

calculateFinalValuesForFailedDistanceCalculations ::
  (MonadThrow m, Log m, MonadTime m, MonadGuid m, EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r, ServiceFlow m r, HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal], HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) => ServiceHandle m -> SRB.Booking -> DRide.Ride -> LatLong -> Bool -> DTConf.TransporterConfig -> m (Meters, HighPrecMoney, Maybe FareParameters)
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

-- Helper function to calculate segment charges (Comment 1: move logic to helper)
calculateSegmentChargesForEndRide ::
  (MonadFlow m, MonadThrow m, Log m, EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r, ServiceFlow m r, HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal], HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) =>
  DM.Merchant ->
  Id DM.Merchant ->
  DTConf.TransporterConfig ->
  SRB.Booking ->
  [SEPD.JourneySegment] ->
  m [SEPD.SegmentChargeResult]
calculateSegmentChargesForEndRide merchant merchantId thresholdConfig booking journeySegments = do
  forM journeySegments $ \segment -> do
    logDebug $ "[STATE_ENTRY_PERMIT][EndRide] evaluating segment from=" <> show segment.segmentFrom <> " to=" <> show segment.segmentTo
    segmentSourceCityResult <- DSearch.getNearestOperatingAndSourceCity merchant segment.segmentFrom
    segmentMerchantOpCityForSegment <- CQMOC.getMerchantOpCity merchant (Just segmentSourceCityResult.nearestOperatingCity.city)
    (segmentIsIntercity, segmentIsCrossCity, mbSegmentDestinationTravelCityName) <- DSearch.checkForIntercityOrCrossCity thresholdConfig (Just segment.segmentTo) segmentSourceCityResult.sourceCity merchant
    logDebug $ "[STATE_ENTRY_PERMIT][EndRide] segment classification isIntercity=" <> show segmentIsIntercity <> ", isCrossCity=" <> show segmentIsCrossCity <> ", destTravelCityName=" <> show mbSegmentDestinationTravelCityName
    mbSegmentFarePolicy <-
      if segmentIsIntercity || segmentIsCrossCity
        then do
          let segmentFromGeohash = Text.pack <$> Geohash.encode (fromMaybe 5 thresholdConfig.dpGeoHashPercision) (segment.segmentFrom.lat, segment.segmentFrom.lon)
              segmentToGeohash = Text.pack <$> Geohash.encode (fromMaybe 5 thresholdConfig.dpGeoHashPercision) (segment.segmentTo.lat, segment.segmentTo.lon)
          let segmentTripCategories = SEPD.constructSegmentTripCategories segmentIsIntercity segmentIsCrossCity mbSegmentDestinationTravelCityName False
          case listToMaybe segmentTripCategories of
            Just segmentTripCategory -> do
              segmentFarePoliciesProduct <- SFP.getAllFarePoliciesProduct merchantId segmentMerchantOpCityForSegment.id booking.isDashboardRequest segment.segmentFrom (Just segment.segmentTo) (Just (TransactionId (Id booking.transactionId))) segmentFromGeohash segmentToGeohash Nothing Nothing booking.dynamicPricingLogicVersion segmentTripCategory booking.configInExperimentVersions
              let segmentFarePolicies = filter (\fp -> fp.vehicleServiceTier == booking.vehicleServiceTier) segmentFarePoliciesProduct.farePolicies
              return $ listToMaybe segmentFarePolicies
            Nothing -> return Nothing
        else return Nothing
    SEPD.calculateSegmentStateEntryPermitCharge segmentIsIntercity segmentIsCrossCity mbSegmentFarePolicy segment
