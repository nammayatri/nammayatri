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
    EndRideFlow,
    RideInterpolationData (..),
    callBasedEndRide,
    buildEndRideHandle,
    driverEndRide,
    dashboardEndRide,
    cronJobEndRide,
    isPickupDropOutsideOfThreshold,

    -- * Fare computation shared with collect-payment
    FareComputeMode (..),
    RideFarePreview (..),
    previewRideFare,
    buildTollInput,
  )
where

-- import qualified Lib.Yudhishthira.Event as Yudhishthira

import qualified BecknV2.OnDemand.FareBreakup.Core as Fields
import Data.Either.Extra (eitherToMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import Data.OpenApi.Internal.Schema (ToSchema)
import qualified Data.Text as Text
import Data.Time (utctDay)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Action.Internal.ViolationDetection as VID
import qualified Domain.Action.UI.DriverGoHomeRequest as Fields
import qualified Domain.Action.UI.Ride.Common as DUIRideCommon
import qualified Domain.Action.UI.Ride.EndRide.Internal as RideEndInt
import qualified Domain.Action.UI.Ride.EndRide.RecomputeDecision as RD
import qualified Domain.Action.UI.Ride.EndRide.TollDecision as TD
import Domain.Action.UI.Route as DMaps
import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.ConditionalCharges as Fields
import qualified Domain.Types.DriverGoHomeRequest as DDGR
import Domain.Types.FareParameters as Fare
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.GoHomeConfig as Fields
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideDetails as Fields
import qualified Domain.Types.RiderDetails as RD
import qualified Domain.Types.StopInformation as Fields
import qualified Domain.Types.TransporterConfig as DTConf
import qualified Domain.Types.Yudhishthira as Y
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id, pi)
import Kernel.Beam.Functions (runInMasterDbAndRedis)
import Kernel.Beam.Types (TxnIdKey (..))
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Encryption as Fields
import Kernel.External.Maps
import qualified Kernel.External.Maps.Interface.Types as Maps
import qualified Kernel.External.Maps.Types as Maps
import qualified Kernel.External.Types as KET
import Kernel.Prelude (roundToIntegral)
import Kernel.Storage.Clickhouse.Config
import qualified Kernel.Storage.ClickhouseV2 as CHV2
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Common hiding (Days)
import Kernel.Types.Id
import Kernel.Types.SlidingWindowCounters
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common hiding (Days)
import Kernel.Utils.DatastoreLatencyCalculator
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified Lib.DriverCoins.Coins as DC
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.IncentiveJourney.Storage.Beam.BeamFlow as IJBeamFlow
import qualified Lib.LocationUpdates as LocUpd
import qualified Lib.LocationUpdates.Internal as LocUpdInternal
import qualified Lib.Queries.SpecialLocation as QSL
import Lib.Scheduler.Environment (JobCreator)
import qualified Lib.Types.SpecialLocation as SL
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.CallBAP as CallBAP
import qualified SharedLogic.CallBAPInternal as CallBAPInternal
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.FareCalculator as Fare
import qualified SharedLogic.FarePolicy as FarePolicy
import qualified SharedLogic.GpsDetectedTolls as GpsDetectedTolls
import qualified SharedLogic.ManualTollCharge as ManualTollCharge
import qualified SharedLogic.MerchantPaymentMethod as DMPM
import qualified SharedLogic.ParkingFeeExemption as SPFE
import qualified SharedLogic.PassengerLuggage as SPL
import SharedLogic.Ride (validateRideLocation)
import qualified SharedLogic.SharedRun.Fare as SharedRunFare
import qualified SharedLogic.Type as SLT
import Storage.Beam.IncentiveJourney ()
import Storage.Beam.Toll ()
import qualified Storage.CachedQueries.DomainDiscountConfig as CQDDC
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Merchant as MerchantS
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import Storage.ConfigPilot.Config.GoHomeConfig (GoHomeConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Booking as QRB
import Storage.Queries.DriverGoHomeRequest as QDGR
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRD
import qualified Storage.Queries.RiderDetails as QRiderDetails
import qualified Storage.Queries.RiderDriverCorrelation as QRiderDriverCorrelation
import qualified Storage.Queries.StopInformation as QSI
import qualified Toll.SharedLogic.TollsDetector as TollsDetector
import Tools.Error
import qualified Tools.Maps as TM
import qualified Tools.Notifications as TN
import Tools.Utils (isDropInsideThreshold)
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
    driverGpsTurnedOff :: Maybe Bool,
    manualTollCharge :: Maybe HighPrecMoney
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
    endRideTransaction :: Id DP.Driver -> SRB.Booking -> DRide.Ride -> Maybe FareParameters -> Maybe (Id RD.RiderDetails) -> FareParameters -> DTConf.TransporterConfig -> HighPrecMoney -> m (),
    notifyCompleteToBAP :: SRB.Booking -> DRide.Ride -> Fare.FareParameters -> Maybe DMPM.PaymentMethodInfo -> Maybe Text -> Maybe LatLong -> m (),
    getFarePolicyByEstOrQuoteId :: Maybe UTCTime -> Maybe LatLong -> Maybe LatLong -> Maybe Text -> Maybe Text -> Maybe Meters -> Maybe Seconds -> Id DMOC.MerchantOperatingCity -> DTC.TripCategory -> DVST.ServiceTierType -> Maybe SL.Area -> Text -> Maybe UTCTime -> Maybe Bool -> Maybe Int -> Maybe CacKey -> [LYT.ConfigVersionMap] -> Maybe Text -> m DFP.FullFarePolicy,
    getFarePolicyOnEndRide :: Maybe UTCTime -> Maybe LatLong -> Maybe LatLong -> Maybe Text -> Maybe Text -> Maybe Meters -> Maybe Seconds -> LatLong -> Id DMOC.MerchantOperatingCity -> DTC.TripCategory -> DVST.ServiceTierType -> Maybe SL.Area -> Text -> Maybe UTCTime -> Maybe Bool -> Maybe Int -> Maybe CacKey -> [LYT.ConfigVersionMap] -> Maybe Text -> m DFP.FullFarePolicy,
    calculateFareParameters :: Fare.CalculateFareParametersParams -> m Fare.FareParameters,
    putDiffMetric :: Id DM.Merchant -> HighPrecMoney -> Meters -> m (),
    isDistanceCalculationFailed :: Id DP.Person -> m Bool,
    finalDistanceCalculation :: Maybe MapsServiceConfig -> Bool -> Bool -> Id DRide.Ride -> Id DP.Person -> [(LatLong, Int64)] -> NonEmpty (LatLong, Int64) -> Meters -> Maybe HighPrecMoney -> Maybe [Text] -> Maybe [Text] -> Bool -> Bool -> Bool -> m (),
    getInterpolatedPoints :: Id DP.Person -> m [LatLong],
    clearInterpolatedPoints :: Id DP.Person -> m (),
    findConfig :: Maybe CacKey -> m (Maybe DTConf.TransporterConfig),
    whenWithLocationUpdatesLock :: forall a. Id DP.Person -> m a -> m a,
    getRouteAndDistanceBetweenPoints :: Maybe Text -> LatLong -> LatLong -> [LatLong] -> Meters -> m ([LatLong], Meters),
    findPaymentMethodByIdAndMerchantId :: Id DMPM.MerchantPaymentMethod -> Id DMOC.MerchantOperatingCity -> m (Maybe DMPM.MerchantPaymentMethod),
    uiDistanceCalculation :: Id DRide.Ride -> Maybe Int -> Maybe Int -> m (),
    getCongestionChargeOnEndRide :: Seconds -> Maybe LatLong -> Maybe Text -> Maybe Text -> DVST.ServiceTierType -> Maybe Meters -> Maybe Seconds -> Maybe Double -> Maybe FarePolicy.DropQARConfig -> Maybe Text -> Maybe Int -> Id DMOC.MerchantOperatingCity -> Maybe Seconds -> Maybe Seconds -> Maybe DTC.TripCategory -> Maybe Text -> Maybe SL.Area -> m (Maybe FarePolicy.CongestionChargeDetailsModel)
  }

buildEndRideHandle ::
  ( LocUpd.LocationUpdateFlow m r c,
    HasField "activeDriversListKeyShards" r Int,
    HasField "enableDriverFeeShardedFanOut" r Bool,
    HasField "ijBehaviorTickPublisherCfg" r (Maybe Environment.RideEventsPublisherCfg),
    IJBeamFlow.BeamFlow m r
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe (Id DRide.Ride) ->
  Bool ->
  m (ServiceHandle m)
buildEndRideHandle merchantId merchantOpCityId rideId allowSnapshotVehicleFallback = do
  defaultRideInterpolationHandler <- LocUpd.buildRideInterpolationHandler merchantId merchantOpCityId rideId True Nothing
  return $
    ServiceHandle
      { findBookingById = QRB.findById,
        findRideById = QRide.findById,
        getMerchant = MerchantS.findById,
        notifyCompleteToBAP = \b r fp pm pu loc -> CallBAP.sendRideCompletedUpdateToBAP b r fp pm pu loc allowSnapshotVehicleFallback,
        endRideTransaction = RideEndInt.endRideTransaction,
        getFarePolicyByEstOrQuoteId = FarePolicy.getFarePolicyByEstOrQuoteId,
        getFarePolicyOnEndRide = FarePolicy.getFarePolicyOnEndRide,
        calculateFareParameters = Fare.calculateFareParameters,
        putDiffMetric = RideEndInt.putDiffMetric,
        isDistanceCalculationFailed = LocUpd.isDistanceCalculationFailed defaultRideInterpolationHandler,
        finalDistanceCalculation = LocUpd.finalDistanceCalculation defaultRideInterpolationHandler,
        getInterpolatedPoints = LocUpd.getInterpolatedPoints defaultRideInterpolationHandler,
        clearInterpolatedPoints = LocUpd.clearInterpolatedPoints defaultRideInterpolationHandler,
        findConfig = \_ -> getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing,
        whenWithLocationUpdatesLock = LocUpd.whenWithLocationUpdatesLock,
        getRouteAndDistanceBetweenPoints = RideEndInt.getRouteAndDistanceBetweenPoints merchantId merchantOpCityId,
        findPaymentMethodByIdAndMerchantId = CQMPM.findByIdAndMerchantOpCityId,
        uiDistanceCalculation = QRide.updateUiDistanceCalculation,
        getCongestionChargeOnEndRide = \timeDiff mbFromLoc mbFromGeohash mbToGeohash svcTier mbDist mbDur mbRadius mbDropQARConfig mbSpecialLoc mbDpVersion mocId mbEstDur mbActDur mbTripCategory mbTxnId mbArea ->
          -- mbTxnId (booking.transactionId) lets the surge wrapper replay the
          -- surge version PINNED at search time, mirroring how mbDpVersion pins
          -- the json-logic rule version for this recompute
          -- trailing Nothing: the engine flag only affects initial pricing;
          -- this recompute path replays the pin or the json-logic version
          FarePolicy.getCongestionChargeMultiplierFromModel' Nothing mbDropQARConfig timeDiff mbFromLoc mbFromGeohash mbToGeohash svcTier Nothing mbDist mbDur (Just True) mbRadius mbSpecialLoc mbDpVersion mocId mbEstDur mbActDur mbTripCategory mbTxnId mbArea Nothing
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
  ( CoreMetrics m,
    HasField "enableAPILatencyLogging" r Bool,
    HasField "enableAPIPrometheusMetricLogging" r Bool,
    LT.HasLocationService m r,
    HasKafkaProducer r,
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    HasFlowEnv m r '["appBackendBapInternal" ::: CallBAPInternal.AppBackendBapInternal],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    -- the shared-route run's fare is read from the rider platform, whose client wants the request id
    HasRequestId r,
    JobCreator r m,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    Esq.EsqDBReplicaFlow m r,
    Finance.HasActorInfo m r
  )

driverEndRide ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasShortDurationRetryCfg r c, ClickhouseFlow m r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  DriverEndRideReq ->
  m EndRideResp
driverEndRide handle rideId req = do
  withLogTag ("requestorId-" <> req.requestor.id.getId)
    . endRide handle rideId
    $ DriverReq req

callBasedEndRide ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasShortDurationRetryCfg r c, ClickhouseFlow m r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  CallBasedEndRideReq ->
  m EndRideResp
callBasedEndRide handle rideId = endRide handle rideId . CallBasedReq

dashboardEndRide ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasShortDurationRetryCfg r c, ClickhouseFlow m r) =>
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
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasShortDurationRetryCfg r c, ClickhouseFlow m r) =>
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
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasShortDurationRetryCfg r c, ClickhouseFlow m r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  EndRideReq ->
  m EndRideResp
endRide handle rideId req = withLogTag ("rideId-" <> rideId.getId) do
  isLocked <- withLockRideId
  if isLocked
    then do
      finally
        (DUIRideCommon.waitForRideStopsLock rideId $ endRideHandler handle rideId req)
        ( do
            Redis.unlockRedis mkLockKey
            logDebug $ "End ride for RideId: " <> rideId.getId <> " Unlocked"
        )
    else throwError (InvalidRequest "End ride already in progress for this ride")
  where
    withLockRideId = do
      isLocked <- Redis.tryLockRedis mkLockKey 60
      return isLocked
    mkLockKey = "EndTransaction:RID:-" <> rideId.getId

endRideHandler ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasShortDurationRetryCfg r c, ClickhouseFlow m r, Redis.HedisLTSFlowEnv r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  EndRideReq ->
  m EndRideResp
endRideHandler handle@ServiceHandle {..} rideId req = do
  rideOld <- runInMasterDbAndRedis $ findRideById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  let driverId = rideOld.driverId
  booking <- findBookingById rideOld.bookingId >>= fromMaybeM (BookingNotFound rideOld.bookingId.getId)
  L.setOptionLocal TxnIdKey booking.transactionId
  case req of
    DriverReq driverReq -> do
      let requestor = driverReq.requestor
      validateRideLocation driverReq.point
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
      whenJust dashboardReq.point validateRideLocation
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
          endPoint <- (getCoordinates <$> booking.toLocation) <|> rideOld.tripStartPos & fromMaybeM (InvalidRequest "Trip end location is required")
          pure (endPoint, Nothing, DRide.CronJob)
    CallBasedReq _ -> do
      toLocation <- booking.toLocation & fromMaybeM (InvalidRequest "Trip end location is required")
      pure (getCoordinates toLocation, Nothing, DRide.CallBased)

  goHomeConfig <- getConfig (GoHomeConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (InvalidRequest $ "GoHome Config not found for MerchantOperatingCity: " <> booking.merchantOperatingCityId.getId)
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
    (allTripEndPoints, accTripEndPoints) <- do
      let mbAdvanceRideId = (.id) <$> advanceRide
      res <- LF.rideEnd rideId tripEndPoint.lat tripEndPoint.lon booking.providerId driverId mbAdvanceRideId Nothing (Just $ floor $ utcTimeToPOSIXSeconds now)
      let nowTs = floor $ utcTimeToPOSIXSeconds now
          mkPoint locUpd = (LatLong locUpd.lat locUpd.lon, fromMaybe nowTs locUpd.ts)
          accLoc = NE.filter (\locUpd -> maybe True (< 50) locUpd.acc) res.loc
      pure (toList $ fmap mkPoint res.loc, map mkPoint accLoc)
    (chargeableDistance, finalFare, mbUpdatedFareParams, mbRecalcFarePolicy, ride, pickupDropOutsideOfThreshold, distanceCalculationFailed, mbShadowApprox) <-
      case req of
        -- a shared-route run is priced by its passengers, not its distance: the fare is the sum of the tickets of the
        -- riders who boarded, read from the rider platform's manifest; the distance ladder below would price it as a cab ride
        _ | DTC.isSharedRouteTrip booking.tripCategory -> do
          (runFare, runFareParams) <- SharedRunFare.runFareAtEnd booking rideOld
          pure (roundToIntegral rideOld.traveledDistance, runFare, Just runFareParams, Nothing, rideOld, Nothing, Nothing, Nothing)
        CronJobReq _ -> do
          logTagInfo "cron job -> endRide : " "Do not call snapToRoad, return estimates as final values."
          res <- withTryCatch "recalculateFareForDistance:endRideHandler" $ recalculateFareForDistance EndRideMode True handle booking rideOld estimatedDistance thresholdConfig False tripEndPoint
          (chargeableDistance, finalFare, mbUpdatedFareParams, mbRecalcFarePolicy) <-
            case res of
              Left err -> do
                logTagError "recalculateFareForDistance" $ "Failed to recalculate fare : " <> show err
                return (fromMaybe 0 booking.estimatedDistance, booking.estimatedFare, Nothing, Nothing)
              Right response -> return response

          pure (chargeableDistance, finalFare, mbUpdatedFareParams, mbRecalcFarePolicy, rideOld, Nothing, Nothing, Nothing)
        _ -> do
          withNoFareProductFallback (fromMaybe 0 booking.estimatedDistance, booking.estimatedFare, Nothing, Nothing, rideOld, Nothing, Nothing, Nothing) $ do
            if DTC.isOdometerReadingsRequired booking.tripCategory
              then do
                case mbOdometer of
                  Just odometer -> do
                    odometerCalculatedDistance <- getOdometerDistance rideOld odometer
                    (recalcDistance, finalFare, mbUpdatedFareParams, mbRecalcFarePolicy) <- recalculateFareForDistance EndRideMode False handle booking rideOld odometerCalculatedDistance thresholdConfig False tripEndPoint
                    pure (recalcDistance, finalFare, mbUpdatedFareParams, mbRecalcFarePolicy, rideOld, Nothing, Nothing, Nothing)
                  Nothing -> throwError $ OdometerReadingRequired (show booking.tripCategory)
              else do
                -- here we update the current ride, so below we fetch the updated version
                pickupDropOutsideOfThreshold <- isPickupDropOutsideOfThreshold booking rideOld tripEndPoint thresholdConfig
                whenJust (nonEmpty allTripEndPoints) \allTripEndPoints' -> do
                  rectificationMapsConfig <-
                    if shouldRectifyDistantPointsSnapToRoadFailure
                      then Just <$> TM.getServiceConfigForRectifyingSnapToRoadDistantPointsFailure booking.providerId booking.merchantOperatingCityId
                      else pure Nothing
                  let passedThroughDrop = any (isDropInsideThreshold booking thresholdConfig . fst) accTripEndPoints
                  logDebug $ "Did we passed through drop yet in endRide" <> show passedThroughDrop <> " " <> show accTripEndPoints
                  withTimeAPI "endRide" "finalDistanceCalculation" $ finalDistanceCalculation rectificationMapsConfig (DTC.isTollApplicableForTrip booking.vehicleServiceTier booking.tripCategory) thresholdConfig.enableTollCrossedNotifications rideOld.id driverId accTripEndPoints allTripEndPoints' estimatedDistance estimatedTollCharges estimatedTollNames estimatedTollIds pickupDropOutsideOfThreshold passedThroughDrop (booking.tripCategory == DTC.OneWay DTC.MeterRide)

                updRide <- runInMasterDbAndRedis $ findRideById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
                -- Toll reconciliation + distance-branch selection, shared with the collect-payment preview.
                -- End ride resolves the driver-declared manual toll (which throws while the rider's
                -- approval is pending/rejected), so it never reports a pending confirmation.
                let resolveManualToll signal = case req of
                      DriverReq driverReq -> (,False) . join <$> forM driverReq.manualTollCharge (ManualTollCharge.resolveManualTollChargeForEndRide thresholdConfig rideOld booking.tripCategory booking.vehicleServiceTier signal)
                      _ -> pure (Nothing, False)
                fst <$> computeTrackedDistanceFare EndRideMode handle booking updRide tripEndPoint thresholdConfig pickupDropOutsideOfThreshold resolveManualToll
    -- Discount, commission and payment charge on top of the computed fare (shared with the collect-payment preview).
    FinalisedRideFare {baseFareParams, discountAmount, mbFarePolicy, commission = finalCommission, chargeRes, mbFareParamsToPersist} <-
      finaliseRideFare thresholdConfig booking finalFare mbUpdatedFareParams mbRecalcFarePolicy
    let finalCancellationDuesCommission = Fare.calculateCancellationDuesCommission booking.cancellationDuesCommission baseFareParams
    -- SHADOW MODE: the legacy ladder above did the billing; this evaluates the
    -- unified decision core on the same inputs, persists its reason on the
    -- ride, and logs/counts divergence. See RecomputeDecision module header.
    mbRecomputeDecision <- shadowRecomputeDecision req booking ride mbOdometer pickupDropOutsideOfThreshold distanceCalculationFailed mbShadowApprox chargeableDistance tripEndPoint thresholdConfig mbFarePolicy now
    let finalFareWithPaymentCharge = chargeRes.adjustedFare
        mbPaymentCharge = chargeRes.paymentCharge
        mbPaymentChargeBearer = chargeRes.paymentChargeBearer
        rideFareParams = chargeRes.adjustedFareParams
    clearEditDestinationWayAndSnappedPointsFork <- awaitableFork "endRide->clearEditDestinationWayAndSnappedPoints" $ withTimeAPI "endRide" "clearEditDestinationWayAndSnappedPoints" $ clearEditDestinationWayAndSnappedPoints driverId
    clearReachedStopLocationsFork <- awaitableFork "endRide->clearReachedStopLocations" $ withTimeAPI "endRide" "clearReachedStopLocations" $ clearReachedStopLocations rideOld.id
    let updRide' =
          ride{DRide.tripEndTime = Just now,
               DRide.chargeableDistance = Just chargeableDistance,
               DRide.fare = Just finalFareWithPaymentCharge,
               DRide.status = DRide.COMPLETED,
               DRide.tripEndPos = Just tripEndPoint,
               DRide.rideEndedBy = Just rideEndedBy',
               DRide.fareParametersId = Just rideFareParams.id,
               DRide.tollCharges = mbUpdatedFareParams >>= (.tollCharges),
               DRide.distanceCalculationFailed = distanceCalculationFailed,
               DRide.pickupDropOutsideOfThreshold = pickupDropOutsideOfThreshold,
               DRide.endOdometerReading = mbOdometer,
               DRide.driverGoHomeRequestId = ghInfo.driverGoHomeRequestId,
               DRide.hasStops = Just (not $ null ride.stops),
               DRide.commission = finalCommission,
               DRide.cancellationDuesCommission = finalCancellationDuesCommission,
               DRide.cancellationDuesCommissionTaxBreakup = Fare.calculateCancellationDuesCommissionTax booking.cancellationDuesCommissionTaxBreakup baseFareParams,
               DRide.paymentCharge = mbPaymentCharge,
               DRide.paymentChargeBearer = mbPaymentChargeBearer,
               DRide.discountAmount = discountAmount,
               DRide.recomputeReason = (\d -> RD.mkShadowRecordText d chargeableDistance) <$> mbRecomputeDecision
              }
    let mbDriverFromReq = case req of
          DriverReq driverReq -> Just driverReq.requestor
          CallBasedReq callBasedReq -> Just callBasedReq.requestor
          _ -> Nothing
    mbDriver <- maybe (QP.findById driverId) (pure . Just) mbDriverFromReq
    mbRiderDetails <- join <$> (QRiderDetails.findById `mapM` booking.riderId)
    riderBlockedForCoins <- QRiderDetails.isRiderFlaggedForCoinZero booking.riderId
    let mbDriverMobileHash = (.hash) <$> (mbDriver >>= (.mobileNumber))
        mbRiderMobileHash = (.hash) . (.mobileNumber) <$> mbRiderDetails
        isDriverSameAsCustomer = isJust mbDriverMobileHash && isJust mbRiderMobileHash && mbDriverMobileHash == mbRiderMobileHash
        merchantLocalDay = utctDay $ addUTCTime (secondsToNominalDiffTime thresholdConfig.timeDiffFromUtc) now
        rideDurationSeconds = maybe 0 (\tStart -> max 0 $ roundToIntegral (diffUTCTime now tStart)) updRide'.tripStartTime
    priorRidesSameCustomer <- QRide.countPriorCompletedRidesWithSameCustomer (cast driverId) booking.riderId updRide'.id merchantLocalDay thresholdConfig.sameRiderDriverRideCountLookbackDays
    isFavouritePair <- isFavouriteDriverRiderPair (cast driverId) booking.riderId
    (shouldBlockCoinsForSameRiderFlow, shouldFlagRiderForRepeatCustomerFraud) <-
      if isFavouritePair
        then do
          priorRidesSameCustomerToday <- QRide.countPriorCompletedRidesWithSameCustomer (cast driverId) booking.riderId updRide'.id merchantLocalDay 1
          let exceededFavouriteDailyCap = priorRidesSameCustomerToday >= thresholdConfig.favouriteDriverDailyCoinRideThreshold
          pure (exceededFavouriteDailyCap, False)
        else do
          let exceededLookback = riderBlockedForCoins || priorRidesSameCustomer > thresholdConfig.sameRiderDriverRideCountThreshold
          pure (exceededLookback, exceededLookback)
    when shouldFlagRiderForRepeatCustomerFraud $ QRiderDetails.flagRiderForCoinZero booking.riderId
    newRideTags <- withTryCatch "computeNammaTags:RideEnd" (LYDL.computeNammaTagsWithDebugLog LYDL.Driver (cast booking.merchantOperatingCityId) LYT.RideEnd (Just booking.transactionId) (Y.EndRideTagData updRide' booking isDriverSameAsCustomer shouldBlockCoinsForSameRiderFlow rideDurationSeconds))
    let updRide = updRide' {DRide.rideTags = ride.rideTags <> eitherToMaybe newRideTags}
    QRide.incrementDriverRiderRideCountForDay (cast driverId) booking.riderId
    fork "updating time and latlong in advance ride if any" $ do
      whenJust advanceRide $ \advanceRide' -> do
        QRide.updatePreviousRideTripEndPosAndTime (Just tripEndPoint) (Just now) advanceRide'.id

    -- we need to store fareParams only when they changed
    endRideTransactionFork <- awaitableFork "endRide->endRideTransaction" $ withTimeAPI "endRide" "endRideTransaction" $ endRideTransaction (cast @DP.Person @DP.Driver driverId) booking updRide mbFareParamsToPersist booking.riderId rideFareParams thresholdConfig finalFare
    clearInterpolatedPointsFork <- awaitableFork "endRide->clearInterpolatedPoints" $ withTimeAPI "endRide" "clearInterpolatedPoints" $ clearInterpolatedPoints driverId

    -- Driver coins / incentive-journey evaluation moved to kafka-consumers
    -- RIDE_EVENTS_CONSUMER (SharedLogic.RideEvents.DriverCoinsAndJourney).

    mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId -> do
      findPaymentMethodByIdAndMerchantId paymentMethodId booking.merchantOperatingCityId
        >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
    let mbPaymentMethodInfo = DMPM.mkPaymentMethodInfo <$> mbPaymentMethod
    notifyCompleteToBAPFork <- awaitableFork "endRide->notifyCompleteToBAP" $ withTimeAPI "endRide" "notifyCompleteToBAP" $ notifyCompleteToBAP booking updRide rideFareParams mbPaymentMethodInfo Nothing (Just tripEndPoint)
    awaitAll [clearEditDestinationWayAndSnappedPointsFork, endRideTransactionFork, clearInterpolatedPointsFork, notifyCompleteToBAPFork, clearReachedStopLocationsFork]

    whenJust (booking.area >>= SL.dropSpecialZoneIdFromArea) $ \slIdText ->
      fork "endRide -> airport drop queue priority" $ do
        skipQueue <- QSL.checkSkipQueueById (Id slIdText)
        when skipQueue $
          Redis.setExp (mkDriverAirportDropKey driverId.getId) True 1800

    fork "Push End Ride Metric" $ incrementRideEndCounter "endRide"

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
        mbDriverPerson <- QP.findById finalUpdatedRide.driverId
        let endRideLanguage = fromMaybe KET.ENGLISH (mbDriverPerson >>= (.language))
        endRideLabels <- DUIRideCommon.fetchEarningsLabels finalUpdatedRide.merchantOperatingCityId endRideLanguage
        mbEndRideTransporterConfig <- findConfig (Just (TransactionId (Id booking.transactionId)))
        let endRideCalling =
              DUIRideCommon.ResolvedCalling
                { riderMobileNumber = Nothing,
                  callingNumber =
                    DUIRideCommon.CallingNumberAPIEntity
                      { number = DUIRideCommon.mkExoPhone mbExophone booking,
                        countryCode = Nothing,
                        numberType = DUIRideCommon.ANONYMOUS
                      }
                }
        Just <$> DUIRideCommon.mkDriverRideRes endRideLanguage (Just endRideLabels) rideDetail driverNumber rideRating mbExophone (finalUpdatedRide, booking) bapMetadata goHomeReqId Nothing isValueAddNP stopsInfo endRideCalling (mbEndRideTransporterConfig >>= (.showRoutePerRequest)) (mbEndRideTransporterConfig >>= (.enableManualArrival))

  return $
    EndRideResp
      { result = "Success",
        homeLocationReached = homeLocationReached',
        driverRideRes = driverRideRes
      }
  where
    isFavouriteDriverRiderPair driverId' mbRiderDetailsId = case mbRiderDetailsId of
      Nothing -> pure False
      Just riderDetailsId -> isJust <$> QRiderDriverCorrelation.checkRiderFavDriver riderDetailsId driverId' True

    clearEditDestinationWayAndSnappedPoints driverId = LocUpdInternal.deleteEditDestinationSnappedWaypoints driverId >> LocUpdInternal.deleteEditDestinationWaypoints driverId
    clearReachedStopLocations existingRideId = do
      Redis.del (VID.mkReachedStopsKey existingRideId)
      Redis.del (VID.mkProcessedStopsKey existingRideId)
    awaitAll = mapM_ (L.await Nothing)
    buildRoutesReq tripEndPoint driverHomeLocation =
      Maps.GetRoutesReq
        { waypoints = tripEndPoint :| [driverHomeLocation],
          mode = Nothing,
          calcPoints = True
        }

--------------------------------------------------------------------------------
-- Fare computation shared by end ride and the collect-payment preview
--
-- End ride and collect-payment ("what would the rider pay if the ride ended
-- here, now?") must bill exactly the same way, so both go through the helpers
-- below. 'FareComputeMode' is the only difference: in 'PreviewMode' every write
-- and every end-of-ride side effect along the fare path is skipped, so a preview
-- leaves the ride, the driver's extra-km budget and the metrics untouched, and a
-- later end ride computes exactly what it would have computed without a preview.
--------------------------------------------------------------------------------

-- | Why the fare is being computed.
data FareComputeMode
  = -- | The ride is ending: the computed fare is billed and its side effects are applied.
    EndRideMode
  | -- | Collect-payment preview while the ride is still in progress: read-only.
    PreviewMode
  deriving (Show, Eq)

-- | Runs an action only when the fare is really being billed (end ride). Every
-- write on the shared fare path is wrapped in this, which makes the list of
-- things a preview skips greppable.
whenEndRideMode :: Applicative m => FareComputeMode -> m () -> m ()
whenEndRideMode mode = when (mode == EndRideMode)

-- | Label for the API latency metrics of the shared fare path, so preview
-- timings don't land in the end-ride buckets.
fareComputeApiLabel :: FareComputeMode -> Text
fareComputeApiLabel = \case
  EndRideMode -> "endRide"
  PreviewMode -> "collectPayment"

-- | The fare computation result threaded through 'endRideHandler':
-- (chargeableDistance, finalFare, mbUpdatedFareParams, ride with resolved tolls,
--  pickupDropOutsideOfThreshold, distanceCalculationFailed, approx distance used on the failed-distance branch).
-- mbUpdatedFareParams is Nothing when the fare was not recomputed (estimate billed as is).
type FareComputation = (Meters, HighPrecMoney, Maybe FareParameters, Maybe DFP.FullFarePolicy, DRide.Ride, Maybe Bool, Maybe Bool, Maybe Meters)

-- | Distance travelled as per the odometer readings (in meters), validated against the start reading.
getOdometerDistance :: (MonadThrow m, Log m) => DRide.Ride -> DRide.OdometerReading -> m Meters
getOdometerDistance ride odometer = do
  let startValue = maybe 0 (.value) ride.startOdometerReading
  unless (odometer.value >= startValue) $ throwError InvalidEndOdometerReading
  pure $ Meters $ round (odometer.value - startValue) * 1000

-- | Builds the toll reconciliation input for a ride from its current detected/estimated tolls
-- plus any validated pending toll (entry gate seen, exit gate not yet). Shared by end ride,
-- the end-ride requirements check and the collect-payment preview, so all three take the
-- same toll decision. Read-only.
buildTollInput :: CacheFlow m r => DTConf.TransporterConfig -> DRide.Ride -> Bool -> Bool -> m TD.TollInput
buildTollInput thresholdConfig ride distanceCalculationFailed pickupDropOutsideOfThreshold = do
  -- Check for pending tolls (entry detected but exit not found) and validate against estimate using IDs
  mbValidatedPendingToll <-
    TollsDetector.checkAndValidatePendingTolls
      TollsDetector.TollTrackingSnapToRoad
      ride.driverId.getId
      ride.estimatedTollCharges
      ride.estimatedTollNames
      ride.estimatedTollIds
      ride.tollCharges
      ride.tollIds
  pure
    TD.TollInput
      { distanceCalculationFailed = distanceCalculationFailed,
        numberOfSelfTuned = ride.numberOfSelfTuned,
        pickupDropOutsideOfThreshold = pickupDropOutsideOfThreshold,
        estimatedTollCharges = ride.estimatedTollCharges,
        estimatedTollNames = ride.estimatedTollNames,
        estimatedTollIds = ride.estimatedTollIds,
        detectedTollCharges = ride.tollCharges,
        detectedTollNames = ride.tollNames,
        detectedTollIds = ride.tollIds,
        driverDeviatedToTollRoute = ride.driverDeviatedToTollRoute,
        validatedPendingToll = mbValidatedPendingToll,
        enableEstimatedTollFallback = (RD.mkRecomputeConfig thresholdConfig).cfgEstimatedTollFallback
      }

-- | Bills a ride from its tracked (snap-to-road) distance: resolves the tolls, then picks the
-- distance branch (rectification / failed distance calculation / correct distance calculation).
--
-- Moved verbatim out of 'endRideHandler'. The ride passed in must be the latest one:
--   * end ride: re-read after 'finalDistanceCalculation' flushed the last location points;
--   * preview:  the ride as it stands mid-trip. While the driver stays on the planned route the
--               GPS points are only buffered, not snapped, so the ride's distance is 0 and its toll
--               fields are empty until end ride. The preview fills both in the way end ride would
--               (see below): the distance driven so far from the buffered points, and the tolls
--               crossed so far from 'SharedLogic.GpsDetectedTolls'.
--
-- The manual-toll step is passed in because the callers treat an unconfirmed declaration
-- differently: end ride throws (the driver must wait or re-declare), a preview must never
-- throw and instead reports it. The resolver returns the resolution plus whether a manual
-- toll confirmation is still pending; the latter is returned as the second component.
computeTrackedDistanceFare ::
  (MonadFlow m, EndRideFlow m r, CacheFlow m r, EsqDBFlow m r) =>
  FareComputeMode ->
  ServiceHandle m ->
  SRB.Booking ->
  DRide.Ride ->
  LatLong ->
  DTConf.TransporterConfig ->
  Bool ->
  (ManualTollCharge.ManualTollChargeSignal -> m (Maybe ManualTollCharge.ManualTollChargeResolution, Bool)) ->
  m (FareComputation, Bool)
computeTrackedDistanceFare mode handle@ServiceHandle {..} booking updRide tripEndPoint thresholdConfig pickupDropOutsideOfThreshold resolveManualToll = do
  let rideId = updRide.id
      shouldRectifyDistantPointsSnapToRoadFailure = DTC.shouldRectifyDistantPointsSnapToRoadFailure booking.tripCategory
  distanceCalculationFailedSoFar <- withTimeAPI (fareComputeApiLabel mode) "isDistanceCalculationFailed" $ isDistanceCalculationFailed updRide.driverId

  -- Preview only: mid-ride ride.traveledDistance stays 0 on a non-deviated ride, and called away
  -- from the drop the preview is outside the pickup/drop threshold, so with downward recompute on it would bill ~0 m (the minimum fare). Whenever end
  -- ride would snap the GPS points (its ending branch's snap condition), bill the distance driven
  -- so far instead, as end ride would. Otherwise end ride bills the cached quote route, which the
  -- distance branches already reproduce from the estimate, so nothing changes.
  let endRideWouldSnapGps =
        pickupDropOutsideOfThreshold
          || isJust updRide.estimatedTollCharges
          || updRide.driverDeviatedToTollRoute == Just True
          || (updRide.driverDeviatedFromRoute == Just True && (RD.mkRecomputeConfig thresholdConfig).cfgRecomputeIfPickupDropNotOutsideOfThreshold)
          || shouldRectifyDistantPointsSnapToRoadFailure
  mbDrivenSoFar <-
    if mode == PreviewMode && endRideWouldSnapGps && not distanceCalculationFailedSoFar
      then do
        drivenSoFar <- estimateDistanceDrivenSoFar booking updRide
        logInfo $ "Collect payment: distance driven so far " <> show drivenSoFar.distance <> " (snapped " <> show updRide.traveledDistance <> ", gps points " <> show drivenSoFar.hasGpsPoints <> "), ride " <> rideId.getId
        pure (Just drivenSoFar)
      else pure Nothing
  -- Preview only: with no GPS points since the start (GPS off, app in the background), end ride's
  -- final snap fails, it marks the distance calculation failed and bills the approximate route
  -- (maps route from trip start to the end point). Take that same branch instead of billing 0 m.
  let previewHasNoGps = maybe False (not . (.hasGpsPoints)) mbDrivenSoFar
      distanceCalculationFailed = distanceCalculationFailedSoFar || previewHasNoGps

  when distanceCalculationFailed $ do
    logWarning $ "Failed to calculate distance for this ride: " <> rideId.getId

  -- Preview only: while the driver stays on the planned route, the ride's own toll fields stay
  -- empty until end ride runs the final toll detection, so a toll already crossed would be left
  -- out. Use the tolls the per-batch check has recorded on the driven route instead (see
  -- SharedLogic.GpsDetectedTolls). End ride keeps using the ride's tolls from its final detection.
  rideForTolls <-
    if mode == PreviewMode && DTC.isTollApplicableForTrip booking.vehicleServiceTier booking.tripCategory
      then do
        mbGpsTolls <- GpsDetectedTolls.getGpsDetectedTolls updRide.id
        -- The GPS record covers every batch; tolls already on the ride come only from batches
        -- snapped mid-ride (deviated rides), so prefer the record when it has more crossings.
        let rideCrossings = length (fromMaybe [] updRide.tollIds)
            useGps gpsTolls = isNothing updRide.tollCharges || length gpsTolls.tollIds > rideCrossings
        case mbGpsTolls of
          Just gpsTolls | useGps gpsTolls -> do
            logInfo $ "Collect payment: billing tolls crossed so far on the driven route: " <> show gpsTolls.tollNames
            pure updRide {DRide.tollCharges = Just gpsTolls.tollCharges, DRide.tollNames = Just gpsTolls.tollNames, DRide.tollIds = Just gpsTolls.tollIds}
          _ -> pure updRide
      else pure updRide

  let rideForDistance = case mbDrivenSoFar of
        Just drivenSoFar | drivenSoFar.hasGpsPoints -> updRide {DRide.traveledDistance = drivenSoFar.distance}
        _ -> updRide

  -- Toll reconciliation matrix lives in the pure, unit-tested
  -- Domain.Action.UI.Ride.EndRide.TollDecision (behavior-identical
  -- extraction of the if-tree that used to be inline here); this input also feeds
  -- the manual-toll-charge signal below.
  tollInput <- buildTollInput thresholdConfig rideForTolls distanceCalculationFailed pickupDropOutsideOfThreshold

  -- Log if we have validated toll but can't apply due to route deviation
  when (isJust tollInput.validatedPendingToll && pickupDropOutsideOfThreshold) $ do
    logWarning $ "Validated pending toll found but NOT applying due to pickup/drop outside threshold. RideId: " <> rideId.getId

  let tollBilling = TD.decideTollBilling tollInput
      manualTollChargeSignal = ManualTollCharge.ManualTollChargeSignal {tollConfidence = tollBilling.tollConfidence, hasNoTollEvidence = TD.hasNoTollEvidence tollInput}
  (mbManualTollChargeResolution, tollPendingConfirmation) <- resolveManualToll manualTollChargeSignal
  let (tollCharges, tollNames, tollIds, tollConfidence) = case mbManualTollChargeResolution of
        -- A driver-declared 0 means no toll was paid, so the estimated toll's name/id
        -- shouldn't linger on a ride the driver says had none.
        Just manualTollChargeResolution
          | manualTollChargeResolution.amount == 0 -> (Just 0, Nothing, Nothing, Just (ManualTollCharge.manualTollChargeConfidence manualTollChargeResolution.confirmedBy))
          | otherwise -> (Just manualTollChargeResolution.amount, updRide.estimatedTollNames, updRide.estimatedTollIds, Just (ManualTollCharge.manualTollChargeConfidence manualTollChargeResolution.confirmedBy))
        Nothing -> (tollBilling.tollCharges, tollBilling.tollNames, tollBilling.tollIds, tollBilling.tollConfidence)
      manualTollChargeTags =
        ( \manualTollChargeResolution ->
            ManualTollCharge.manualTollChargeTag manualTollChargeResolution.confirmedBy :
            case ManualTollCharge.manualTollChargeRejectionTag manualTollChargeResolution.rejectedAmounts of
              Just rejectionTag -> [rejectionTag]
              Nothing -> []
        )
          <$> mbManualTollChargeResolution

  -- Ride-interpolation Kafka push moved to kafka-consumers RIDE_EVENTS_CONSUMER.

  let ride = rideForDistance{DRide.tollCharges = tollCharges, DRide.tollNames = tollNames, DRide.tollIds = tollIds, DRide.tollConfidence = tollConfidence, DRide.distanceCalculationFailed = Just distanceCalculationFailed, DRide.rideTags = updRide.rideTags <> manualTollChargeTags}

  (chargeableDistance, finalFare, mbUpdatedFareParams, mbRecalcFarePolicy, mbApproxUsed) <-
    if shouldRectifyDistantPointsSnapToRoadFailure
      then do
        (d, f, p, fp) <- recalculateFareForDistance mode False handle booking ride (fromMaybe (roundToIntegral ride.traveledDistance) (bool (Just $ roundToIntegral ride.traveledDistance) booking.estimatedDistance distanceCalculationFailed)) thresholdConfig False tripEndPoint
        pure (d, f, p, fp, Nothing)
      else
        if distanceCalculationFailed
          then calculateFinalValuesForFailedDistanceCalculations mode handle booking ride tripEndPoint pickupDropOutsideOfThreshold thresholdConfig
          else do
            (d, f, p, fp) <- calculateFinalValuesForCorrectDistanceCalculations mode handle booking ride booking.maxEstimatedDistance pickupDropOutsideOfThreshold thresholdConfig tripEndPoint
            pure (d, f, p, fp, Nothing)
  pure ((chargeableDistance, finalFare, mbUpdatedFareParams, mbRecalcFarePolicy, ride, Just pickupDropOutsideOfThreshold, Just distanceCalculationFailed, mbApproxUsed), tollPendingConfirmation)

-- | What is billed on top of the computed fare: offer discount, commission and payment charge.
data FinalisedRideFare = FinalisedRideFare
  { -- | Recomputed fare params, or the booking's when the fare was not recomputed.
    baseFareParams :: FareParameters,
    discountAmount :: Maybe HighPrecMoney,
    mbFarePolicy :: Maybe DFP.FullFarePolicy,
    commission :: Maybe HighPrecMoney,
    -- | adjustedFare is the fare the rider pays (ride.fare); adjustedFareParams is its breakup.
    chargeRes :: Fare.PaymentChargeResult,
    -- | Fare params to insert: only when they were recomputed (otherwise they are the booking's existing row).
    mbFareParamsToPersist :: Maybe FareParameters
  }

-- | Moved verbatim out of 'endRideHandler'. Read-only (the offer discount refresh is a read on the BAP).
finaliseRideFare ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r) =>
  DTConf.TransporterConfig ->
  SRB.Booking ->
  HighPrecMoney ->
  Maybe FareParameters ->
  -- | Fare policy the fare was recomputed with, if it was; otherwise the booking's is looked up.
  Maybe DFP.FullFarePolicy ->
  m FinalisedRideFare
finaliseRideFare thresholdConfig booking finalFare mbUpdatedFareParams mbRecalcFarePolicy = do
  let baseFareParams = fromMaybe booking.fareParams mbUpdatedFareParams
  rawDiscountAmount <- CallBAPInternal.refreshOfferDiscount booking booking.discountAmount Nothing booking.fareParams baseFareParams
  let discountAmount = Fare.clampDiscountToDiscountable baseFareParams rawDiscountAmount
  -- Airport/parking charge is already in fareParams and finalFare from estimate/quote.
  mbFarePolicy <- maybe (FarePolicy.getFarePolicyByEstOrQuoteIdWithoutFallback booking.quoteId) (pure . Just) mbRecalcFarePolicy
  commission <-
    if DTC.isSharedRouteTrip booking.tripCategory
      then SharedRunFare.commissionAtEnd thresholdConfig baseFareParams mbFarePolicy
      else Fare.calculateCommission baseFareParams mbFarePolicy
  let appliedCharge = case mbUpdatedFareParams of
        Just recalculatedParams -> fromMaybe 0 recalculatedParams.paymentProcessingFee + fromMaybe 0 recalculatedParams.paymentProcessingFeeTax
        Nothing -> fromMaybe 0 baseFareParams.paymentProcessingFee + fromMaybe 0 baseFareParams.paymentProcessingFeeTax
      chargeRes = Fare.finalisePaymentCharge booking.paymentChargeDetails finalFare appliedCharge discountAmount baseFareParams
      mbFareParamsToPersist = chargeRes.adjustedFareParams <$ mbUpdatedFareParams
  pure FinalisedRideFare {..}

-- | When no fare product matches, bill the estimate (the given default) instead of failing.
-- Manual-toll errors are re-thrown as is, since the driver has to act on them; anything else
-- becomes an internal error. Moved out of 'endRideHandler'\'s where-clause unchanged.
withNoFareProductFallback :: (MonadFlow m) => a -> m a -> m a
withNoFareProductFallback defaultVal action = do
  res <- withTryCatch "withFallback:endRideHandler" action
  case res of
    Left someException ->
      case fromException someException of
        Just NoFareProduct -> return defaultVal
        _ -> case fromException someException of
          Just (manualTollChargeError :: ManualTollChargeError) -> throwError manualTollChargeError
          Nothing -> throwError $ InternalError (Text.pack $ displayException someException)
    Right resp -> return resp

--------------------------------------------------------------------------------
-- Collect-payment preview
--------------------------------------------------------------------------------

-- | Fare the rider would pay if the ride ended at the given point now.
data RideFarePreview = RideFarePreview
  { -- | Same number end ride would store in ride.fare (payment charge included).
    fare :: HighPrecMoney,
    -- | Breakup behind 'fare'. In memory only: the preview never writes a fare params row.
    fareParams :: FareParameters,
    chargeableDistance :: Meters,
    tollCharges :: Maybe HighPrecMoney,
    -- | Manual toll declaration is required for this ride but not yet confirmed by the rider,
    -- so the toll in 'fare' is the automatic one and may change at end ride.
    tollPendingConfirmation :: Bool
  }

-- | Collect-payment preview: distance driven so far, as end ride would measure it when it snaps
-- the GPS points, without calling snap-to-road. Batches already snapped mid-ride (deviated rides)
-- are in ride.traveledDistance; the points still buffered for the next batch are added as a path
-- from the last snapped point (or the trip start when nothing has been snapped). The points arrive
-- every few seconds, so the straight-line path stays close to the road distance.
data DrivenSoFar = DrivenSoFar
  { distance :: HighPrecMeters,
    -- | False when no GPS point has arrived since the trip start: end ride's final snap would fail.
    hasGpsPoints :: Bool
  }

estimateDistanceDrivenSoFar :: (CacheFlow m r) => SRB.Booking -> DRide.Ride -> m DrivenSoFar
estimateDistanceDrivenSoFar booking ride = do
  bufferedPoints <- LocUpdInternal.getAllWaypointsImplementation ride.driverId
  mbSnapState :: Maybe LocUpdInternal.SnapToRoadState <- Redis.safeGet (LocUpdInternal.onRideSnapToRoadStateKey ride.driverId)
  let lastSnappedPoint = mbSnapState >>= (.distanceCalcReferencePoint)
      tripStart = ride.tripStartPos <|> Just (getCoordinates booking.fromLocation)
      startPoint = lastSnappedPoint <|> (if ride.traveledDistance > 0 then Nothing else tripStart)
      path = maybeToList startPoint <> bufferedPoints
      bufferedPathLength = sum (zipWith distanceBetweenInMeters path (drop 1 path))
  -- Two points make the shortest path end ride can snap; anything less fails there.
  pure DrivenSoFar {distance = ride.traveledDistance + bufferedPathLength, hasGpsPoints = ride.traveledDistance > 0 || length bufferedPoints >= 2}

-- | Collect-payment preview: runs the end-ride fare computation on the ride as it stands, in
-- 'PreviewMode'. Read-only: it does not end the ride on the location tracking service, does not
-- flush/finalise snap-to-road or toll tracking, and writes nothing (persisting the snapshot is
-- the caller's job). The branches mirror 'endRideHandler' (driver request) one to one:
--   odometer trips -> odometer distance; everything else -> 'computeTrackedDistanceFare'.
previewRideFare ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r) =>
  ServiceHandle m ->
  SRB.Booking ->
  DRide.Ride ->
  LatLong ->
  Maybe DRide.OdometerReading ->
  m RideFarePreview
previewRideFare handle@ServiceHandle {..} booking ride tripEndPoint mbOdometer = do
  thresholdConfig <- findConfig (Just (TransactionId (Id booking.transactionId))) >>= fromMaybeM (InternalError "TransportConfigNotFound")
  let estimateFallback = ((fromMaybe 0 booking.estimatedDistance, booking.estimatedFare, Nothing, Nothing, ride, Nothing, Nothing, Nothing), False)
  -- Validated before the fallback wrapper, which would turn these 400s into a 500.
  mbOdometerDistance <-
    if DTC.isOdometerReadingsRequired booking.tripCategory
      then do
        odometer <- mbOdometer & fromMaybeM (OdometerReadingRequired (show booking.tripCategory))
        Just <$> getOdometerDistance ride odometer
      else pure Nothing
  ((chargeableDistance, finalFare, mbUpdatedFareParams, mbRecalcFarePolicy, _billedRide, _, _, _), tollPendingConfirmation) <-
    withNoFareProductFallback estimateFallback $
      case mbOdometerDistance of
        Just odometerDistance -> do
          (d, f, p, fp) <- recalculateFareForDistance PreviewMode False handle booking ride odometerDistance thresholdConfig False tripEndPoint
          pure ((d, f, p, fp, ride, Nothing, Nothing, Nothing), False)
        Nothing -> do
          pickupDropOutsideOfThreshold <- isPickupDropOutsideOfThreshold booking ride tripEndPoint thresholdConfig
          computeTrackedDistanceFare
            PreviewMode
            handle
            booking
            ride
            tripEndPoint
            thresholdConfig
            pickupDropOutsideOfThreshold
            (ManualTollCharge.resolveManualTollChargeForPreview thresholdConfig ride booking.tripCategory booking.vehicleServiceTier)
  FinalisedRideFare {chargeRes} <- finaliseRideFare thresholdConfig booking finalFare mbUpdatedFareParams mbRecalcFarePolicy
  pure
    RideFarePreview
      { fare = chargeRes.adjustedFare,
        fareParams = chargeRes.adjustedFareParams,
        chargeableDistance,
        -- Same as end ride's ride.tollCharges: the toll that actually went into the fare.
        tollCharges = mbUpdatedFareParams >>= (.tollCharges),
        tollPendingConfirmation
      }

-- | Shadow-mode evaluation of the pure recompute decision core. Read-only:
-- never affects billing; any failure is swallowed after logging. Returns
-- Nothing on the NoFareProduct fallback path (the ladder never ran there).
shadowRecomputeDecision ::
  (MonadFlow m, EndRideFlow m r, CacheFlow m r, EsqDBFlow m r) =>
  EndRideReq ->
  SRB.Booking ->
  DRide.Ride ->
  Maybe DRide.OdometerReading ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Meters ->
  Meters ->
  LatLong ->
  DTConf.TransporterConfig ->
  Maybe DFP.FullFarePolicy ->
  UTCTime ->
  m (Maybe RD.RecomputeDecision)
shadowRecomputeDecision req booking ride mbOdometer mbPickupDropOutside mbDistanceCalcFailed mbApproxUsed billedDistance tripEndPoint thresholdConfig mbFarePolicy now = do
  res <- withTryCatch "recomputeDecisionShadow" $ do
    let requestSource = case req of
          DriverReq _ -> RD.DriverSource
          DashboardReq _ -> RD.DashboardSource
          CallBasedReq _ -> RD.CallBasedSource
          CronJobReq _ -> RD.CronJobSource
        isOdometerBilled = DTC.isOdometerReadingsRequired booking.tripCategory
    if requestSource /= RD.CronJobSource && not isOdometerBilled && isNothing mbPickupDropOutside
      then pure Nothing
      else do
        passedThroughDrop <- LocUpd.isPassedThroughDrop ride.driverId
        dropOutside <- isDropOutsideOfThreshold booking tripEndPoint thresholdConfig
        -- Post-increment budget values, matching what the legacy
        -- checkExtraKmsThreshold compared against; absent daily key means the
        -- budget never applied this ride (decision treats unknown as ok).
        mbDaily :: Maybe HighPrecMeters <- Redis.get ("DailyExtraKms:PersonId-" <> ride.driverId.getId)
        weekly :: HighPrecMeters <- fromIntegral <$> SWC.getCurrentWindowCount ("WeeklyExtraKms:PersonId-" <> ride.driverId.getId) SlidingWindowOptions {period = 7, periodType = Days}
        let decision =
              RD.decideRecompute
                RD.RecomputeInput
                  { requestSource = requestSource,
                    tripCategory = booking.tripCategory,
                    isOdometerBilled = isOdometerBilled,
                    isRectificationCategory = DTC.shouldRectifyDistantPointsSnapToRoadFailure booking.tripCategory,
                    estimatedDistance = booking.estimatedDistance,
                    maxEstimatedDistance = booking.maxEstimatedDistance,
                    estimatedDuration = booking.estimatedDuration,
                    traveledDistance = ride.traveledDistance,
                    odometerDistance = mbOdometer <&> \odo -> Meters $ round (odo.value - maybe 0 (.value) ride.startOdometerReading) * 1000,
                    approxTraveledDistance = mbApproxUsed,
                    actualDuration =
                      if requestSource == RD.CronJobSource
                        then booking.estimatedDuration <|> (ride.tripStartTime <&> \startTime -> roundToIntegral $ diffUTCTime now startTime)
                        else ride.tripStartTime <&> \startTime -> roundToIntegral $ diffUTCTime now startTime,
                    distanceCalculationFailed = fromMaybe False mbDistanceCalcFailed,
                    rideFlagDistanceCalculationFailed = ride.distanceCalculationFailed,
                    pickupDropOutsideOfThreshold = fromMaybe False mbPickupDropOutside,
                    dropOutsideOfThreshold = dropOutside,
                    passedThroughDrop = passedThroughDrop,
                    budgetState = RD.ExtraKmBudgetState <$> mbDaily <*> pure weekly,
                    productFlags =
                      RD.ProductFlags
                        { disableRecompute = (mbFarePolicy >>= (.disableRecompute)) == Just True,
                          disableDownwardRecompute = (mbFarePolicy >>= (.disableDownwardRecompute)) == Just True,
                          hasPerMinRateSections =
                            maybe
                              False
                              ( \fp -> case fp.farePolicyDetails of
                                  DFP.ProgressiveDetails det -> isJust det.perMinRateSections
                                  _ -> False
                              )
                              mbFarePolicy,
                          extraTimeChargeConfigured = maybe False (\fp -> isJust fp.perMinuteRideExtraTimeCharge) mbFarePolicy,
                          timeLeversApplicable =
                            maybe
                              True
                              ( \fp -> case fp.farePolicyDetails of
                                  DFP.ProgressiveDetails _ -> True
                                  DFP.SlabsDetails _ -> True
                                  _ -> False
                              )
                              mbFarePolicy
                        },
                    earlyEndFacts = mkEarlyEndFacts booking ride tripEndPoint now (fromMaybe False mbDistanceCalcFailed) dropOutside,
                    cfg = RD.mkRecomputeConfig thresholdConfig
                  }
            mismatch = maybe False (/= billedDistance) decision.predictedChargeableDistance
        logTagInfo "RecomputeDecisionShadow" $
          "reason: " <> RD.reasonText decision
            <> ", predicted: "
            <> show decision.predictedChargeableDistance
            <> ", billed: "
            <> show billedDistance
            <> ", mismatch: "
            <> show mismatch
        when mismatch $ do
          logTagError "RecomputeDecisionShadowMismatch" $
            "predicted " <> show decision.predictedChargeableDistance <> " /= billed " <> show billedDistance <> " (reason " <> RD.reasonText decision <> ") for ride " <> ride.id.getId
          fork "recompute shadow mismatch metric" $ incrementRideEndCounter "recomputeDecisionShadowMismatch"
        pure (Just decision)
  case res of
    Left err -> do
      logTagError "recomputeDecisionShadow" $ "shadow evaluation failed: " <> show err
      pure Nothing
    Right d -> pure d

mkEarlyEndFacts :: SRB.Booking -> DRide.Ride -> LatLong -> UTCTime -> Bool -> Bool -> RD.EarlyEndFacts
mkEarlyEndFacts booking ride tripEndPoint tripEndTime distanceCalculationFailed dropOutsideOfThreshold = do
  let startPoint = fromMaybe (getCoordinates booking.fromLocation) ride.tripStartPos
  RD.EarlyEndFacts
    { eeTraveledDistance = ride.traveledDistance,
      eeDistanceFromPickup = roundToIntegral $ distanceBetweenInMeters startPoint tripEndPoint,
      eeTripDuration = ride.tripStartTime <&> \startTime -> roundToIntegral $ diffUTCTime tripEndTime startTime,
      eeDistanceCalculationFailed = distanceCalculationFailed,
      eeDropOutsideOfThreshold = dropOutsideOfThreshold
    }

recalculateFareForDistance :: (MonadThrow m, Log m, MonadTime m, MonadGuid m, EsqDBFlow m r, CacheFlow m r) => FareComputeMode -> Bool -> ServiceHandle m -> SRB.Booking -> DRide.Ride -> Meters -> DTConf.TransporterConfig -> Bool -> LatLong -> m (Meters, HighPrecMoney, Maybe FareParameters, Maybe DFP.FullFarePolicy)
recalculateFareForDistance mode useEstimatedDuration ServiceHandle {..} booking ride recalcDistance' thresholdConfig recomputeWithLatestPricing tripEndPoint = do
  tripEndTime <- getCurrentTime
  let merchantId = booking.providerId
      oldDistance = fromMaybe 0 booking.estimatedDistance -- TODO: Fix later with rentals
  passedThroughDrop <- LocUpd.isPassedThroughDrop ride.driverId
  let measuredDuration = ride.tripStartTime <&> \startTime -> roundToIntegral $ diffUTCTime tripEndTime startTime
      actualDuration = if useEstimatedDuration then booking.estimatedDuration <|> measuredDuration else measuredDuration
  pickupDropOutsideOfThreshold <- isDropOutsideOfThreshold booking tripEndPoint thresholdConfig
  -- Preview only reads the flag; the ride is stamped once, at end ride.
  whenEndRideMode mode $ QRide.updatePassedThroughDestination ride.id passedThroughDrop
  let rcfg = RD.mkRecomputeConfig thresholdConfig
  let notAllowedComponents =
        if useEstimatedDuration || DTC.isOdometerReadingsRequired booking.tripCategory
          then Nothing
          else RD.detectEndedAtPickup rcfg (mkEarlyEndFacts booking ride tripEndPoint tripEndTime (ride.distanceCalculationFailed == Just True) pickupDropOutsideOfThreshold)
      endedAtPickup = isJust notAllowedComponents
  whenJust notAllowedComponents $ \components -> logTagInfo "EndedAtPickup" $ "ride " <> ride.id.getId <> ", not billing: " <> show components
  let (recalcDistance, finalDuration) =
        if endedAtPickup
          then (roundToIntegral ride.traveledDistance, measuredDuration)
          else bool (recalcDistance', actualDuration) (oldDistance, booking.estimatedDuration) (passedThroughDrop && pickupDropOutsideOfThreshold && booking.tripCategory `elem` rcfg.cfgNoRecomputeTripCategories && ride.distanceCalculationFailed == Just False && maybe True (oldDistance >) rcfg.cfgMinThresholdForPassThroughDestination)
  let estimatedFare = Fare.fareSum booking.fareParams Nothing
      destinationWaitingTime = fromMaybe 0 $ if isNothing ride.destinationReachedAt || (not $ isUnloadingTimeRequired booking.vehicleServiceTier) then Nothing else fmap (max 0) (secondsToMinutes . roundToIntegral <$> (diffUTCTime <$> pure tripEndTime <*> ride.destinationReachedAt))
  vehicleAge <-
    if DTC.isAmbulanceTrip booking.tripCategory
      then do
        rideDetail <- QRD.findById ride.id -- replica?
        pure $ (.vehicleAge) =<< rideDetail
      else pure Nothing
  -- A scheduled booking prices against its requested pickup, not when it was created, so the fare product timeBounds window matches the estimate
  let pricingTime = Just $ if booking.isScheduled then booking.startTime else booking.createdAt
  farePolicy <-
    if recomputeWithLatestPricing
      then getFarePolicyOnEndRide pricingTime (Just $ getCoordinates booking.fromLocation) (Just . getCoordinates =<< booking.toLocation) booking.fromLocGeohash booking.toLocGeohash (Just recalcDistance) finalDuration (getCoordinates tripEndPoint) booking.merchantOperatingCityId booking.tripCategory booking.vehicleServiceTier booking.area booking.quoteId (Just booking.startTime) (Just booking.isDashboardRequest) booking.dynamicPricingLogicVersion (Just (TransactionId (Id booking.transactionId))) booking.configInExperimentVersions booking.specialLocationName
      else getFarePolicyByEstOrQuoteId pricingTime (Just $ getCoordinates booking.fromLocation) (Just . getCoordinates =<< booking.toLocation) booking.fromLocGeohash booking.toLocGeohash (Just recalcDistance) finalDuration booking.merchantOperatingCityId booking.tripCategory booking.vehicleServiceTier booking.area booking.quoteId (Just booking.startTime) (Just booking.isDashboardRequest) booking.dynamicPricingLogicVersion (Just (TransactionId (Id booking.transactionId))) booking.configInExperimentVersions booking.specialLocationName
  -- The fare policy the ride was finally billed with is recorded only when it is actually billed (end ride).
  whenEndRideMode mode $ QRide.updateFinalFarePolicyId (Just farePolicy.id) ride.id
  if farePolicy.disableRecompute == Just True && not endedAtPickup
    then return (fromMaybe 0 booking.estimatedDistance, booking.estimatedFare, Nothing, Just farePolicy)
    else do
      -- Fare can go up but never below the estimate: charge at least the estimated distance and duration
      -- Time-billing mechanism is NOT a config; precedence per fare policy:
      -- a policy that configures perMinuteRideExtraTimeCharge bills time
      -- overage through the extra-time charge alone (grace + forgiveness
      -- window deducted via the chargeable duration) and its
      -- perMinRateSections, if any, stay pinned to the ESTIMATED minutes —
      -- exactly as quoted, so sections never double-bill the overage. Only a
      -- policy WITH sections and WITHOUT the extra-time charge bills the
      -- sections on the CHARGEABLE (actual, post floor/forgiveness) minutes —
      -- fed via the calculator's duration input, so TrafficDelay basis
      -- becomes actual-minus-static.
      let hasPerMinSections = case farePolicy.farePolicyDetails of
            DFP.ProgressiveDetails det -> isJust det.perMinRateSections
            _ -> False
          extraTimeChargeConfigured = isJust farePolicy.perMinuteRideExtraTimeCharge
          sectionsBillChargeableMinutes = hasPerMinSections && not extraTimeChargeConfigured
          -- Duration levers (forgiveness/gating/floor) are CITY-pricing rules:
          -- rental/intercity/ambulance time billing is contractual
          -- (per-extra-minute, per-hour) and must never be forgiven or floored.
          timeLeversApplicable = case farePolicy.farePolicyDetails of
            DFP.ProgressiveDetails _ -> True
            DFP.SlabsDetails _ -> True
            _ -> False
          (chargeableDistance, chargeableDuration) =
            if endedAtPickup
              then (recalcDistance, finalDuration)
              else getChargeableDistanceAndDuration farePolicy.disableDownwardRecompute rcfg sectionsBillChargeableMinutes timeLeversApplicable (not pickupDropOutsideOfThreshold) booking.estimatedDuration oldDistance recalcDistance finalDuration
          durationForTimeFare =
            if endedAtPickup || sectionsBillChargeableMinutes
              then chargeableDuration <|> booking.estimatedDuration
              else booking.estimatedDuration
      stopsInfo <- if fromMaybe False ride.hasStops then QSI.findAllByRideId ride.id else return []
      mbDomainDiscountPct <- CQDDC.resolveDomainDiscountPercentage booking.merchantOperatingCityId booking.emailDomain booking.businessEmailDomain booking.billingCategory farePolicy.vehicleServiceTier
      -- Recompute congestion charge at end ride if config enabled. A ride whose
      -- congestion was REPLACED by a FareAdjustment (dpVersion "FareAdjustment:*")
      -- must keep it: the surge/json-logic recompute never ran at search time for
      -- that ride, so re-running it here would overwrite the adjusted multiplier
      -- with an outcome from an engine the ride was never priced by.
      let congestionPricedByAdjustment = maybe False ("FareAdjustment" `Text.isPrefixOf`) farePolicy.dpVersion
      (farePolicyWithCongestion, endRideCongestionCharge) <-
        if recalcDistance <= 0 && not congestionPricedByAdjustment
          then do
            logInfo $ "Zero chargeable distance, skipping congestion charge for ride: " <> ride.id.getId
            return (farePolicy, Just 0)
          else
            if rcfg.cfgRecomputeCongestionOnEndRide
              then do
                logInfo "Recomputing congestion charge on end ride"
                mbCongestionDetails <-
                  getCongestionChargeOnEndRide
                    thresholdConfig.timeDiffFromUtc
                    (Just $ getCoordinates booking.fromLocation)
                    booking.fromLocGeohash
                    booking.toLocGeohash
                    booking.vehicleServiceTier
                    (Just recalcDistance)
                    finalDuration
                    thresholdConfig.qarCalRadiusInKm
                    (FarePolicy.mkDropQARConfig thresholdConfig (Just . getCoordinates =<< booking.toLocation))
                    booking.specialLocationName
                    booking.dynamicPricingLogicVersion
                    booking.merchantOperatingCityId
                    booking.estimatedDuration
                    actualDuration
                    (Just booking.tripCategory)
                    (Just booking.transactionId)
                    booking.area
                case mbCongestionDetails of
                  Just details -> do
                    logInfo $ "End ride congestion recompute result: " <> show details
                    let updatedFarePolicy =
                          farePolicy
                            { DFP.congestionChargeMultiplier = details.congestionChargeMultiplier,
                              DFP.congestionChargePerMin = details.congestionChargePerMin
                            }
                    return (updatedFarePolicy, Nothing) -- Pass Nothing so fare calculator recomputes from updated fare policy
                  Nothing -> do
                    logInfo "End ride congestion recompute returned Nothing, falling back to booking estimate"
                    return (farePolicy, booking.estimatedCongestionCharge)
              else return (farePolicy, booking.estimatedCongestionCharge)
      let farePolicy' =
            farePolicyWithCongestion
              { DFP.businessDiscountPercentage = mbDomainDiscountPct <|> farePolicy.businessDiscountPercentage,
                DFP.personalDiscountPercentage = mbDomainDiscountPct <|> farePolicy.personalDiscountPercentage
              } ::
              DFP.FullFarePolicy
      rcParkingFeeExempt <-
        if fromMaybe False farePolicy'.parkingFeeExemptionEnabled
          then do
            mbRideDetails <- QRD.findById ride.id
            SPFE.isParkingFeeExemptByVehicleNumber ((.vehicleNumber) <$> mbRideDetails)
          else pure False
      mbExcessLuggageHalfUnits <- SPL.findExcessLuggageHalfUnits booking.merchantOperatingCityId booking.vehicleServiceTier booking.numberOfCheckInBags booking.numberOfCabinBags
      fareParams <-
        calculateFareParameters
          Fare.CalculateFareParametersParams
            { farePolicy = farePolicy',
              computationPhase = Fare.FCRecompute notAllowedComponents,
              mbCapConfig = farePolicy.fareRecomputeCapConfig,
              mbEstimateFareParams = Just booking.fareParams,
              paymentChargeDetails = booking.paymentChargeDetails,
              buyerOfferDiscount = booking.discountAmount,
              actualDistance = Just chargeableDistance,
              estimatedDistance = Just oldDistance,
              rideTime = booking.startTime,
              returnTime = booking.returnTime,
              roundTrip = fromMaybe False booking.roundTrip,
              waitingTime = fmap (destinationWaitingTime +) $ if isNothing ride.driverArrivalTime then Nothing else fmap (max 0) (secondsToMinutesCeil . roundToIntegral <$> (diffUTCTime <$> ride.tripStartTime <*> (liftA2 max ride.driverArrivalTime (Just booking.startTime)))),
              stopWaitingTimes = stopsInfo <&> (\stopInfo -> max 0 (secondsToMinutesCeil $ roundToIntegral (diffUTCTime (fromMaybe stopInfo.waitingTimeStart stopInfo.waitingTimeEnd) stopInfo.waitingTimeStart))),
              actualRideDuration = chargeableDuration,
              estimatedRideDuration = durationForTimeFare,
              estimatedRideStaticDuration = booking.estimatedStaticDuration,
              driverSelectedFare = booking.fareParams.driverSelectedFare,
              customerExtraFee = booking.fareParams.customerExtraFee,
              negativeFareAdjustment = booking.fareParams.negativeFareAdjustment,
              nightShiftCharge = booking.fareParams.nightShiftCharge,
              petCharges = booking.fareParams.petCharges,
              addOnCharges = booking.fareParams.addOnCharges,
              negotiatedFareDelta = booking.fareParams.negotiatedFareDelta,
              estimatedCongestionCharge = endRideCongestionCharge,
              isScheduled = booking.isScheduled,
              customerCancellationDues = booking.fareParams.customerCancellationDues,
              customerCancellationDuesTaxBreakup = (Fare.fareTaxesOf booking.fareParams).cancellationFee,
              nightShiftOverlapChecking = DTC.isFixedNightCharge booking.tripCategory,
              timeDiffFromUtc = Just thresholdConfig.timeDiffFromUtc,
              tollCharges = ride.tollCharges,
              isManualTollCharge = ManualTollCharge.isManualTollChargeRide ride,
              vehicleAge = vehicleAge,
              currency = booking.currency,
              noOfStops = length ride.stops,
              distanceUnit = booking.distanceUnit,
              shouldApplyBusinessDiscount = booking.billingCategory == SLT.BUSINESS,
              shouldApplyPersonalDiscount = booking.billingCategory == SLT.PERSONAL,
              merchantOperatingCityId = booking.merchantOperatingCityId,
              mbAdditonalChargeCategories = Just $ map (.chargeCategory) booking.fareParams.conditionalCharges,
              numberOfLuggages = booking.numberOfLuggages,
              excessLuggageHalfUnits = mbExcessLuggageHalfUnits,
              pickupGateId = booking.pickupGateId,
              fareSettlementType = booking.fareSettlementType,
              isParkingFeeExempt = rcParkingFeeExempt
            }
      finalFare <- checkRecomputedFareCeiling booking fareParams
      let distanceDiff = chargeableDistance - oldDistance
          fareDiff = finalFare - estimatedFare
      logTagInfo "Fare recalculation" $
        "Fare difference: "
          <> show (realToFrac @_ @Double fareDiff)
          <> ", Distance difference: "
          <> show distanceDiff
      -- Estimate-vs-final metric: a preview is not a final fare, so it must not skew it.
      whenEndRideMode mode $ putDiffMetric merchantId fareDiff distanceDiff
      return (chargeableDistance, finalFare, Just fareParams, Just farePolicy)

checkRecomputedFareCeiling :: (Log m, Monad m) => SRB.Booking -> FareParameters -> m HighPrecMoney
checkRecomputedFareCeiling booking fareParams = do
  let finalFare = Fare.fareSum fareParams Nothing
  whenJust booking.fareParams.bufferedFare $ \ceiling' ->
    when (finalFare > ceiling') $
      logTagError "Fare recompute cap BREACH" $
        "Recomputed fare " <> show finalFare <> " above buffered ceiling " <> show ceiling' <> " for booking " <> booking.id.getId <> " -- a component without a configured cap strategy grew past its estimate; the fare policy needs a cap for it."
  pure finalFare

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

isDownwardRecomputeEnabledForRide :: SRB.Booking -> DTConf.TransporterConfig -> Bool
isDownwardRecomputeEnabledForRide booking thresholdConfig = do
  let rcfg = RD.mkRecomputeConfig thresholdConfig
  booking.tripCategory `notElem` rcfg.cfgNoRecomputeTripCategories || fromMaybe True rcfg.cfgEnableDownwardRecomputeForDifferentDestination

getChargeableDistanceAndDuration :: Maybe Bool -> RD.RecomputeConfig -> Bool -> Bool -> Bool -> Maybe Seconds -> Meters -> Meters -> Maybe Seconds -> (Meters, Maybe Seconds)
getChargeableDistanceAndDuration disableDownwardRecompute rcfg sectionsBillChargeableMinutes timeLeversApplicable withinThreshold estimatedDuration estimatedDistance recalcDistance finalDuration =
  (chargeableDistance, adjustedDuration)
  where
    (chargeableDistance, baseDuration)
      | disableDownwardRecompute == Just True = (max recalcDistance estimatedDistance, flooredDuration)
      | downwardRecomputeTolerance = (estimatedDistance, flooredDuration)
      | otherwise = (recalcDistance, finalDuration)
    flooredDuration = (max <$> finalDuration <*> estimatedDuration) <|> finalDuration
    downwardRecomputeTolerance =
      recalcDistance < estimatedDistance
        && metersToHighPrecMeters (estimatedDistance - recalcDistance) < fromMaybe 0 rcfg.cfgDownwardRecomputeDistanceThreshold
    -- Duration levers. Billing the estimated duration keeps
    -- calculateExtraTimeFare at zero: it only charges actual > estimated + grace.
    isFloored = disableDownwardRecompute == Just True || downwardRecomputeTolerance
    overran = fromMaybe False ((>) <$> finalDuration <*> estimatedDuration)
    -- Estimate-billed rides must not bill the per-minute extra-time charge
    -- when the gating lever is on: the distance ladder already decided to
    -- forgive this ride.
    gated = timeLeversApplicable && rcfg.cfgGateExtraTimeChargeByRecompute && chargeableDistance == estimatedDistance && overran
    -- The matching forgiveness window is deducted from an overrun's billed
    -- duration, clamped at the estimate: overage inside the window bills the
    -- estimate, overage beyond it bills only the excess past the window
    -- (city pricing only; rental/intercity time is contractual).
    mbForgivenessAdjusted =
      if timeLeversApplicable
        then RD.forgivenessAdjustedDuration rcfg.cfgTimeForgivenessBands rcfg.cfgMaxChargeableOverageSeconds estimatedDuration finalDuration
        else Nothing
    -- Never bill below the estimated duration: within the pickup/drop
    -- threshold by default, outside it only when configured. Binds only when
    -- perMinRateSections actually bill the chargeable minutes (otherwise no
    -- fare effect on city rides; would inflate rental billing).
    underran = fromMaybe False ((<) <$> finalDuration <*> estimatedDuration)
    floorTimeEnabled = if withinThreshold then rcfg.cfgFloorTimeAtEstimateWithinThreshold else rcfg.cfgFloorTimeAtEstimateOutsideThreshold
    timeFloored = floorTimeEnabled && sectionsBillChargeableMinutes && not isFloored && underran
    adjustedDuration
      | gated || timeFloored = estimatedDuration
      | Just adj <- mbForgivenessAdjusted = Just adj
      | otherwise = baseDuration

calculateFinalValuesForCorrectDistanceCalculations ::
  (MonadFlow m, MonadThrow m, Log m, MonadTime m, MonadGuid m, EsqDBFlow m r, CacheFlow m r, Redis.HedisLTSFlowEnv r) => FareComputeMode -> ServiceHandle m -> SRB.Booking -> DRide.Ride -> Maybe HighPrecMeters -> Bool -> DTConf.TransporterConfig -> LatLong -> m (Meters, HighPrecMoney, Maybe FareParameters, Maybe DFP.FullFarePolicy)
calculateFinalValuesForCorrectDistanceCalculations mode handle booking ride mbMaxDistance pickupDropOutsideOfThreshold thresholdConfig tripEndPoint = do
  distanceDiff <- getDistanceDiff booking (highPrecMetersToMeters ride.traveledDistance)
  let estimatedDistance = fromMaybe 0 booking.estimatedDistance -- TODO: Fix with rentals
      shouldRecompute = RD.recomputeBandQualifies rcfg.cfgRecomputeThresholds estimatedDistance (highPrecMetersToMeters distanceDiff)
      -- Outside the threshold a sub-band |delta| keeps the estimate: upward
      -- bands gate an overage, downward bands a shortfall.
      upwardQualifies = RD.recomputeBandQualifies rcfg.cfgRecomputeThresholds estimatedDistance (abs (highPrecMetersToMeters distanceDiff))
      shortfallQualifies = RD.recomputeBandQualifies rcfg.cfgDownwardRecomputeThresholds estimatedDistance (abs (highPrecMetersToMeters distanceDiff))
      thresholdChecks = rcfg.cfgRecomputeIfPickupDropNotOutsideOfThreshold && shouldRecompute
  (mbDailyExtraKms, mbWeeklyExtraKms) <- if thresholdChecks then handleExtraKmsRecomputation distanceDiff else return (Nothing, Nothing)
  -- The over-budget overlay is an end-ride outcome; a preview must not show it.
  whenEndRideMode mode $
    fork "Send Extra Kms Limit Exceeded Overlay" $
      when (rcfg.cfgNotifyDriverOnBudgetExceeded && not (checkExtraKmsThreshold mbDailyExtraKms mbWeeklyExtraKms)) notifyDriverOnExtraKmsLimitExceed
  let maxDistance = fromMaybe ride.traveledDistance mbMaxDistance + maxUpwardBuffer
  if not pickupDropOutsideOfThreshold
    then
      if thresholdChecks && checkExtraKmsThreshold mbDailyExtraKms mbWeeklyExtraKms
        then recalculateFareForDistance mode False handle booking ride (roundToIntegral $ min ride.traveledDistance maxDistance) thresholdConfig False tripEndPoint
        else recalculateFareForDistance mode False handle booking ride estimatedDistance thresholdConfig False tripEndPoint
    else
      if distanceDiff < 0
        then
          if isDownwardRecomputeEnabledForRide booking thresholdConfig
            then
              if shortfallQualifies
                then recalculateFareForDistance mode False handle booking ride (roundToIntegral ride.traveledDistance) thresholdConfig True tripEndPoint
                else recalculateFareForDistance mode False handle booking ride estimatedDistance thresholdConfig True tripEndPoint
            else recalculateFareForDistance mode False handle booking ride estimatedDistance thresholdConfig False tripEndPoint
        else
          if upwardQualifies
            then recalculateFareForDistance mode False handle booking ride (roundToIntegral ride.traveledDistance) thresholdConfig True tripEndPoint
            else recalculateFareForDistance mode False handle booking ride estimatedDistance thresholdConfig True tripEndPoint
  where
    rcfg = RD.mkRecomputeConfig thresholdConfig
    maxUpwardBuffer = case (booking.estimatedDistance, rcfg.cfgUpwardsRecomputeBufferPercentage) of
      (Just estDistance, Just percentage) -> HighPrecMeters (max (fromRational $ (toRational estDistance.getMeters) * (toRational percentage / 100)) rcfg.cfgUpwardsRecomputeBuffer.getHighPrecMeters)
      _ -> rcfg.cfgUpwardsRecomputeBuffer
    makeDailyAndWeeklyExtraKmsKey personId = ("DailyExtraKms:PersonId-" <> personId, "WeeklyExtraKms:PersonId-" <> personId)

    checkExtraKmsThreshold (Just dailyExtraKms) (Just weeklyExtraKms) = rcfg.cfgFareRecomputeDailyExtraKmsThreshold >= dailyExtraKms && rcfg.cfgFareRecomputeWeeklyExtraKmsThreshold >= weeklyExtraKms
    checkExtraKmsThreshold _ _ = True

    handleExtraKmsRecomputation distanceDiff = do
      expirationPeriodForDay <- DC.getExpirationSeconds thresholdConfig.timeDiffFromUtc
      let (dailyExtraKmsKey, weeklyExtraKmsKey) = makeDailyAndWeeklyExtraKmsKey ride.driverId.getId
      prevDailyExtraKms <- Redis.get dailyExtraKmsKey
      prevWeeklyExtraKms <- fromIntegral <$> SWC.getCurrentWindowCount weeklyExtraKmsKey SlidingWindowOptions {period = 7, periodType = Days}
      let dailyExtraKms = fromMaybe 0 prevDailyExtraKms + distanceDiff
          weeklyExtraKms = (prevWeeklyExtraKms :: HighPrecMeters) + distanceDiff
      -- Preview evaluates the budget with this ride's extra km included (the same
      -- post-increment values end ride compares against) but must not spend it:
      -- otherwise every collect-payment call, and then end ride, would each add the
      -- same extra km to the driver's daily/weekly budget.
      whenEndRideMode mode $ do
        Redis.setExp dailyExtraKmsKey dailyExtraKms expirationPeriodForDay
        SWC.incrementByValue (round distanceDiff) weeklyExtraKmsKey SlidingWindowOptions {period = 7, periodType = Days}
        QDI.updateDailyAndWeeklyExtraKms (Just dailyExtraKms) (Just weeklyExtraKms) ride.driverId
      pure (Just dailyExtraKms, Just weeklyExtraKms)

    notifyDriverOnExtraKmsLimitExceed = do
      driver <- QP.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
      overlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategoryInRideFlow booking.merchantOperatingCityId "EXTRA_KMS_LIMIT_EXCEEDED" (fromMaybe ENGLISH driver.language) Nothing Nothing booking.configInExperimentVersions >>= fromMaybeM (OverlayKeyNotFound "EXTRA_KMS_LIMIT_EXCEEDED")
      TN.sendOverlay booking.merchantOperatingCityId driver $ TN.mkOverlayReq overlay

calculateFinalValuesForFailedDistanceCalculations ::
  (MonadThrow m, Log m, MonadTime m, MonadGuid m, EsqDBFlow m r, CacheFlow m r) => FareComputeMode -> ServiceHandle m -> SRB.Booking -> DRide.Ride -> LatLong -> Bool -> DTConf.TransporterConfig -> m (Meters, HighPrecMoney, Maybe FareParameters, Maybe DFP.FullFarePolicy, Maybe Meters)
calculateFinalValuesForFailedDistanceCalculations mode handle@ServiceHandle {..} booking ride tripEndPoint pickupDropOutsideOfThreshold thresholdConfig = do
  let tripStartPoint = case ride.tripStartPos of
        Nothing -> getCoordinates booking.fromLocation
        Just tripStartPos -> tripStartPos
  interpolatedPoints <- getInterpolatedPoints ride.driverId
  let estimatedDistance = fromMaybe 0 booking.estimatedDistance -- TODO: Fix with rentals
  if not pickupDropOutsideOfThreshold
    then do
      (d, f, p, fp) <- recalculateFareForDistance mode False handle booking ride estimatedDistance thresholdConfig False tripEndPoint -- TODO: Fix with rentals
      pure (d, f, p, fp, Nothing)
    else do
      (_routePoints, approxTraveledDistance) <- getRouteAndDistanceBetweenPoints (Just $ ride.id.getId <> LocUpd.approxRouteEntityIdSuffix) tripStartPoint tripEndPoint interpolatedPoints estimatedDistance
      logTagInfo "endRide" $ "approxTraveledDistance when pickup and drop are not outside threshold: " <> show approxTraveledDistance
      distanceDiff <- getDistanceDiff booking approxTraveledDistance
      -- On the approx route a sub-band delta keeps the estimate: downward
      -- bands gate a shortfall, upward bands an overage.
      let upwardQualifies = RD.recomputeBandQualifies rcfg.cfgRecomputeThresholds estimatedDistance (abs (highPrecMetersToMeters distanceDiff))
          shortfallQualifies = RD.recomputeBandQualifies rcfg.cfgDownwardRecomputeThresholds estimatedDistance (abs (highPrecMetersToMeters distanceDiff))
      (d, f, p, fp) <-
        if distanceDiff < 0
          then do
            if isDownwardRecomputeEnabledForRide booking thresholdConfig
              then
                if shortfallQualifies
                  then recalculateFareForDistance mode False handle booking ride approxTraveledDistance thresholdConfig True tripEndPoint -- TODO :: Recompute Toll Charges Here ?
                  else recalculateFareForDistance mode False handle booking ride estimatedDistance thresholdConfig True tripEndPoint
              else recalculateFareForDistance mode False handle booking ride estimatedDistance thresholdConfig False tripEndPoint
          else
            if not upwardQualifies
              then do
                recalculateFareForDistance mode False handle booking ride estimatedDistance thresholdConfig True tripEndPoint
              else do
                if highPrecMetersToMeters distanceDiff < maxDistance
                  then recalculateFareForDistance mode False handle booking ride approxTraveledDistance thresholdConfig True tripEndPoint -- TODO :: Recompute Toll Charges Here ?
                  else do
                    logTagInfo "Inaccurate Location Updates and Pickup/Drop Deviated." ("DistanceDiff: " <> show distanceDiff)
                    recalculateFareForDistance mode False handle booking ride (estimatedDistance + maxDistance) thresholdConfig True tripEndPoint
      -- threaded out so the shadow decision sees the same approximation the
      -- billing branch used (otherwise every failed-outside ride would be
      -- persisted as FailedOutsideUnknownApprox and never comparable)
      pure (d, f, p, fp, Just approxTraveledDistance)
  where
    rcfg = RD.mkRecomputeConfig thresholdConfig
    maxDistance = case (booking.estimatedDistance, rcfg.cfgUpwardsRecomputeBufferPercentage) of
      (Just estDistance, Just percentage) -> Meters $ max (round $ (toRational estDistance.getMeters) * (toRational percentage / 100)) (round rcfg.cfgUpwardsRecomputeBuffer.getHighPrecMeters)
      _ -> highPrecMetersToMeters rcfg.cfgUpwardsRecomputeBuffer

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

mkDriverAirportDropKey :: Text -> Text
mkDriverAirportDropKey driverId = "Driver:AirportDrop:" <> driverId
