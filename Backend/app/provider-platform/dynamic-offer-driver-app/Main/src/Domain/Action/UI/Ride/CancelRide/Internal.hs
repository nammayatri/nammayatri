{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Ride.CancelRide.Internal
  ( cancelRideImpl,
    updateNammaTagsForCancelledRide,
    driverDistanceToPickup,
    getCancellationCharges,
    customerCancellationChargesCalculation,
    getDistanceToPickup,
    buildPenaltyCheckContext,
  )
where

import Data.Aeson as A
import Data.Either.Extra (eitherToMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.CancellationReason as DTCR
import Domain.Types.DriverLocation
import qualified Domain.Types.Merchant as DMerc
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderDetails as RiderDetails
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.Yudhishthira as TY
import EulerHS.Prelude hiding (whenJust)
import Kernel.External.Maps
import Kernel.Prelude hiding (any, elem, map, notElem)
import qualified Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.ClickhouseV2 as CHV2
import qualified Kernel.Storage.Esqueleto as Esq hiding (whenJust_)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer, KafkaProducerTools)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Coins as DC
import qualified Lib.DriverCoins.Types as DCT
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import Lib.Scheduler (SchedulerType)
import Lib.SessionizerMetrics.Types.Event
-- import qualified Lib.Yudhishthira.Event as Yudhishthira
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
-- import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types as Yudhishthira
import qualified SharedLogic.Analytics as Analytics
import qualified SharedLogic.CallBAP as BP
import SharedLogic.CallBAPInternal
import qualified SharedLogic.CallInternalMLPricing as ML
import SharedLogic.Cancel
import qualified SharedLogic.DriverCancellationPenalty as DCP
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.GoogleTranslate (TranslateFlow)
import SharedLogic.Ride (releaseLien, updateOnRideStatusWithAdvancedRideCheck)
import SharedLogic.RuleBasedTierUpgrade
import qualified SharedLogic.UserCancellationDues as UserCancellationDues
import qualified Storage.Cac.TransporterConfig as CCT
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRiderDetails
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Constants
import Tools.DynamicLogic
import Tools.Error
import Tools.Event
import qualified Tools.Maps as Maps
import qualified Tools.Metrics as Metrics
import qualified Tools.Notifications as Notify
import TransactionLogs.Types
import Utils.Common.Cac.KeyNameConstants
import Kernel.Storage.Clickhouse.Config

cancelRideImpl ::
  ( MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasKafkaProducer r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "singleBatchProcessingTempDelay" r NominalDiffTime,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig],
    TranslateFlow m r,
    LT.HasLocationService m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasShortDurationRetryCfg r c,
    Redis.HedisFlow m r,
    EventStreamFlow m r,
    Metrics.HasCoreMetrics r,
    HasShortDurationRetryCfg r c,
    HasField "enableAPILatencyLogging" r Bool,
    HasField "enableAPIPrometheusMetricLogging" r Bool,
    HasFlowEnv m r '["appBackendBapInternal" ::: AppBackendBapInternal],
    HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal],
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    HasField "blackListedJobs" r [Text],
    ClickhouseFlow m r
  ) =>
  Id DRide.Ride ->
  DRide.RideEndedBy ->
  SBCR.BookingCancellationReason ->
  Bool ->
  Maybe Bool ->
  m ()
cancelRideImpl rideId rideEndedBy bookingCReason isForceReallocation doCancellationRateBasedBlocking = do
  isLocked <- Redis.tryLockRedis (buildCancelRideTransactionKey rideId) 15
  if isLocked
    then do
      finally
        ( do
            ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
            booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
            isValueAddNP <- CQVAN.isValueAddNP booking.bapId
            let merchantId = booking.providerId
            merchant <-
              CQM.findById merchantId
                >>= fromMaybeM (MerchantNotFound merchantId.getId)
            transporterConfig <- CCT.findByMerchantOpCityId booking.merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
            rideTags <- updateNammaTagsForCancelledRide booking ride bookingCReason transporterConfig
            noShowCharges <- withTryCatch "noShowCharges:cancelRideImpl" $ do
              if transporterConfig.canAddCancellationFee
                then do
                  let tagsForCancellationCharges = [validCustomerCancellation, validUserNoShowCancellation]
                  let cancellationType =
                        if bookingCReason.source == SBCR.ByDriver && validUserNoShowCancellation `elem` rideTags
                          then DCT.CancellationByDriver
                          else DCT.CancellationByCustomer
                  if any (`elem` rideTags) tagsForCancellationCharges
                    then getCancellationCharges booking ride cancellationType bookingCReason.reasonCode
                    else return Nothing
                else return Nothing
            userNoShowCharges <- case noShowCharges of
              Left e -> do
                logError $ "Error in getting no show charges - " <> show e
                return Nothing
              Right (charges :: Maybe PriceAPIEntity) -> return charges
            -- calculateNoShowCharges booking ride else return Nothing
            driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
            vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (DriverWithoutVehicle ride.driverId.getId)
            unless (isValidRide ride) $ throwError (InternalError "Ride is not valid for cancellation")
            cancelRideTransaction booking ride bookingCReason merchant rideEndedBy userNoShowCharges transporterConfig
            logTagInfo ("rideId-" <> getId rideId) ("Cancellation reason " <> show bookingCReason.source)

            fork "DriverRideCancelledCoin Event : " $ do
              mbLocation <- do
                driverLocations <- LF.driversLocation [ride.driverId]
                return $ listToMaybe driverLocations
              disToPickup <- forM mbLocation $ \location -> do
                driverDistanceToPickup booking (getCoordinates location) (getCoordinates booking.fromLocation)
              when (bookingCReason.source == SBCR.ByDriver) $
                DC.driverCoinsEvent ride.driverId driver.merchantId booking.merchantOperatingCityId (DCT.Cancellation ride.createdAt booking.distanceToPickup disToPickup DCT.CancellationByDriver (fromMaybe (DTCR.CancellationReasonCode "OTHER") bookingCReason.reasonCode)) (Just ride.id.getId) ride.vehicleVariant (Just booking.configInExperimentVersions)

            fork "cancelRide - Notify driver" $ do
              triggerRideCancelledEvent RideEventData {ride = ride{status = DRide.CANCELLED}, personId = driver.id, merchantId = merchantId}
              triggerBookingCancelledEvent BookingEventData {booking = booking{status = SRB.CANCELLED}, personId = driver.id, merchantId = merchantId}
              when (bookingCReason.source == SBCR.ByDriver) $ do
                DS.driverScoreEventHandler ride.merchantOperatingCityId DST.OnDriverCancellation {rideTags, merchantId = merchantId, driver = driver, rideFare = Just booking.estimatedFare, currency = booking.currency, distanceUnit = booking.distanceUnit, doCancellationRateBasedBlocking}
                DCP.accumulateCancellationPenalty booking ride rideTags transporterConfig
              Notify.notifyOnCancel ride.merchantOperatingCityId booking driver bookingCReason.source
            fork "cancelRide/ReAllocate - Notify BAP" $ do
              isReallocated <- reAllocateBookingIfPossible isValueAddNP False merchant booking ride driver vehicle bookingCReason isForceReallocation
              unless isReallocated $ BP.sendBookingCancelledUpdateToBAP booking merchant bookingCReason.source userNoShowCharges
            computeEligibleUpgradeTiers ride transporterConfig
        )
        ( do
            logDebug $ "CancelRideTransaction:RID:-" <> rideId.getId <> " Unlocked"
            Redis.unlockRedis (buildCancelRideTransactionKey rideId)
        )
    else throwError (InternalError "Ride is already cancelled")
  where
    buildCancelRideTransactionKey rideId' = "CancelRideTransaction:RID:-" <> rideId'.getId
    isValidRide ride = ride.status `elem` [DRide.NEW, DRide.UPCOMING]

-- calculateNoShowCharges :: (MonadFlow m, CacheFlow m r) => SRB.Booking -> DRide.Ride -> m (Maybe PriceAPIEntity)
-- calculateNoShowCharges booking ride = do
--   mbFullFarePolicy <- SFP.getFarePolicyByEstOrQuoteIdWithoutFallback booking.quoteId
--   let mbCancellationAndNoShowConfigs :: Maybe DTC.CancellationFarePolicy = (.cancellationFarePolicy) =<< mbFullFarePolicy
--   now <- getCurrentTime
--   logInfo $ "Params passed to calculateNoShowCharges: driverArrivalTime" <> show ride.driverArrivalTime <> " | cancellationAndNoShowConfigs: " <> show mbCancellationAndNoShowConfigs <> "| current time: " <> show now
--   let cancellationCharges = FareCalculator.calculateNoShowCharges ride.driverArrivalTime mbCancellationAndNoShowConfigs now
--   case cancellationCharges of
--     Just cancellationFee -> do
--       return $ Just PriceAPIEntity {amount = cancellationFee, currency = booking.currency}
--     _ -> return Nothing

cancelRideTransaction ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Esq.EsqDBReplicaFlow m r,
    LT.HasLocationService m r,
    HasShortDurationRetryCfg r c
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  SBCR.BookingCancellationReason ->
  DMerc.Merchant ->
  DRide.RideEndedBy ->
  Maybe PriceAPIEntity ->
  DTC.TransporterConfig ->
  m ()
cancelRideTransaction booking ride bookingCReason merchant rideEndedBy cancellationFee transporterConfig = do
  let driverId = cast ride.driverId
      isPrepaidSubscriptionAndWalletEnabled = fromMaybe False merchant.prepaidSubscriptionAndWalletEnabled
  when (isPrepaidSubscriptionAndWalletEnabled && isJust transporterConfig.subscriptionConfig.fleetPrepaidSubscriptionThreshold) $ releaseLien booking ride
  void $ CQDGR.setDriverGoHomeIsOnRideStatus ride.driverId booking.merchantOperatingCityId False
  updateOnRideStatusWithAdvancedRideCheck driverId (Just ride)
  when booking.isScheduled $ QDI.updateLatestScheduledBookingAndPickup Nothing Nothing driverId
  void $ LF.rideDetails ride.id DRide.CANCELLED merchant.id ride.driverId booking.fromLocation.lat booking.fromLocation.lon Nothing (Just $ (LT.Car $ LT.CarRideInfo {pickupLocation = LatLong (booking.fromLocation.lat) (booking.fromLocation.lon), minDistanceBetweenTwoPoints = Nothing, rideStops = Just $ map (\stop -> LatLong stop.lat stop.lon) booking.stops}))
  void $ QRide.updateStatusAndRideEndedBy ride.id DRide.CANCELLED rideEndedBy
  QBCR.upsert bookingCReason
  void $ QRB.updateStatus booking.id SRB.CANCELLED
  when (bookingCReason.source == SBCR.ByDriver) $ QDriverStats.updateIdleTime driverId
  case (cancellationFee, booking.riderId) of
    (Just fee, Just rid) -> do
      QRide.updateCancellationFeeIfCancelledField (Just fee.amount) ride.id
      riderDetails <- QRiderDetails.findById rid >>= fromMaybeM (RiderDetailsNotFound rid.getId)
      void $ QRiderDetails.updateCancellationDues (fee.amount + riderDetails.cancellationDues) rid
      QRiderDetails.updateValidCancellationsCount rid.getId
    _ -> do
      logError "cancelRideTransaction: riderId in booking or cancellationFee is not present"

updateNammaTagsForCancelledRide ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Esq.EsqDBReplicaFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    ClickhouseFlow m r
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  SBCR.BookingCancellationReason ->
  DTC.TransporterConfig ->
  m [LYT.TagNameValue]
updateNammaTagsForCancelledRide booking ride bookingCReason transporterConfig = do
  now <- getCurrentTime
  mbCallStatus <- QCallStatus.findOneByEntityId (Just ride.id.getId)
  let callAtemptByDriver = isJust mbCallStatus
      currentTime = floor $ utcTimeToPOSIXSeconds now
      rideCreatedTime = floor $ utcTimeToPOSIXSeconds ride.createdAt
      driverArrivalTime = floor . utcTimeToPOSIXSeconds <$> (ride.driverArrivalTime)
      tagData =
        TY.CancelRideTagData
          { ride = ride{status = DRide.CANCELLED},
            booking = booking{status = SRB.CANCELLED},
            cancellationReason = bookingCReason,
            merchantOperatingCityId = booking.merchantOperatingCityId,
            ..
          }
  nammaTags <- withTryCatch "computeNammaTags:RideCancel" (LYDL.computeNammaTagsWithDebugLog (cast booking.merchantOperatingCityId) Yudhishthira.RideCancel tagData)
  logDebug $ "Tags for cancelled ride, rideId: " <> ride.id.getId <> " tagresults:" <> show (eitherToMaybe nammaTags) <> "| tagdata: " <> show tagData
  let allTags = ride.rideTags <> eitherToMaybe nammaTags
  QRide.updateRideTags allTags ride.id
  let tags = fromMaybe [] allTags
  when (bookingCReason.reasonCode == Just userNoShowCancellationReason && validUserNoShowCancellation `notElem` tags) $
    logError $ "Customer no show tag was not applied: rideId: " <> ride.id.getId
  Analytics.updateCancellationAnalyticsAndDriverStats transporterConfig ride bookingCReason
  return $ fromMaybe [] allTags

driverDistanceToPickup ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Maps.HasCoordinates tripStartPos,
    Maps.HasCoordinates tripEndPos,
    ToJSON tripStartPos,
    ToJSON tripEndPos,
    HasKafkaProducer r
  ) =>
  SRB.Booking ->
  tripStartPos ->
  tripEndPos ->
  m Meters
driverDistanceToPickup booking tripStartPos tripEndPos = do
  distRes <-
    Maps.getDistanceForCancelRide booking.providerId booking.merchantOperatingCityId (Just booking.id.getId) $
      Maps.GetDistanceReq
        { origin = tripStartPos,
          destination = tripEndPos,
          travelMode = Just Maps.CAR,
          distanceUnit = booking.distanceUnit,
          sourceDestinationMapping = Nothing
        }
  return $ distRes.distance

getDistanceToPickup ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    HasField "shortDurationRetryCfg" r RetryCfg,
    EncFlow m r,
    HasKafkaProducer r,
    HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig],
    Esq.EsqDBReplicaFlow m r
  ) =>
  SRB.Booking ->
  Maybe DRide.Ride ->
  m (Maybe Meters, Maybe DriverLocation)
getDistanceToPickup booking mbRide = do
  case mbRide of
    Just ride -> do
      mbLocation <- do
        driverLocations <- withTryCatch "driversLocation:getDistanceToPickup" $ LF.driversLocation [ride.driverId]
        case driverLocations of
          Left err -> do
            logError ("Failed to fetch Driver Location with error : " <> show err)
            return Nothing
          Right locations -> return $ listToMaybe locations
      case mbLocation of
        Just location -> do
          distance <- driverDistanceToPickup booking (getCoordinates location) (getCoordinates booking.fromLocation)
          return (Just distance, Just location)
        Nothing -> return (Nothing, Nothing)
    _ -> return (Nothing, Nothing)

customerCancellationChargesCalculation ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    HasKafkaProducer r,
    HasField "shortDurationRetryCfg" r RetryCfg,
    HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig],
    Esq.EsqDBReplicaFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    ClickhouseFlow m r
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  RiderDetails.RiderDetails ->
  DCT.CancellationType ->
  Maybe DTCR.CancellationReasonCode ->
  m (Maybe HighPrecMoney)
customerCancellationChargesCalculation booking ride riderDetails cancellationType reasonCode = do
  transporterConfig <- CCT.findByMerchantOpCityId booking.merchantOperatingCityId (Just (TransactionId (Id booking.transactionId))) >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
  (cancellationDisToPickup, _mbLocation) <- getDistanceToPickup booking (Just ride)
  now <- getCurrentTime
  driverQuote <- QDQ.findById (Id booking.quoteId) >>= fromMaybeM (QuoteNotFound booking.quoteId)
  let estimatedTimeToPickup = secondsToNominalDiffTime driverQuote.durationToPickup
  mbCallStatus <- QCallStatus.findOneByEntityId (Just ride.id.getId)
  let callAttemptByDriver = isJust mbCallStatus
  let isArrivedAtPickup = case cancellationDisToPickup of
        Just disToPickup' -> disToPickup' < highPrecMetersToMeters transporterConfig.arrivedPickupThreshold
        Nothing -> False
      timeOfCancellation = round $ diffUTCTime now ride.createdAt
      initialDisToPickup = booking.distanceToPickup
      actualCoveredDistance = case (initialDisToPickup, cancellationDisToPickup) of
        (Just initial, Just cancellation) -> Just (initial - cancellation)
        _ -> Nothing
      expectedCoveredDistance =
        if isJust initialDisToPickup
          then
            let initialDistance = fromJust initialDisToPickup
                progressRatio = fromIntegral timeOfCancellation / max 1 estimatedTimeToPickup
                expectedDistance = round $ fromIntegral initialDistance * progressRatio
             in Just expectedDistance
          else Nothing
      driverWaitingTime = if isJust ride.driverArrivalTime then Just (round $ diffUTCTime now (fromJust ride.driverArrivalTime)) else Nothing
  let logicInput =
        UserCancellationDues.UserCancellationDuesData
          { cancelledBy = cancellationType,
            timeOfDriverCancellation = timeOfCancellation,
            timeOfCustomerCancellation = timeOfCancellation,
            isArrivedAtPickup = isArrivedAtPickup,
            driverWaitingTime = driverWaitingTime,
            callAttemptByDriver = callAttemptByDriver,
            actualCoveredDistance = actualCoveredDistance,
            expectedCoveredDistance = expectedCoveredDistance,
            cancellationDues = riderDetails.cancellationDues,
            cancelledRides = riderDetails.cancelledRides,
            totalBookings = riderDetails.totalBookings,
            completedRides = riderDetails.completedRides,
            validCancellations = riderDetails.validCancellations,
            cancellationDueRides = riderDetails.cancellationDueRides,
            serviceTier = booking.vehicleServiceTier,
            tripCategory = booking.tripCategory,
            cancellationReasonSelected = reasonCode
          }
  if transporterConfig.canAddCancellationFee
    then do
      localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
      (allLogics, _mbVersion) <- getAppDynamicLogic (cast ride.merchantOperatingCityId) LYT.USER_CANCELLATION_DUES localTime Nothing Nothing
      logDebug $ "allLogics: for cancellation charges calculation" <> show allLogics
      logDebug $ "logicInput: for cancellation charges calculation" <> show logicInput
      response <- withTryCatch "runLogics:UserCancellationDues" $ LYDL.runLogicsWithDebugLog (cast ride.merchantOperatingCityId) LYT.USER_CANCELLATION_DUES allLogics logicInput
      cancellationCharge <- case response of
        Left e -> do
          logError $ "Error in running UserCancellationDuesLogics - " <> show e <> " - " <> show logicInput <> " - " <> show allLogics
          return (Just 0)
        Right resp -> do
          case (A.fromJSON resp.result :: Result UserCancellationDues.UserCancellationDuesResult) of
            A.Success result -> do
              logTagInfo ("bookingId-" <> getId booking.id) ("result.cancellationCharges: " <> show result.cancellationCharges)
              return (Just result.cancellationCharges)
            A.Error e -> do
              logError $ "Error in parsing UserCancellationDuesResult - " <> show e <> " - " <> show resp.result <> " - " <> show logicInput <> " - " <> show allLogics
              return (Just 0)
      when (reasonCode == Just userNoShowCancellationReason && fromMaybe 0 cancellationCharge <= 0) $
        logError $ "User no show charges was not applied: " <> show cancellationCharge <> ": rideId: " <> ride.id.getId <> ". Please check dynamic logic"
      pure cancellationCharge
    else return Nothing

userNoShowCancellationReason :: DTCR.CancellationReasonCode
userNoShowCancellationReason = DTCR.CancellationReasonCode "CUSTOMER_NO_SHOW"

getCancellationCharges ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    HasKafkaProducer r,
    Esq.EsqDBReplicaFlow m r,
    HasField "shortDurationRetryCfg" r RetryCfg,
    HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig],
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    ClickhouseFlow m r
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  DCT.CancellationType ->
  Maybe DTCR.CancellationReasonCode ->
  m (Maybe PriceAPIEntity)
getCancellationCharges booking ride cancellationType reasonCode = do
  transporterConfig <- CCT.findByMerchantOpCityId booking.merchantOperatingCityId (Just (TransactionId (Id booking.transactionId))) >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
  case booking.riderId of
    Nothing -> return Nothing
    Just rid -> do
      riderDetails <- QRiderDetails.findById rid >>= fromMaybeM (RiderDetailsNotFound rid.getId)
      if transporterConfig.canAddCancellationFee
        then do
          charges' <- customerCancellationChargesCalculation booking ride riderDetails cancellationType reasonCode
          case charges' of
            Just charges -> do
              return $ Just $ PriceAPIEntity {amount = charges, currency = booking.currency}
            Nothing -> return Nothing
        else return Nothing

buildPenaltyCheckContext ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HasShortDurationRetryCfg r c,
    EncFlow m r,
    LT.HasLocationService m r,
    HasKafkaProducer r
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  LatLong ->
  m TY.PenaltyCheckTagData
buildPenaltyCheckContext booking ride point = do
  now <- getCurrentTime
  numberOfCallAttempts <- QCallStatus.countCallsByEntityId ride.id

  driverDistanceFromPickupNow <- do
    distRes <- withTryCatch "driverDistanceToPickup:buildPenaltyCheckContext" $ driverDistanceToPickup booking (getCoordinates point) (getCoordinates booking.fromLocation)
    either
      ( \err -> do
          logError ("PenaltyCheckError: Failed to calculate Driver Distance to Pickup with error : " <> show err)
          return Nothing
      )
      (\d -> return $ Just d)
      distRes

  let driverDistanceFromPickupAtAcceptance = booking.distanceToPickup

  let currentTime = floor $ utcTimeToPOSIXSeconds now
      rideCreatedTime = floor $ utcTimeToPOSIXSeconds ride.createdAt
      driverArrivedAtPickup = isJust ride.driverArrivalTime

  return $
    TY.PenaltyCheckTagData
      { ride = ride,
        booking = booking,
        currentTime,
        rideCreatedTime,
        driverArrivedAtPickup,
        driverDistanceFromPickupNow,
        driverDistanceFromPickupAtAcceptance,
        numberOfCallAttempts
      }
