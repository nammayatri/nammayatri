-- | Consumer-side handlers for RideEndedEvent.
--
-- Each handler replaces one of the original synchronous calls / forks in EndRide.hs
-- and EndRide/Internal.hs. The handler looks up the Ride and Booking from the event's
-- IDs and then invokes the same domain functions the original code did.
--
-- Lives in kafka-consumers so handler iteration doesn't trigger a full driver-app
-- rebuild — driver-app exports the needed primitives via its public API.
module Processor.RideEvents.Handlers
  ( handleAnalyticsKafka,
    handleRideInterpolation,
    handleNammaTags,
    handleFleetOperatorStats,
    handleGpsTollBehavior,
    handleRCStatsReminders,
    handleRideEndNotifications,
    handleLeaderboard,
    handleReferral,
  )
where

import qualified Data.Aeson as A
import Data.Time (addUTCTime, diffUTCTime, utctDay)
import "dynamic-offer-driver-app" Domain.Action.UI.Ride.EndRide (RideInterpolationData (..))
import qualified "dynamic-offer-driver-app" Domain.Types.Booking as SRB
import "dynamic-offer-driver-app" Domain.Types.Event.RideEndedEvent (RideEndedEvent (..))
import qualified "dynamic-offer-driver-app" Domain.Types.Ride as Ride
import qualified "dynamic-offer-driver-app" Domain.Types.RideRelatedNotificationConfig as DRN
import "dynamic-offer-driver-app" Domain.Types.TransporterConfig (TransporterConfig)
import qualified "dynamic-offer-driver-app" Domain.Types.Yudhishthira as Y
import Kernel.Beam.Lib.Utils (pushToKafka)
import Kernel.External.Encryption (EncFlow)
import qualified Kernel.External.Encryption as EncFlow
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CHConfig
import qualified Kernel.Storage.ClickhouseV2 as CHV2
import qualified Kernel.Storage.Esqueleto.Config as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer, KafkaProducerTools)
import qualified Kernel.Tools.Metrics.CoreMetrics as CoreMetrics
import Kernel.Types.Common (MonadFlow)
import Kernel.Types.Confidence (Confidence (..))
import Kernel.Types.Id
import Kernel.Utils.Common
  ( CacheFlow,
    HasShortDurationRetryCfg,
    fromMaybeM,
    getCurrentTime,
    getLocalCurrentTime,
    logInfo,
    logWarning,
    withTryCatch,
  )
import Kernel.Utils.Time (secondsToNominalDiffTime)
import qualified Lib.BehaviorEngine.Orchestrator as BEOrch
import qualified Lib.BehaviorTracker.Snapshot as BTSnap
import qualified Lib.BehaviorTracker.Types as BTT
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.LocationUpdates.Internal as LU
import Lib.Scheduler.Environment (JobCreator)
import Lib.SessionizerMetrics.Types.Event (EventStreamFlow)
import Lib.Yudhishthira.Storage.Beam.BeamFlow (HasYudhishthiraTablesSchema)
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types as Yudhishthira
import qualified Processor.RideEvents.InternalHelpers as IH
import qualified "dynamic-offer-driver-app" SharedLogic.Analytics as Analytics
import qualified "dynamic-offer-driver-app" SharedLogic.BehaviourManagement.ConsequenceDispatcher as BehaviorDispatch
import qualified "dynamic-offer-driver-app" SharedLogic.External.LocationTrackingService.Types as LT
import qualified "dynamic-offer-driver-app" SharedLogic.FleetVehicleStats as FVS
import "dynamic-offer-driver-app" SharedLogic.Reminder.Helper (checkAndCreateRemindersForRidesThreshold)
import qualified "dynamic-offer-driver-app" SharedLogic.ScheduledNotifications as SN
import qualified "dynamic-offer-driver-app" Storage.Cac.TransporterConfig as SCTC
import qualified "dynamic-offer-driver-app" Storage.CachedQueries.RideRelatedNotificationConfig as CRN
import qualified "dynamic-offer-driver-app" Storage.Queries.Booking as QRB
import qualified "dynamic-offer-driver-app" Storage.Queries.DriverRCAssociation as QDRCA
import qualified "dynamic-offer-driver-app" Storage.Queries.DriverStats as QDriverStats
import qualified "dynamic-offer-driver-app" Storage.Queries.Person as QPerson
import qualified "dynamic-offer-driver-app" Storage.Queries.RCStatsExtra as QRCStats
import qualified "dynamic-offer-driver-app" Storage.Queries.Ride as QRide
import qualified "dynamic-offer-driver-app" Storage.Queries.RiderDetails as QRiderDetails
import qualified "dynamic-offer-driver-app" Tools.ActorInfo as ActorInfo
import qualified Tools.DynamicLogic as DL
import "dynamic-offer-driver-app" Tools.Error
import "dynamic-offer-driver-app" Tools.Event (BookingEventData (..), RideEventData (..))
import qualified "dynamic-offer-driver-app" Tools.Event as Event

------------------------------------------------------------
-- Helpers
------------------------------------------------------------

-- | Look up the Ride and Booking referenced by the event. If either lookup fails,
-- the handler is a no-op and logs a warning — the event is acked, not retried.
withRideAndBooking ::
  (CacheFlow m r, Esq.EsqDBFlow m r, MonadFlow m) =>
  RideEndedEvent ->
  (Ride.Ride -> SRB.Booking -> m ()) ->
  m ()
withRideAndBooking ev action = do
  let rideId = Id ev.rideId :: Id Ride.Ride
  mbRide <- QRide.findById rideId
  case mbRide of
    Nothing -> logWarning $ "ride-events handler: ride not found rideId=" <> ev.rideId
    Just ride -> do
      mbBooking <- QRB.findById ride.bookingId
      case mbBooking of
        Nothing -> logWarning $ "ride-events handler: booking not found bookingId=" <> ride.bookingId.getId
        Just booking -> action ride booking

fetchTransporterConfig ::
  (CacheFlow m r, Esq.EsqDBFlow m r, MonadFlow m) =>
  Ride.Ride ->
  m TransporterConfig
fetchTransporterConfig ride =
  SCTC.findByMerchantOpCityId ride.merchantOperatingCityId Nothing
    >>= fromMaybeM (TransporterConfigNotFound ride.merchantOperatingCityId.getId)

------------------------------------------------------------
-- P1b-1 : Analytics Kafka events
------------------------------------------------------------

handleAnalyticsKafka ::
  (CacheFlow m r, Esq.EsqDBFlow m r, MonadFlow m, EventStreamFlow m r) =>
  RideEndedEvent ->
  m ()
handleAnalyticsKafka ev = withRideAndBooking ev $ \ride booking -> do
  Event.triggerRideEndEvent
    RideEventData
      { ride = ride {Ride.status = Ride.COMPLETED},
        personId = ride.driverId,
        merchantId = booking.providerId
      }
  Event.triggerBookingCompletedEvent
    BookingEventData
      { booking = booking {SRB.status = SRB.COMPLETED},
        personId = ride.driverId,
        merchantId = booking.providerId
      }

------------------------------------------------------------
-- P1b-2 : Ride interpolation Kafka push
------------------------------------------------------------

handleRideInterpolation ::
  ( CacheFlow m r,
    Esq.EsqDBFlow m r,
    MonadFlow m,
    Redis.HedisFlow m r,
    HasField "kafkaProducerTools" r KafkaProducerTools
  ) =>
  RideEndedEvent ->
  m ()
handleRideInterpolation ev = withRideAndBooking ev $ \ride _booking -> do
  interpolatedPoints <- LU.getInterpolatedPointsImplementation ride.driverId
  let rideInterpolationData = RideInterpolationData {interpolatedPoints = interpolatedPoints, rideId = ride.id}
  let tollCharges = ride.tollCharges
      estTolls = ride.estimatedTollCharges
      tollChargesMismatch =
        fromMaybe False $
          ((,) <$> tollCharges <*> estTolls)
            <&> \(detected, estimated) -> detected /= estimated
  when
    ( isJust ride.driverDeviatedToTollRoute
        && ride.tollConfidence == Just Sure
        && ( (maybe True (== 0) tollCharges && isJust estTolls)
               || tollChargesMismatch
           )
    )
    $ pushToKafka rideInterpolationData "ride-interpolated-waypoints" ride.id.getId

------------------------------------------------------------
-- P1b-3 : LYDL Namma Tags
------------------------------------------------------------

handleNammaTags ::
  ( CacheFlow m r,
    Esq.EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    MonadFlow m,
    CoreMetrics.CoreMetrics m,
    CHConfig.ClickhouseFlow m r,
    HasYudhishthiraTablesSchema
  ) =>
  RideEndedEvent ->
  m ()
handleNammaTags ev = withRideAndBooking ev $ \ride booking -> do
  thresholdConfig <- fetchTransporterConfig ride
  mbDriver <- QPerson.findById ride.driverId
  mbRiderDetails <- join <$> QRiderDetails.findById `mapM` booking.riderId
  riderBlockedForCoins <- QRiderDetails.isRiderFlaggedForCoinZero booking.riderId
  let mbDriverMobileHash = (.hash) <$> (mbDriver >>= (.mobileNumber))
      mbRiderMobileHash = (.hash) . (.mobileNumber) <$> mbRiderDetails
      isDriverSameAsCustomer =
        isJust mbDriverMobileHash
          && isJust mbRiderMobileHash
          && mbDriverMobileHash == mbRiderMobileHash
  now <- getCurrentTime
  let merchantLocalDay = utctDay $ addUTCTime (secondsToNominalDiffTime thresholdConfig.timeDiffFromUtc) now
  priorRidesSameCustomer <-
    QRide.countPriorCompletedRidesWithSameCustomer
      (cast ride.driverId)
      booking.riderId
      ride.id
      merchantLocalDay
      thresholdConfig.sameRiderDriverRideCountLookbackDays
  let shouldBlockCoinsForSameRiderFlow =
        riderBlockedForCoins
          || priorRidesSameCustomer > thresholdConfig.sameRiderDriverRideCountThreshold
      rideDurationSeconds =
        maybe 0 (\tStart -> max 0 $ roundToIntegral (diffUTCTime now tStart)) ride.tripStartTime
  void $
    withTryCatch "ride-events:computeNammaTags" $
      LYDL.computeNammaTagsWithDebugLog
        LYDL.Driver
        (cast booking.merchantOperatingCityId)
        Yudhishthira.RideEnd
        (Just booking.transactionId)
        (Y.EndRideTagData ride booking isDriverSameAsCustomer shouldBlockCoinsForSameRiderFlow rideDurationSeconds)

------------------------------------------------------------
-- P1b-4 : Fleet + Operator analytics
------------------------------------------------------------

handleFleetOperatorStats ::
  ( CacheFlow m r,
    Esq.EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    MonadFlow m,
    Redis.HedisFlow m r,
    CoreMetrics.CoreMetrics m,
    EncFlow m r,
    CHConfig.ClickhouseFlow m r
  ) =>
  RideEndedEvent ->
  m ()
handleFleetOperatorStats ev = withRideAndBooking ev $ \ride booking -> do
  thresholdConfig <- fetchTransporterConfig ride
  when thresholdConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $ do
    Analytics.updateOperatorAnalyticsTotalRideCount thresholdConfig ride.driverId ride booking
    whenJust ride.fleetOwnerId $ \fleetOwnerId ->
      FVS.updateFleetVehicleDailyStats fleetOwnerId.getId thresholdConfig ride

------------------------------------------------------------
-- P1b-5 : GPS Toll Behavior
------------------------------------------------------------

handleGpsTollBehavior ::
  ( CacheFlow m r,
    Esq.EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    MonadFlow m,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    CoreMetrics.CoreMetrics m,
    CHConfig.ClickhouseFlow m r,
    HasYudhishthiraTablesSchema,
    LT.HasLocationService m r,
    JobCreator r m,
    HasShortDurationRetryCfg r c
  ) =>
  RideEndedEvent ->
  m ()
handleGpsTollBehavior ev = withRideAndBooking ev $ \ride booking -> do
  thresholdConfig <- fetchTransporterConfig ride
  when thresholdConfig.enableGpsTollBehavior $ do
    let isTollRide = isJust ride.estimatedTollCharges || isJust ride.tollCharges
        gpsTurnedOff = fromMaybe False ride.driverGpsTurnedOff
    when isTollRide $ do
      logInfo $
        "GPS toll behavior check for DriverId: " <> ride.driverId.getId
          <> ", RideId: "
          <> ride.id.getId
      let windowDays = fromMaybe 15 thresholdConfig.gpsTollBehaviorWindowDays
          counterConfig =
            BTT.CounterConfig
              { windowSizeDays = 30,
                counters = [BTT.ACTION_COUNT],
                periods = [BTT.mkPeriodConfig "window" (toInteger windowDays)]
              }
      eventTime <- getCurrentTime
      let actionEvent =
            BTT.ActionEvent
              { entityType = BTT.DRIVER,
                entityId = ride.driverId.getId,
                actionType = "GPS_TOLL_BAD_BEHAVIOR",
                merchantOperatingCityId = booking.merchantOperatingCityId.getId,
                flowContext = A.object [],
                eventData =
                  A.object
                    [ "estimatedTollCharges" A..= ride.estimatedTollCharges,
                      "estimatedTollNames" A..= ride.estimatedTollNames,
                      "estimatedTollIds" A..= ride.estimatedTollIds,
                      "detectedTollCharges" A..= ride.tollCharges,
                      "detectedTollNames" A..= ride.tollNames,
                      "detectedTollIds" A..= ride.tollIds,
                      "gpsTurnedOffInCurrentRide" A..= gpsTurnedOff
                    ],
                timestamp = eventTime
              }
          entityState = A.object []
          fetchRules domain = do
            localTime <- getLocalCurrentTime thresholdConfig.timeDiffFromUtc
            DL.getAppDynamicLogic (cast booking.merchantOperatingCityId) domain localTime Nothing Nothing
      snapshot <- BTSnap.buildSnapshot counterConfig actionEvent entityState
      output <-
        BEOrch.orchestrate
          snapshot
          LYDL.Driver
          (cast booking.merchantOperatingCityId)
          LYT.GPS_TOLL_BEHAVIOR
          fetchRules
      logInfo $
        "GPS Toll Behavior evaluation result: consequences="
          <> show (length output.consequences)
          <> ", communications="
          <> show (length output.communications)
      let dispatchCtx =
            BehaviorDispatch.DispatchContext
              { merchantId = booking.providerId,
                merchantOperatingCityId = booking.merchantOperatingCityId,
                counterConfig = Just counterConfig,
                actionEvent = Just actionEvent
              }
      BehaviorDispatch.handleConsequences dispatchCtx (cast ride.driverId) output.consequences
      BehaviorDispatch.handleCommunications (cast ride.driverId) output.communications

------------------------------------------------------------
-- P1b-6 : RC stats + reminders
------------------------------------------------------------

handleRCStatsReminders ::
  ( CacheFlow m r,
    Esq.EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    MonadFlow m,
    Redis.HedisFlow m r,
    EncFlow m r,
    CoreMetrics.CoreMetrics m,
    HasKafkaProducer r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  ) =>
  RideEndedEvent ->
  m ()
handleRCStatsReminders ev = withRideAndBooking ev $ \ride booking -> do
  mbDriverStats <- QDriverStats.findById (cast ride.driverId)
  let driverRideCount = maybe 0 (.totalRides) mbDriverStats
  mbRCAssoc <- QDRCA.findActiveAssociationByDriver (cast ride.driverId) True
  mbRCRideCount <- case mbRCAssoc of
    Just rcAssoc -> Just <$> QRCStats.incrementTotalRides rcAssoc.rcId
    Nothing -> pure Nothing
  checkAndCreateRemindersForRidesThreshold
    (cast ride.driverId)
    driverRideCount
    mbRCAssoc
    mbRCRideCount
    booking.merchantOperatingCityId
    booking.providerId

------------------------------------------------------------
-- P1b-7 : Ride-end scheduled notifications
------------------------------------------------------------

handleRideEndNotifications ::
  ( CacheFlow m r,
    Esq.EsqDBFlow m r,
    MonadFlow m,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  ) =>
  RideEndedEvent ->
  m ()
handleRideEndNotifications ev = withRideAndBooking ev $ \ride booking -> do
  now <- getCurrentTime
  rideRelatedNotificationConfigList <-
    CRN.findAllByMerchantOperatingCityIdAndTimeDiffEventInRideFlow
      booking.merchantOperatingCityId
      DRN.END_TIME
      booking.configInExperimentVersions
  forM_ rideRelatedNotificationConfigList $
    SN.pushReminderUpdatesInScheduler booking ride now (cast ride.driverId)

------------------------------------------------------------
-- P1b-8 : Leaderboard
------------------------------------------------------------

handleLeaderboard ::
  (CacheFlow m r, Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, MonadFlow m) =>
  RideEndedEvent ->
  m ()
handleLeaderboard ev = withRideAndBooking ev $ \ride booking ->
  when
    ( ev.isValidRide
        && ( ride.traveledDistance > 1000
               || (fromMaybe False ride.distanceCalculationFailed && fromMaybe 0 ride.chargeableDistance > 1000)
           )
    )
    $ IH.updateLeaderboardZScore booking ride

------------------------------------------------------------
-- P1b-10 : Referral FCM + driver-to-driver referral reward
------------------------------------------------------------

handleReferral ::
  ( CacheFlow m r,
    Esq.EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    EncFlow.EncFlow m r,
    Finance.HasActorInfo m r,
    CoreMetrics.CoreMetrics m,
    CHConfig.ClickhouseFlow m r,
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    Redis.HedisLTSFlowEnv r
  ) =>
  RideEndedEvent ->
  m ()
handleReferral ev = ActorInfo.withMbActorInfo ev.actorInfo . withRideAndBooking ev $ \ride booking -> do
  thresholdConfig <- fetchTransporterConfig ride
  mbRiderDetails <- join <$> QRiderDetails.findById `mapM` booking.riderId
  IH.sendReferralFCM ev.isValidRide ride booking mbRiderDetails thresholdConfig
  IH.sendDriverToDriverReferralReward ev.isValidRide ride booking mbRiderDetails thresholdConfig
