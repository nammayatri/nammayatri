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
    handleFleetOperatorStats,
    handleGpsTollBehavior,
    handleRCStatsReminders,
    handleRideEndNotifications,
    handleLeaderboard,
    handleReferral,
    handleDriverCityMigration,
    handleDriverCoinsAndJourney,
    handleUpgradeTiers,
    handleCallBasedEndRideSms,
    handleDeferredPanVerification,
    handleGoogleMobilityBilling,
  )
where

import qualified Data.Aeson as A
import qualified "dynamic-offer-driver-app" Domain.Action.UI.DriverOnboarding.PanVerification as PanVerification
import "dynamic-offer-driver-app" Domain.Action.UI.Ride.EndRide (RideInterpolationData (..))
import qualified "dynamic-offer-driver-app" Domain.Types.Booking as SRB
import qualified "dynamic-offer-driver-app" Domain.Types.DocumentVerificationConfig as DTO
import "dynamic-offer-driver-app" Domain.Types.Event.RideEndedEvent (RideEndedEvent (..))
import qualified "dynamic-offer-driver-app" Domain.Types.Ride as Ride
import qualified "dynamic-offer-driver-app" Domain.Types.RideRelatedNotificationConfig as DRN
import "dynamic-offer-driver-app" Domain.Types.TransporterConfig (TransporterConfig)
import qualified "beckn-spec" Domain.Types.Trip as DTrip
import qualified "beckn-spec" Domain.Types.VehicleCategory as DVC
import Kernel.Beam.Lib.Utils (pushToKafka)
import Kernel.External.Encryption (EncFlow)
import qualified Kernel.External.Encryption as EncFlow
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import qualified Kernel.Storage.Clickhouse.Config as CHConfig
import qualified Kernel.Storage.ClickhouseV2 as CHV2
import qualified Kernel.Storage.Esqueleto.Config as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer, KafkaProducerTools)
import qualified Kernel.Tools.Metrics.CoreMetrics as CoreMetrics
import Kernel.Types.Common (MonadFlow)
import Kernel.Types.Confidence (Confidence (..))
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Field ((:::))
import Kernel.Types.Id
import Kernel.Utils.Common
  ( CacheFlow,
    HasFlowEnv,
    HasRequestId,
    HasShortDurationRetryCfg,
    fromMaybeM,
    getCurrentTime,
    getLocalCurrentTime,
    logInfo,
    logWarning,
  )
import qualified Lib.BehaviorEngine.Orchestrator as BEOrch
import qualified Lib.BehaviorTracker.Snapshot as BTSnap
import qualified Lib.BehaviorTracker.Types as BTT
import "config-pilot" Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.LocationUpdates.Internal as LU
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import Lib.Scheduler.Environment (JobCreator)
import Lib.SessionizerMetrics.Types.Event (EventStreamFlow)
import Lib.Yudhishthira.Storage.Beam.BeamFlow (HasYudhishthiraTablesSchema)
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import qualified Processor.RideEvents.InternalHelpers as IH
import qualified "dynamic-offer-driver-app" SharedLogic.Analytics as Analytics
import qualified "dynamic-offer-driver-app" SharedLogic.BehaviourManagement.ConsequenceDispatcher as BehaviorDispatch
import qualified "dynamic-offer-driver-app" SharedLogic.DriverCityMigration as DriverCityMigration
import qualified "dynamic-offer-driver-app" SharedLogic.External.LocationTrackingService.Types as LT
import qualified "dynamic-offer-driver-app" SharedLogic.FleetOperatorStats as SFleetOperatorStats
import qualified "dynamic-offer-driver-app" SharedLogic.FleetVehicleStats as FVS
import qualified "dynamic-offer-driver-app" SharedLogic.GoogleMobilityBilling as GoogleMobilityBilling
import "dynamic-offer-driver-app" SharedLogic.Reminder.Helper (checkAndCreateRemindersForRidesThreshold)
import qualified "dynamic-offer-driver-app" SharedLogic.RideEvents.DriverCoinsAndJourney as DriverCoinsAndJourney
import "dynamic-offer-driver-app" SharedLogic.RuleBasedTierUpgrade (computeEligibleUpgradeTiersSync)
import qualified "dynamic-offer-driver-app" SharedLogic.ScheduledNotifications as SN
import "dynamic-offer-driver-app" Storage.Beam.Payment ()
import qualified "dynamic-offer-driver-app" Storage.CachedQueries.RideRelatedNotificationConfig as CRN
import "dynamic-offer-driver-app" Storage.ConfigPilot.Config.DocumentVerificationConfig (DocumentVerificationConfigDimensions (..))
import "dynamic-offer-driver-app" Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified "dynamic-offer-driver-app" Storage.Queries.Booking as QRB
import qualified "dynamic-offer-driver-app" Storage.Queries.DriverPanCard as DPQuery
import qualified "dynamic-offer-driver-app" Storage.Queries.DriverRCAssociation as QDRCA
import qualified "dynamic-offer-driver-app" Storage.Queries.DriverStats as QDriverStats
import qualified "dynamic-offer-driver-app" Storage.Queries.IdfyVerificationExtra as IVQueryExtra
import qualified "dynamic-offer-driver-app" Storage.Queries.Person as QPerson
import qualified "dynamic-offer-driver-app" Storage.Queries.RCStatsExtra as QRCStats
import qualified "dynamic-offer-driver-app" Storage.Queries.Ride as QRide
import qualified "dynamic-offer-driver-app" Storage.Queries.RiderDetails as QRiderDetails
import qualified "dynamic-offer-driver-app" Tools.ActorInfo as ActorInfo
import qualified Tools.DynamicLogic as DL
import "dynamic-offer-driver-app" Tools.Error
import "dynamic-offer-driver-app" Tools.Event (BookingEventData (..), RideEventData (..))
import qualified "dynamic-offer-driver-app" Tools.Event as Event
import qualified "dynamic-offer-driver-app" Tools.SMS as Sms

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
  getConfig (TransporterConfigDimensions {merchantOperatingCityId = ride.merchantOperatingCityId.getId}) Nothing
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
-- P1b-4 : Fleet + Operator analytics
------------------------------------------------------------

handleFleetOperatorStats ::
  ( CacheFlow m r,
    Esq.EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    MonadFlow m,
    Redis.HedisFlow m r,
    Analytics.PublishesFleetAnalytics m,
    CoreMetrics.CoreMetrics m,
    EncFlow m r,
    CHConfig.ClickhouseFlow m r
  ) =>
  RideEndedEvent ->
  m ()
handleFleetOperatorStats ev = withRideAndBooking ev $ \ride booking -> do
  thresholdConfig <- fetchTransporterConfig ride
  when thresholdConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $ do
    rideStats <- SFleetOperatorStats.mkCompletedRideStats ride booking
    Analytics.recordFleetOperatorAnalytics thresholdConfig (Analytics.RideCompleted ride.driverId rideStats)
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
                periods = [BTT.mkPeriodConfig "window" (toInteger windowDays)],
                hashTagEntityId = False
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
          (BehaviorDispatch.behaviourEventCity booking.merchantOperatingCityId)
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
      BehaviorDispatch.handleCommunications dispatchCtx (cast ride.driverId) output.communications

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

------------------------------------------------------------
-- P1b-11 : Driver operating-city migration on relocation
------------------------------------------------------------

-- | Keeps a driver's operating city in sync with where their rides actually originate.
-- Reads ride.merchantOperatingCityId (the city the ride was booked/dispatched in) --
-- for the same-city rides this handler processes, that's the city the driver is
-- actually operating in for this ride. Skipped for InterCity/CrossCity rides, which are
-- *expected* to run outside the driver's operating city and don't by themselves imply
-- the driver has relocated there -- genuine relocation is still caught by the driver's
-- subsequent *local* rides in the new city, which are never tagged either way.
--
-- The entire migration (auth-layer sync, notification, billing tag, doc-gap
-- notifications, operational-table sync) runs inline via
-- SharedLogic.DriverCityMigration.migrateDriverOperatingCity, which holds a per-driver
-- Redis lock only while the migration runs. That prevents concurrent migrations for the
-- same driver; it does not debounce, so every qualifying ride in a new city migrates.
handleDriverCityMigration ::
  ( CacheFlow m r,
    Esq.EsqDBFlow m r,
    MonadFlow m,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  RideEndedEvent ->
  m ()
handleDriverCityMigration ev = withRideAndBooking ev $ \ride booking -> do
  let isInterOrCrossCityRide = case ride.tripCategory of
        DTrip.InterCity _ _ -> True
        DTrip.CrossCity _ _ -> True
        _ -> False
  unless isInterOrCrossCityRide $ do
    let targetOpCityId = ride.merchantOperatingCityId
    driverPerson <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
    when (driverPerson.merchantOperatingCityId /= targetOpCityId) $
      DriverCityMigration.migrateDriverOperatingCity driverPerson booking.providerId targetOpCityId

handleDriverCoinsAndJourney ::
  ( CacheFlow m r,
    Esq.EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    EncFlow.EncFlow m r,
    Finance.HasActorInfo m r,
    CHConfig.ClickhouseFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    ServiceFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    PaymentBeamFlow.BeamFlow m r,
    BeamFlow m r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  ) =>
  RideEndedEvent ->
  m ()
handleDriverCoinsAndJourney ev = ActorInfo.withMbActorInfo ev.actorInfo . withRideAndBooking ev $ \ride booking -> do
  thresholdConfig <- fetchTransporterConfig ride
  DriverCoinsAndJourney.processRideEndedCoinsAndJourney ride booking thresholdConfig

handleUpgradeTiers ::
  ( CacheFlow m r,
    Esq.EsqDBFlow m r,
    MonadFlow m,
    CHConfig.ClickhouseFlow m r
  ) =>
  RideEndedEvent ->
  m ()
handleUpgradeTiers ev = withRideAndBooking ev $ \ride _booking -> do
  thresholdConfig <- fetchTransporterConfig ride
  computeEligibleUpgradeTiersSync ride thresholdConfig

handleCallBasedEndRideSms ::
  ( CacheFlow m r,
    Esq.EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    MonadFlow m,
    ServiceFlow m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig]
  ) =>
  RideEndedEvent ->
  m ()
handleCallBasedEndRideSms ev = withRideAndBooking ev $ \ride booking ->
  when (ride.rideEndedBy == Just Ride.CallBased) $ do
    driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
    Sms.sendDashboardSms driver.merchantId booking.merchantOperatingCityId Sms.ENDRIDE (Just ride) ride.driverId (Just booking) (fromMaybe (fromMaybe 0 ride.fare) ev.fareBeforePaymentCharge)

handleDeferredPanVerification ::
  ( CacheFlow m r,
    Esq.EsqDBFlow m r,
    MonadFlow m,
    EncFlow.EncFlow m r,
    ServiceFlow m r
  ) =>
  RideEndedEvent ->
  m ()
handleDeferredPanVerification ev = withRideAndBooking ev $ \ride booking -> do
  mbDriverPerson <- QPerson.findById ride.driverId
  whenJust mbDriverPerson $ \driverPerson -> do
    mbDriverPanCard <- DPQuery.findByDriverId ride.driverId
    whenJust mbDriverPanCard $ \driverPanCard ->
      when (driverPanCard.verificationStatus == Documents.PENDING) $ do
        panDocCfg <-
          getOneConfig
            ( DocumentVerificationConfigDimensions
                { merchantOperatingCityId = booking.merchantOperatingCityId.getId,
                  documentType = Just DTO.PanCard,
                  vehicleCategory = Just DVC.CAR
                }
            )
            Nothing
            >>= fromMaybeM (DocumentVerificationConfigNotFound booking.merchantOperatingCityId.getId (show DTO.PanCard))
        when (fromMaybe False panDocCfg.doNotValidateDuringOnboarding) $ do
          mbInFlight <- IVQueryExtra.findLatestPendingByDriverIdAndDocType ride.driverId DTO.PanCard
          when (isNothing mbInFlight) $ do
            panNumber <- EncFlow.decrypt driverPanCard.panCardNumber
            nowForDob <- getCurrentTime
            let dob = fromMaybe nowForDob driverPanCard.driverDob
            PanVerification.verifyPanFlow
              driverPerson
              driverPerson.merchantOperatingCityId
              panDocCfg
              panNumber
              dob
              driverPanCard.documentImageId1
              driverPanCard.driverNameOnGovtDB

handleGoogleMobilityBilling ::
  ( CacheFlow m r,
    Esq.EsqDBFlow m r,
    MonadFlow m,
    EncFlow.EncFlow m r,
    CoreMetrics.CoreMetrics m,
    HasRequestId r,
    HasKafkaProducer r
  ) =>
  RideEndedEvent ->
  m ()
handleGoogleMobilityBilling ev = withRideAndBooking ev $ \ride booking -> do
  thresholdConfig <- fetchTransporterConfig ride
  when (thresholdConfig.enableMobilityBilling == Just True) $
    GoogleMobilityBilling.reportNavBillableEvent booking ride
