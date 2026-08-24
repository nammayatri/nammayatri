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
  )
where

import qualified Data.Aeson as A
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Registration as DReg
import "dynamic-offer-driver-app" Domain.Action.UI.Ride.EndRide (RideInterpolationData (..))
import qualified "dynamic-offer-driver-app" Domain.Types.Booking as SRB
import qualified "dynamic-offer-driver-app" Domain.Types.DocumentVerificationConfig as DTO
import "dynamic-offer-driver-app" Domain.Types.Event.RideEndedEvent (RideEndedEvent (..))
import qualified "dynamic-offer-driver-app" Domain.Types.Merchant as DM
import qualified "dynamic-offer-driver-app" Domain.Types.MerchantOperatingCity as DMOC
import qualified "dynamic-offer-driver-app" Domain.Types.Person as DP
import qualified "dynamic-offer-driver-app" Domain.Types.Ride as Ride
import qualified "dynamic-offer-driver-app" Domain.Types.RideRelatedNotificationConfig as DRN
import "dynamic-offer-driver-app" Domain.Types.TransporterConfig (TransporterConfig)
import qualified "beckn-spec" Domain.Types.Trip as DTrip
import Kernel.Beam.Lib.Utils (pushToKafka)
import Kernel.External.Encryption (EncFlow)
import qualified Kernel.External.Encryption as EncFlow
import qualified Kernel.External.Notification.FCM.Types as FCM
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
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import Kernel.Utils.Common
  ( CacheFlow,
    HasShortDurationRetryCfg,
    Hours,
    fromMaybeM,
    getCurrentTime,
    getLocalCurrentTime,
    logInfo,
    logWarning,
  )
import qualified Lib.BehaviorEngine.Orchestrator as BEOrch
import qualified Lib.BehaviorTracker.Snapshot as BTSnap
import qualified Lib.BehaviorTracker.Types as BTT
import "config-pilot" Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.LocationUpdates.Internal as LU
import Lib.Scheduler.Environment (JobCreator)
import Lib.SessionizerMetrics.Types.Event (EventStreamFlow)
import Lib.Yudhishthira.Storage.Beam.BeamFlow (HasYudhishthiraTablesSchema)
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Tools.Utils as YTUtils
import qualified Lib.Yudhishthira.Types as LYT
import qualified Processor.RideEvents.InternalHelpers as IH
import qualified "dynamic-offer-driver-app" SharedLogic.Analytics as Analytics
import qualified "dynamic-offer-driver-app" SharedLogic.BehaviourManagement.ConsequenceDispatcher as BehaviorDispatch
import qualified "dynamic-offer-driver-app" SharedLogic.External.LocationTrackingService.Types as LT
import qualified "dynamic-offer-driver-app" SharedLogic.FleetVehicleStats as FVS
import "dynamic-offer-driver-app" SharedLogic.Reminder.Helper (checkAndCreateRemindersForRidesThreshold)
import qualified "dynamic-offer-driver-app" SharedLogic.ScheduledNotifications as SN
import qualified "dynamic-offer-driver-app" Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified "dynamic-offer-driver-app" Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified "dynamic-offer-driver-app" Storage.CachedQueries.RideRelatedNotificationConfig as CRN
import "dynamic-offer-driver-app" Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified "dynamic-offer-driver-app" Storage.Queries.Booking as QRB
import qualified "dynamic-offer-driver-app" Storage.Queries.DailyStats as QDailyStats
import qualified "dynamic-offer-driver-app" Storage.Queries.DriverInformationExtra as QDriverInfo
import qualified "dynamic-offer-driver-app" Storage.Queries.DriverPlan as QDPlan
import qualified "dynamic-offer-driver-app" Storage.Queries.DriverProfileQuestions as QDriverProfileQuestions
import qualified "dynamic-offer-driver-app" Storage.Queries.DriverRCAssociation as QDRCA
import qualified "dynamic-offer-driver-app" Storage.Queries.DriverReferral as QDriverReferral
import qualified "dynamic-offer-driver-app" Storage.Queries.DriverStats as QDriverStats
import qualified "dynamic-offer-driver-app" Storage.Queries.Image as QImage
import qualified "dynamic-offer-driver-app" Storage.Queries.Person as QPerson
import qualified "dynamic-offer-driver-app" Storage.Queries.RCStatsExtra as QRCStats
import qualified "dynamic-offer-driver-app" Storage.Queries.RegistrationToken as QReg
import qualified "dynamic-offer-driver-app" Storage.Queries.Ride as QRide
import qualified "dynamic-offer-driver-app" Storage.Queries.RiderDetails as QRiderDetails
import qualified "dynamic-offer-driver-app" Storage.Queries.Vehicle as QVehicle
import qualified "dynamic-offer-driver-app" Storage.Queries.VehicleExtra as QVehicleExtra
import qualified "dynamic-offer-driver-app" Tools.ActorInfo as ActorInfo
import qualified Tools.DynamicLogic as DL
import "dynamic-offer-driver-app" Tools.Error
import "dynamic-offer-driver-app" Tools.Event (BookingEventData (..), RideEventData (..))
import qualified "dynamic-offer-driver-app" Tools.Event as Event
import "dynamic-offer-driver-app" Tools.Notifications (notifyDriver)

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
-- notifications, operational-table sync) is gated by ONE 1-day Redis lock keyed on the
-- driver: the first qualifying ride of the rolling day runs the whole thing inline; any
-- further city-changing ride for this driver within 24h is a clean no-op. This debounces
-- NCR-style thrash (a stray local ride in a neighboring cross-travel city shouldn't yank
-- a driver back and forth) without needing a separate delayed job -- the lock itself is
-- the debounce.
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
      Redis.whenWithLockRedis (driverCityMigrationLockKey ride.driverId) driverCityMigrationLockTtl $ do
        -- --- auth layer: mirrors Dashboard.Driver.postDriverChangeOperatingCity's manual flow ---
        QPerson.updateMerchantOperatingCityId ride.driverId targetOpCityId
        QReg.updateMerchantOperatingCityId targetOpCityId.getId ride.driverId.getId booking.providerId.getId
        DReg.cleanCachedTokens ride.driverId
        targetOpCity <- CQMOC.findById targetOpCityId >>= fromMaybeM (MerchantOperatingCityDoesNotExist targetOpCityId.getId)
        notifyDriver
          targetOpCityId
          FCM.NEW_MESSAGE
          "Operating City Updated"
          ("Your operating city has been updated to " <> show targetOpCity.city <> ".")
          driverPerson
          driverPerson.deviceToken

        -- --- billing: tag only, never auto-switch (per-city pricing differs) ---
        mbDriverPlan <- QDPlan.findByPrimaryKey ride.driverId
        let mbBillingTag =
              mbDriverPlan >>= \p ->
                if p.merchantOpCityId /= targetOpCityId
                  then
                    Just
                      ( LYT.TagNameValue "SubscriptionMigrationPending",
                        "Your operating city has changed. Please visit the plan page to select a plan for your current city."
                      )
                  else Nothing

        -- --- documents: diff required-vs-held, dedup-tag, notify. Uses the ForEnabling
        -- notion (fromMaybe isMandatory isMandatoryForEnabling), not plain isMandatory --
        -- isMandatory alone misses "enabling-only" docs (isMandatory=false,
        -- isMandatoryForEnabling=true, e.g. MSDS/Medical), so a driver needing one of
        -- those to go online in the new city would otherwise never be told. ---
        mbVehicle <- QVehicle.findById ride.driverId
        docTags <- case mbVehicle >>= (.category) of
          Nothing -> pure []
          Just vehicleCategory -> do
            allConfigs <- CQDVC.findAllByMerchantOpCityId targetOpCityId Nothing
            let requiredDocTypes =
                  [ c.documentType
                    | c <- allConfigs,
                      fromMaybe c.isMandatory c.isMandatoryForEnabling,
                      not c.isDisabled,
                      c.vehicleCategory == vehicleCategory
                  ]
            missingDocTypes <- filterM (fmap not . isDocumentHeld ride.driverId booking.providerId) requiredDocTypes
            pure
              [ ( LYT.TagNameValue ("MissingDocumentForCity_" <> show docType <> "#" <> targetOpCityId.getId),
                  "Please upload " <> show docType <> " to continue operating in your current city."
                )
                | docType <- missingDocTypes
              ]

        applyNewTags driverPerson targetOpCityId (maybeToList mbBillingTag <> docTags)

        -- --- operational tables: sync directly, no gating. Document tables are
        -- intentionally excluded -- nothing reads their city column. ---
        QDriverInfo.updateMerchantIdAndCityIdByDriverId ride.driverId booking.providerId targetOpCityId
        QVehicleExtra.updateMerchantIdAndCityIdByDriverId ride.driverId booking.providerId (Just targetOpCityId.getId)
        QDailyStats.updateMerchantIdAndCityIdByDriverId (Just booking.providerId) (Just targetOpCityId) ride.driverId
        QDriverReferral.updateMerchantIdAndCityIdByDriverId (Just booking.providerId) (Just targetOpCityId) ride.driverId
        QDriverProfileQuestions.updateMerchantOperatingCityIdByDriverId targetOpCityId ride.driverId
  where
    isDocumentHeld :: (CacheFlow m r, Esq.EsqDBFlow m r, MonadFlow m) => Id DP.Person -> Id DM.Merchant -> DTO.DocumentType -> m Bool
    isDocumentHeld personId merchantId docType = do
      images <- QImage.findImagesByPersonAndType Nothing Nothing merchantId personId docType
      pure $ any (\img -> img.verificationStatus == Just Documents.VALID) images

    -- Applies every candidate tag against ONE accumulating view of the driver's tag
    -- list, so a tag added earlier in this run is visible to the dedup check for a
    -- later one in the same run. existingTags is expiry-filtered up front -- relying
    -- on the read-side fromTType' filter alone isn't safe here, since EsqDBFlow reads
    -- can be served from a Redis-cached Person snapshot taken before a tag's embedded
    -- expiry passed, which would otherwise keep suppressing a fresh notification for
    -- an already-expired-but-still-unresolved gap. The final write-comparison is
    -- against this same filtered baseline, so a run that only prunes expired entries
    -- (no new tag needed) still persists that cleanup. Sends exactly one notification
    -- per genuinely new tag, targeted at the city the driver is migrating TO (not
    -- person.merchantOperatingCityId, which is stale here -- driverPerson was loaded
    -- before the city-update write above and is never refreshed).
    applyNewTags ::
      ( CacheFlow m r,
        Esq.EsqDBFlow m r,
        MonadFlow m,
        Redis.HedisFlow m r,
        Redis.HedisLTSFlowEnv r
      ) =>
      DP.Person ->
      Id DMOC.MerchantOperatingCity ->
      [(LYT.TagNameValue, Text)] ->
      m ()
    applyNewTags person targetOpCityId candidateTags = do
      now <- getCurrentTime
      let existingTags = YTUtils.filterExpiredTags' now (fromMaybe [] person.driverTag)
      finalTags <- foldM (addTagIfAbsent person targetOpCityId now) existingTags candidateTags
      when (YTUtils.showRawTags finalTags /= YTUtils.showRawTags existingTags) $
        QPerson.updateDriverTag (Just finalTags) person.id

    addTagIfAbsent ::
      ( CacheFlow m r,
        Esq.EsqDBFlow m r,
        MonadFlow m,
        Redis.HedisFlow m r,
        Redis.HedisLTSFlowEnv r
      ) =>
      DP.Person ->
      Id DMOC.MerchantOperatingCity ->
      UTCTime ->
      [LYT.TagNameValueExpiry] ->
      (LYT.TagNameValue, Text) ->
      m [LYT.TagNameValueExpiry]
    addTagIfAbsent person targetOpCityId now acc (tag, body) =
      if YTUtils.elemTagNameValue tag acc
        then pure acc
        else do
          let tagWithExpiry = YTUtils.addTagExpiry tag (Just tagExpiryHours) now
          notifyDriver
            targetOpCityId
            FCM.NEW_MESSAGE
            "City Update"
            body
            person
            person.deviceToken
          pure $ YTUtils.replaceTagNameValue (Just acc) tagWithExpiry

    tagExpiryHours :: Hours
    tagExpiryHours = 336 -- 14 days

driverCityMigrationLockKey :: Id DP.Person -> Text
driverCityMigrationLockKey driverId = "DriverCityMigration:Lock:" <> driverId.getId

driverCityMigrationLockTtl :: Int
driverCityMigrationLockTtl = 86400 -- 1 day: debounces the entire migration (auth sync +
-- notification + reconciliation) per driver, per the
-- senior-review decision -- not just a job enqueue.
