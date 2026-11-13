{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module SharedLogic.IncentiveJourney
  ( hasAssignedJourneys,
    findAssignmentsByUserId,
    findDashboardAssignmentsByUserId,
    selectPreferredJourney,
    orderJourneysForDisplay,
    mkJourneyPeriodKey,
    mkDailyPeriodKey,
    journeyTypeOrDefault,
    isJourneyWindowActive,
    evaluateDriverJourney,
    evaluateDriverBehaviorTick,
    syncDriverBehaviorMetricTick,
    forkSettleClosedPeriodsForList,
    forkEnsureAssignedCampaignTags,
    publishBehaviorMetricTick,
    bumpCampaignBehaviorMetric,
    bumpCampaignBehaviorMetricFromMemberships,
    loadJourneyMilestones,
    waiveDriverMilestone,
    inspectOrTriggerStreakEndReward,
    retryVerifiedVpaJourneyCashPayouts,
    JourneyRewardDispatchFlow,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Text as T
import qualified Domain.Types.Common as DCommon
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.DriverPlan as DDriverPlan
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import Domain.Types.Extra.Plan ()
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Overlay as DOverlay
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleCategory as DTV
import qualified Environment
import qualified Kernel.External.Payout.Interface as IPayout
import Kernel.External.Types (Language (..), SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Hedis (HedisLTSFlowEnv)
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Storage.Hedis.Config (HedisFlow)
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.DriverCoins.Coins as Coins
import qualified Lib.DriverCoins.IncentiveMetrics as IncentiveMetrics
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney as IJ
import qualified Lib.IncentiveJourney.BehaviorMetric as BM
import qualified Lib.IncentiveJourney.BehaviorMetricRedis as BMRedis
import qualified Lib.IncentiveJourney.CampaignSync as CampaignSync
import qualified Lib.IncentiveJourney.CampaignTag as CampaignTag
import Lib.IncentiveJourney.Condition (evaluateCondition)
import qualified Lib.IncentiveJourney.Convert as Convert
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as IJCommon
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats as DIJS
import qualified Lib.IncentiveJourney.MgCampaign as Mg
import qualified Lib.IncentiveJourney.Storage.Beam.BeamFlow as IJBeamFlow
import qualified Lib.IncentiveJourney.Storage.CachedQueries.Assignment as CQAssignment
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourney as CQJourney
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourneyMilestone as CQMilestone
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyMilestone as QMilestone
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyStatsExtra as QStatsExtra
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified SharedLogic.FleetDriverSettings as FDS
import qualified SharedLogic.IncentiveJourney.BehaviorTickPublisher as BehaviorTickPublisher
import qualified SharedLogic.IncentiveJourney.CampaignBehaviorBump as CampaignBehaviorBump
import SharedLogic.IncentiveJourney.CampaignControlsCache (getCampaignControls)
import qualified SharedLogic.IncentiveJourney.MetricBag as MetricBag
import qualified SharedLogic.IncentiveJourney.MgCampaign as MgCampaignSync
import SharedLogic.PayoutStatusCheck (afterPayoutOrderCreated)
import Storage.Beam.IncentiveJourney ()
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import Storage.ConfigPilot.Config.PayoutConfig (PayoutConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverPlan as SQPlan
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Encryption (decrypt)
import Tools.Error
import Tools.Notifications (mkOverlayReq, sendOverlay)
import qualified Tools.Payout as Payout

-- | Cash payout only. Narrower than journey evaluation so a verified-VPA hook can call it
-- without the coin and finance-ledger constraints.
type JourneyCashPayoutFlow m r =
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    Finance.HasActorInfo m r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  )

type JourneyRewardDispatchFlow m r =
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    HedisLTSFlowEnv r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    PaymentBeamFlow.BeamFlow m r,
    BeamFlow m r,
    IJBeamFlow.BeamFlow m r,
    Finance.HasActorInfo m r,
    Coins.EventFlow m r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  )

hasAssignedJourneys :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> m Bool
hasAssignedJourneys personId = not . null <$> findAssignmentsByUserId personId

findAssignmentsByUserId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> m [IJ.JourneyAssignment]
findAssignmentsByUserId personId = do
  mbPerson <- QPerson.findById personId
  mbVehicle <- QVeh.findById personId
  case mbPerson of
    Nothing -> CQAssignment.findAssignmentsByUserId IJ.DriverActor (cast personId)
    Just person ->
      CQAssignment.findAssignmentsForDriver
        IJ.DriverActor
        (cast personId)
        (cast person.merchantId)
        (cast person.merchantOperatingCityId)
        (T.pack . show <$> (mbVehicle >>= (.category)))

-- | Dashboard list: pending OptIn invites included (no optedInAt filter).
findDashboardAssignmentsByUserId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> m [IJ.JourneyAssignment]
findDashboardAssignmentsByUserId personId = do
  mbPerson <- QPerson.findById personId
  mbVehicle <- QVeh.findById personId
  case mbPerson of
    Nothing -> CQAssignment.findMembershipsByUserId IJ.DriverActor (cast personId)
    Just person ->
      CQAssignment.findMembershipsForDriver
        IJ.DriverActor
        (cast personId)
        (cast person.merchantId)
        (cast person.merchantOperatingCityId)
        (T.pack . show <$> (mbVehicle >>= (.category)))

journeyTypeOrDefault :: Maybe DIJ.IncentiveJourneyType -> DIJ.IncentiveJourneyType
journeyTypeOrDefault = IJ.journeyTypeOrDefault

selectPreferredJourney :: UTCTime -> [(DIJ.IncentiveJourney, DCJM.CohortJourneyMapping)] -> Maybe DIJ.IncentiveJourney
selectPreferredJourney = IJ.selectPreferredJourney

orderJourneysForDisplay :: UTCTime -> [(DIJ.IncentiveJourney, DCJM.CohortJourneyMapping)] -> [DIJ.IncentiveJourney]
orderJourneysForDisplay = IJ.orderJourneysForDisplay

isJourneyWindowActive :: UTCTime -> DIJ.IncentiveJourney -> DCJM.CohortJourneyMapping -> Bool
isJourneyWindowActive = IJ.isJourneyWindowActiveFor

mkJourneyPeriodKey :: UTCTime -> DIJ.IncentiveJourney -> UTCTime -> Text
mkJourneyPeriodKey = IJ.mkJourneyPeriodKeyFor

mkDailyPeriodKey :: UTCTime -> Text
mkDailyPeriodKey = IJ.mkDailyPeriodKey

loadJourneyMilestones ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DIJ.IncentiveJourney ->
  m [DIJM.IncentiveJourneyMilestone]
loadJourneyMilestones = CQMilestone.findByJourneyId IJ.DriverActor

sendMilestoneCompletedOverlay ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, HedisLTSFlowEnv r) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  DIJ.IncentiveJourney ->
  DIJM.IncentiveJourneyMilestone ->
  [DIJM.IncentiveJourneyMilestone] ->
  Int ->
  m ()
sendMilestoneCompletedOverlay merchantOpCityId driverId journey milestone journeyMilestones awarded = do
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  mOverlay <-
    CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory
      merchantOpCityId
      IJ.milestoneCompletedOverlayKey
      (fromMaybe ENGLISH driver.language)
      Nothing
      Nothing
      Nothing
  whenJust mOverlay $ \overlay -> do
    let milestoneTarget = IJ.resolveOverlayMilestoneDescription milestone journeyMilestones
        displayReward = IJ.displayMilestoneRewardAmount awarded milestone
        applyTemplates = IJ.applyMilestoneOverlayTemplates journey milestoneTarget milestone journeyMilestones displayReward
        overlay' =
          overlay
            { DOverlay.title = fmap applyTemplates overlay.title,
              DOverlay.description = fmap applyTemplates overlay.description,
              DOverlay.okButtonText = fmap applyTemplates overlay.okButtonText,
              DOverlay.cancelButtonText = fmap applyTemplates overlay.cancelButtonText,
              DOverlay.toastMessage = fmap applyTemplates overlay.toastMessage,
              DOverlay.actions = [IJ.milestoneCompletedOverlayKey]
            }
    sendOverlay merchantOpCityId driver $ mkOverlayReq overlay'

sendMilestoneWaivedOverlay ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, HedisLTSFlowEnv r) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  DIJ.IncentiveJourney ->
  DIJM.IncentiveJourneyMilestone ->
  [DIJM.IncentiveJourneyMilestone] ->
  m ()
sendMilestoneWaivedOverlay merchantOpCityId driverId journey milestone journeyMilestones = do
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  mOverlay <-
    CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory
      merchantOpCityId
      IJ.milestoneWaivedOverlayKey
      (fromMaybe ENGLISH driver.language)
      Nothing
      Nothing
      Nothing
  whenJust mOverlay $ \overlay -> do
    let milestoneTarget = IJ.resolveOverlayMilestoneDescription milestone journeyMilestones
        applyTemplates = IJ.applyMilestoneOverlayTemplates journey milestoneTarget milestone journeyMilestones 0
        overlay' =
          overlay
            { DOverlay.title = fmap applyTemplates overlay.title,
              DOverlay.description = fmap applyTemplates overlay.description,
              DOverlay.okButtonText = fmap applyTemplates overlay.okButtonText,
              DOverlay.cancelButtonText = fmap applyTemplates overlay.cancelButtonText,
              DOverlay.toastMessage = fmap applyTemplates overlay.toastMessage,
              DOverlay.actions = [IJ.milestoneWaivedOverlayKey]
            }
    sendOverlay merchantOpCityId driver $ mkOverlayReq overlay'

mkDriverEvaluateHandle ::
  JourneyRewardDispatchFlow m r =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  DTV.VehicleCategory ->
  Maybe DCommon.ServiceTierType ->
  m (IJ.IncentiveJourneyHandle m)
mkDriverEvaluateHandle driverId merchantId merchantOpCityId transporterConfig vehCategory mbServiceTier =
  pure
    IJ.IncentiveJourneyHandle
      { actor = IJ.DriverActor,
        loadJourneysByIds = CQJourney.findByIds IJ.DriverActor,
        loadMilestones = loadJourneyMilestones,
        dispatchReward = \journey ctx spec ->
          dispatchDriverReward
            driverId
            merchantId
            merchantOpCityId
            transporterConfig
            journey
            vehCategory
            mbServiceTier
            ctx
            spec,
        dispatchStreakEndReward = \journey campaignKey spec ->
          dispatchDriverStreakEndReward
            driverId
            merchantId
            merchantOpCityId
            transporterConfig
            journey
            vehCategory
            mbServiceTier
            campaignKey
            spec,
        mbOnMilestoneCompleted =
          Just $ \journey milestone journeyMilestones awarded ->
            void $
              withTryCatch "IncentiveJourney:sendMilestoneCompletedOverlay" $
                sendMilestoneCompletedOverlay
                  merchantOpCityId
                  driverId
                  journey
                  milestone
                  journeyMilestones
                  awarded,
        mbOnMgOptInCleared = Just $ \personId cjmId -> do
          matched <- MgCampaignSync.clearMgOptedInOnDriverInfo personId cjmId
          when matched $ FDS.onCampaignMembershipCleared (cast personId) (cast cjmId),
        buildMetricContext = MetricBag.buildMetricContext
      }

-- | Kafka RIDE_EVENTS journey evaluation (not HTTP EndRide).
evaluateDriverJourney ::
  JourneyRewardDispatchFlow m r =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  DTV.VehicleCategory ->
  Maybe DCommon.ServiceTierType ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  UTCTime ->
  IncentiveMetrics.RideIncentiveDeltas ->
  m ()
evaluateDriverJourney driverId merchantId merchantOpCityId transporterConfig vehCategory mbServiceTier _mbEntityId mbPickupSpecialLocationId mbDropSpecialLocationId timeBoundReferenceUtc rideDeltas = do
  ijHandle <-
    mkDriverEvaluateHandle
      driverId
      merchantId
      merchantOpCityId
      transporterConfig
      vehCategory
      mbServiceTier
  let input =
        IJ.EvaluateInput
          { personId = cast driverId,
            merchantId = cast merchantId,
            merchantOperatingCityId = cast merchantOpCityId,
            timeBoundReferenceUtc = timeBoundReferenceUtc,
            timeDiffFromUtc = transporterConfig.timeDiffFromUtc,
            rideDeltas = toLibDeltas rideDeltas,
            mbPickupSpecialLocationId = mbPickupSpecialLocationId,
            mbDropSpecialLocationId = mbDropSpecialLocationId,
            mbRideVehicleCategory = Just (show vehCategory),
            mbRideServiceTierType = show <$> mbServiceTier
          }
  IJ.evaluateIncentiveJourneys ijHandle input

-- | Consumer-side (IJ_BEHAVIOR_TICK_CONSUMER): full bag + JsonLogic + stats upsert.
-- Do not call from app HTTP — use 'publishBehaviorMetricTick' after BT bump.
evaluateDriverBehaviorTick ::
  JourneyRewardDispatchFlow m r =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Maybe DTV.VehicleCategory ->
  UTCTime ->
  m ()
evaluateDriverBehaviorTick driverId merchantId merchantOpCityId transporterConfig mbVehCategory timeBoundReferenceUtc = do
  vehCategory <-
    case mbVehCategory of
      Just vc -> pure vc
      Nothing -> do
        mbVeh <- QVeh.findById driverId
        pure $ fromMaybe DTV.AUTO_CATEGORY ((.category) =<< mbVeh)
  let mbServiceTier = Nothing
  ijHandle <-
    mkDriverEvaluateHandle
      driverId
      merchantId
      merchantOpCityId
      transporterConfig
      vehCategory
      mbServiceTier
  IJ.evaluateBehaviorJourneys
    ijHandle
    IJ.BehaviorEvaluateInput
      { personId = cast driverId,
        merchantId = cast merchantId,
        merchantOperatingCityId = cast merchantOpCityId,
        timeBoundReferenceUtc = timeBoundReferenceUtc,
        timeDiffFromUtc = transporterConfig.timeDiffFromUtc,
        mbRideVehicleCategory = Just (show vehCategory)
      }

-- | Stats-sync + closed-period settle (no mid-period reward). Used by IJ_BEHAVIOR_TICK_CONSUMER.
syncDriverBehaviorMetricTick ::
  JourneyRewardDispatchFlow m r =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  UTCTime ->
  [Text] ->
  Text ->
  Int ->
  m ()
syncDriverBehaviorMetricTick driverId merchantId merchantOpCityId transporterConfig occurredAt cjmIds metricOrAction delta = do
  mbVeh <- QVeh.findById driverId
  let vehCategory = fromMaybe DTV.AUTO_CATEGORY ((.category) =<< mbVeh)
  ijHandle <-
    mkDriverEvaluateHandle
      driverId
      merchantId
      merchantOpCityId
      transporterConfig
      vehCategory
      Nothing
  IJ.syncBehaviorMetricAndSettle
    ijHandle
    (cast driverId)
    (cast merchantId)
    (cast merchantOpCityId)
    transporterConfig.timeDiffFromUtc
    occurredAt
    cjmIds
    metricOrAction
    delta
    (Just (show vehCategory))

-- | Homescreen list: sole closed-period settle path (fork).
-- Steady state (already caught up): Redis GET of done-watermark per CJM → no
-- milestone load / vehicle / evaluate-handle / DB settle. Catch-up only when
-- watermark lags (incl. final day during post-end grace).
forkSettleClosedPeriodsForList ::
  JourneyRewardDispatchFlow m r =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  m ()
forkSettleClosedPeriodsForList driverId merchantId merchantOpCityId transporterConfig =
  fork "IncentiveJourney:listSettleClosedPeriods" $ do
    localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
    assignments <- findAssignmentsByUserId driverId
    -- Cheap scan: journeys + done-watermark only. Collect CJMs that still need settle.
    pendingWork <-
      fmap concat $
        forM assignments $ \assignment -> do
          let cjm = assignment.cohortJourneyMapping
          if not cjm.enabled
            then pure []
            else do
              journeys <- CQJourney.findByIds IJ.DriverActor [cjm.journeyId]
              fmap concat $
                forM journeys $ \journey -> do
                  let endExclusive = IJ.computeStreakEndDate cjm.startDate cjm.streakRange journey.journeyType
                      graceEnd = addUTCTime (3 * 24 * 3600) endExclusive
                      canSettle =
                        IJ.isJourneyWindowActiveFor localTime journey cjm
                          || (localTime >= endExclusive && localTime < graceEnd)
                  if not canSettle
                    then pure []
                    else do
                      let doneKey = IJ.settleDoneKey driverId.getId cjm.id.getId
                      mbDone <- Hedis.withCrossAppRedis $ Hedis.get doneKey
                      let pending =
                            IJ.pendingClosedPeriodKeysAfterDone localTime journey cjm mbDone
                      pure [(journey, cjm) | not (null pending)]
    -- Hot path: nothing pending → fork ends here (no vehicle / handle / milestones).
    unless (null pendingWork) $ do
      mbVeh <- QVeh.findById driverId
      let vehCategory = fromMaybe DTV.AUTO_CATEGORY ((.category) =<< mbVeh)
      ijHandle <-
        mkDriverEvaluateHandle
          driverId
          merchantId
          merchantOpCityId
          transporterConfig
          vehCategory
          Nothing
      forM_ pendingWork $ \(journey, cjm) -> do
        milestones <- loadJourneyMilestones journey.id
        IJ.settleClosedPeriodIfNeeded
          ijHandle
          (cast driverId)
          (cast merchantId)
          (cast merchantOpCityId)
          localTime
          journey
          cjm
          milestones

-- | Non-blocking list fork: ensure Assigned IJCampaign tags (never written on assign/bulk).
-- Tag expiry = min(UCM.validTill, streak end) so Unbounded timeBounds cannot keep
-- cancel/AA/pooling alive after the campaign or assignment ends.
-- Also clears tags for CJMs no longer in active assignments. OptIn tags left to their own expiry.
-- Then applies a control campaign's auto-assign if it started after the driver opted in.
forkEnsureAssignedCampaignTags ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EsqDBReplicaFlow m r,
    HedisFlow m r,
    HedisLTSFlowEnv r,
    IJBeamFlow.BeamFlow m r,
    Metrics.HasCoreMetrics r
  ) =>
  Id DP.Person ->
  m ()
forkEnsureAssignedCampaignTags driverId =
  fork "IncentiveJourney:ensureAssignedCampaignTags" $ do
    localNow <- getCurrentTime
    mbPerson <- QPerson.findById driverId
    let memberships = maybe [] CampaignBehaviorBump.membershipsFromPersonTags ((.driverTag) =<< mbPerson)
    assignments <- findAssignmentsByUserId driverId
    let activeCjmIds = [a.cohortJourneyMapping.id.getId | a <- assignments, a.cohortJourneyMapping.enabled]
        staleMemberships = filter (\m -> m.cjmId `notElem` activeCjmIds) memberships
        personId = cast driverId :: Id IJCommon.Person
    -- Drop perks for expired/disabled UCMs still lingering on person tags.
    forM_ staleMemberships $ \m -> do
      matched <- MgCampaignSync.clearMgOptedInOnDriverInfo personId (Id m.cjmId)
      when matched $ FDS.onCampaignMembershipCleared driverId (Id m.cjmId)
    existingControlsCjmIds <-
      fmap catMaybes $
        forM memberships $ \m -> do
          if m.cjmId `elem` activeCjmIds
            then do
              mbControls <- getCampaignControls (Id m.cjmId)
              pure $
                if maybe False (Mg.valueHasControlFeatures . (.controlFeatures)) mbControls
                  then Just m.cjmId
                  else Nothing
            else pure Nothing
    let assignedActive =
          [ a
            | a <- assignments,
              let cjm = a.cohortJourneyMapping,
              cjm.enabled,
              fromMaybe DCJM.Assigned cjm.campaignUserLinkType == DCJM.Assigned
          ]
    -- Upsert with boundary expiry (refresh fixes older tags written with no expiry).
    payloadRows <-
      fmap catMaybes $
        forM assignedActive $ \a -> do
          let cjm = a.cohortJourneyMapping
          mbJourney <- CQJourney.findById IJ.DriverActor cjm.journeyId
          case mbJourney of
            Nothing -> pure Nothing
            Just journey -> do
              let streakEnd = IJ.computeStreakEndDate cjm.startDate cjm.streakRange journey.journeyType
              -- Outside live window [start, end): do not (re)write; clear leftovers from early/late sync.
              if localNow < cjm.startDate || localNow >= streakEnd
                then do
                  when (cjm.id.getId `elem` map (.cjmId) memberships) $ do
                    matched <- MgCampaignSync.clearMgOptedInOnDriverInfo personId cjm.id
                    when matched $ FDS.onCampaignMembershipCleared driverId cjm.id
                  pure Nothing
                else do
                  milestones <- loadJourneyMilestones journey.id
                  let ucmTill = maybe streakEnd (.validTill) a.userCohortMapping
                      tagExpiry = min ucmTill streakEnd
                  pure $
                    case CampaignSync.mkHotPathSync localNow journey cjm milestones True (Just tagExpiry) of
                      Nothing -> Nothing
                      Just p -> Just (p, Mg.cjmHasControlFeatures cjm)
    let payloads = map fst payloadRows
        controlsIds = [p.membership.cjmId | (p, hasControls) <- payloadRows, hasControls]
        -- Prefer refreshing the controls tag already on the person; else first controls payload.
        selectedControls =
          case find (\p -> p.membership.cjmId `elem` existingControlsCjmIds) payloads of
            Just p -> [p]
            Nothing ->
              case listToMaybe [p | p <- payloads, p.membership.cjmId `elem` controlsIds] of
                Just p | null existingControlsCjmIds -> [p]
                _ -> []
        behaviourPayloads =
          filter (\p -> p.membership.cjmId `notElem` controlsIds) payloads
        selectedPayloads = behaviourPayloads <> selectedControls
    case selectedPayloads of
      [] -> pure ()
      ps -> do
        MgCampaignSync.syncCampaignMembershipBatch personId ps
        -- Any opted-in snap is enough: reconcile rebuilds AA from all memberships.
        let mbSnap =
              ((.snapshot) <$> find (\p -> p.membership.cjmId `elem` controlsIds) ps)
                <|> ((.snapshot) <$> listToMaybe ps)
        FDS.reconcileAutoAssignAfterMgSync personId mbSnap
    -- An OptIn control campaign the driver joined before it started takes charge only now: apply its settings once.
    whenJust mbPerson \person -> FDS.reconcileStartedCampaignAutoAssign driverId person.merchantOperatingCityId

-- No assignment / bag / stats work here (that would hit the app pod). Consumer loads
-- assignments and no-ops when empty — same isolation as ride-end → RIDE_EVENTS_CONSUMER.
publishBehaviorMetricTick ::
  ( MonadFlow m,
    HedisFlow m r,
    HasField "ijBehaviorTickPublisherCfg" r (Maybe Environment.RideEventsPublisherCfg),
    Metrics.CoreMetrics m
  ) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  m ()
publishBehaviorMetricTick driverId merchantId merchantOpCityId actionType =
  fork "IncentiveJourney:publishBehaviorMetricTick" $
    BehaviorTickPublisher.publishBehaviorMetricTick driverId merchantId merchantOpCityId actionType

-- | Gated ij:bm bump for campaign members; publishes BehaviourMetricTick only when a key was written.
-- metricName: onlineMinutes | rrReceived | rrAccepted | dxCancel | dafCancel
-- rrReceived: Redis INCR every request; Kafka/stats sync only every N bumps (hot SRD path).
bumpCampaignBehaviorMetric ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    HedisFlow m r,
    IJBeamFlow.BeamFlow m r,
    HasField "ijBehaviorTickPublisherCfg" r (Maybe Environment.RideEventsPublisherCfg),
    Metrics.CoreMetrics m
  ) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Seconds ->
  Text ->
  Int ->
  Text ->
  m ()
bumpCampaignBehaviorMetric driverId merchantId merchantOpCityId timeDiffFromUtc metricName delta actionType =
  fork "IncentiveJourney:bumpCampaignBehaviorMetric" $ do
    case parseCampaignMetric metricName of
      Nothing -> pure ()
      Just metric -> do
        localTime <- getLocalCurrentTime timeDiffFromUtc
        bumpedCjmIds <- CampaignBehaviorBump.bumpIfCampaignMemberDetailed driverId metric delta localTime
        publishAfterCampaignBump driverId merchantId merchantOpCityId metricName delta actionType metric bumpedCjmIds

-- | SRD / pool hot path: memberships already parsed from DriverPoolResult.driverTags.
-- Empty memberships → no fork / no Person read. Same Redis + tick behaviour as bumpCampaignBehaviorMetric.
bumpCampaignBehaviorMetricFromMemberships ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    HedisFlow m r,
    IJBeamFlow.BeamFlow m r,
    HasField "ijBehaviorTickPublisherCfg" r (Maybe Environment.RideEventsPublisherCfg),
    Metrics.CoreMetrics m
  ) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Seconds ->
  Text ->
  Int ->
  Text ->
  [CampaignTag.CampaignMembership] ->
  m ()
bumpCampaignBehaviorMetricFromMemberships _driverId _merchantId _merchantOpCityId _timeDiffFromUtc _metricName _delta _actionType [] = pure ()
bumpCampaignBehaviorMetricFromMemberships driverId merchantId merchantOpCityId timeDiffFromUtc metricName delta actionType memberships =
  fork "IncentiveJourney:bumpCampaignBehaviorMetric" $ do
    case parseCampaignMetric metricName of
      Nothing -> pure ()
      Just metric -> do
        localTime <- getLocalCurrentTime timeDiffFromUtc
        bumpedCjmIds <-
          CampaignBehaviorBump.bumpIfCampaignMemberDetailedFromMemberships driverId memberships metric delta localTime
        publishAfterCampaignBump driverId merchantId merchantOpCityId metricName delta actionType metric bumpedCjmIds

publishAfterCampaignBump ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    HedisFlow m r,
    HasField "ijBehaviorTickPublisherCfg" r (Maybe Environment.RideEventsPublisherCfg),
    Metrics.CoreMetrics m
  ) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  Int ->
  Text ->
  BMRedis.CampaignBehaviorMetric ->
  [Text] ->
  m ()
publishAfterCampaignBump driverId merchantId merchantOpCityId metricName delta actionType metric bumpedCjmIds =
  unless (null bumpedCjmIds) $ do
    let publishMetric metName publishDelta act =
          BehaviorTickPublisher.publishBehaviorMetricTickDetailed
            driverId
            merchantId
            merchantOpCityId
            act
            (Just bumpedCjmIds)
            (Just metName)
            (Just publishDelta)
    case metric of
      BMRedis.RrReceived -> do
        mbFlush <- takeRrReceivedStatsSyncDelta driverId delta
        whenJust mbFlush $ \flushDelta ->
          publishMetric "rrReceived" flushDelta actionType
      BMRedis.OnlineMinutes ->
        -- Online ticks are frequent; do not drain rrReceived batch here.
        publishMetric metricName delta actionType
      _ -> do
        -- Accept/cancel: drain pending rrReceived so stats catch up without waiting for N more RRs.
        mbRrFlush <- takeRrReceivedStatsSyncDelta driverId 0
        whenJust mbRrFlush $ \rrDelta ->
          publishMetric "rrReceived" rrDelta "RR_RECEIVED_FLUSH"
        publishMetric metricName delta actionType

parseCampaignMetric :: Text -> Maybe BMRedis.CampaignBehaviorMetric
parseCampaignMetric = \case
  "onlineMinutes" -> Just BMRedis.OnlineMinutes
  "rrReceived" -> Just BMRedis.RrReceived
  "rrAccepted" -> Just BMRedis.RrAccepted
  "dxCancel" -> Just BMRedis.DxCancel
  "dafCancel" -> Just BMRedis.DafCancel
  _ -> Nothing

-- | Accumulate rrReceived for stats sync. Returns Just flushedDelta when Kafka should fire.
-- delta=0 → only try to flush an existing pending buffer (used by other behaviour events).
takeRrReceivedStatsSyncDelta ::
  (MonadFlow m, CacheFlow m r, HedisFlow m r) =>
  Id DP.Person ->
  Int ->
  m (Maybe Int)
takeRrReceivedStatsSyncDelta driverId delta = do
  let key = BM.rrReceivedPendingSyncKey driverId.getId
      thresh = fromIntegral BM.rrReceivedStatsSyncThreshold :: Integer
      ttlSeconds = 24 * 3600
  pending <-
    if delta > 0
      then
        Hedis.runInMultiCloudRedisWrite $
          Hedis.withCrossAppRedis $
            Hedis.incrby key (fromIntegral delta)
      else do
        mbPending <- Hedis.withCrossAppRedis (Hedis.get key)
        pure $ fromMaybe (0 :: Integer) mbPending
  when (delta > 0) $
    void $
      Hedis.runInMultiCloudRedisWrite $
        Hedis.withCrossAppRedis $
          Hedis.expire key ttlSeconds
  let shouldFlush =
        if delta > 0
          then pending >= thresh
          else pending > 0
  if not shouldFlush
    then pure Nothing
    else do
      let lockKey = key <> ":flush"
      acquired <-
        Hedis.withCrossAppRedis $
          Hedis.setNxExpire lockKey 60 ("1" :: Text)
      if not acquired
        then pure Nothing
        else -- Atomic-ish drain: get n then incrby (-n) so concurrent incrbys survive
        -- (setExp 0 would overwrite them). Always release :flush so accept/cancel
        -- can drain again without waiting for the 60s lock TTL.
        flip finally (Hedis.withCrossAppRedis $ Hedis.del lockKey) $ do
          mbLatest <- Hedis.withCrossAppRedis (Hedis.get key)
          let n = fromMaybe (0 :: Integer) mbLatest
          when (n > 0) $
            void $
              Hedis.runInMultiCloudRedisWrite $
                Hedis.withCrossAppRedis $
                  Hedis.incrby key (negate n)
          pure $
            if n > 0
              then Just (fromIntegral n)
              else Nothing

dispatchDriverReward ::
  JourneyRewardDispatchFlow m r =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  DIJ.IncentiveJourney ->
  DTV.VehicleCategory ->
  Maybe DCommon.ServiceTierType ->
  IJ.RewardDispatchCtx ->
  IJ.RewardSpec ->
  m IJ.AwardResult
dispatchDriverReward driverId merchantId merchantOpCityId transporterConfig journey vehCategory mbServiceTier ctx spec =
  case spec.rewardKind of
    IJ.Coins ->
      case spec.rewardValue of
        Nothing -> do
          logInfo $
            "Journey milestone "
              <> ctx.milestoneId
              <> " has Coins rewardType but no rewardValue; skipping award"
          pure IJ.AwardSkipped
        Just coinsToAward | coinsToAward <= 0 -> do
          logInfo $
            "Journey milestone "
              <> ctx.milestoneId
              <> " has non-positive rewardValue; skipping award"
          pure IJ.AwardSkipped
        Just coinsToAward -> do
          awarded <-
            Coins.awardJourneyMilestoneCoins
              driverId
              merchantId
              merchantOpCityId
              journey.name
              spec.rewardExpirationAt
              coinsToAward
              (Just (ctx.cohortJourneyMappingId <> ":" <> ctx.milestoneId))
              vehCategory
              mbServiceTier
          when (awarded > 0) $
            Coins.updateDriverCoins driverId awarded transporterConfig.timeDiffFromUtc
          logInfo $
            "Awarded "
              <> show awarded
              <> " coins for journey milestone "
              <> ctx.milestoneId
              <> " driver "
              <> driverId.getId
              <> " journey "
              <> journey.name
          if awarded > 0 then pure (IJ.Awarded awarded) else pure IJ.AwardSkipped
    IJ.Cash ->
      case spec.rewardValue of
        Nothing -> do
          logInfo $ "Journey milestone " <> ctx.milestoneId <> " Cash reward missing rewardValue; skipping"
          pure IJ.AwardSkipped
        Just amount | amount <= 0 -> do
          logInfo $ "Journey milestone " <> ctx.milestoneId <> " Cash reward non-positive; skipping"
          pure IJ.AwardSkipped
        Just amount ->
          awardJourneyCashPayout
            driverId
            merchantId
            merchantOpCityId
            transporterConfig
            (milestoneCashReferenceId driverId ctx)
            amount
    IJ.SubscriptionWaiveOff ->
      awardJourneySubscriptionWaiveOff driverId ("milestone " <> ctx.milestoneId <> " journey " <> journey.name) spec
    IJ.NoReward -> pure (IJ.Awarded 0)
    otherKind -> do
      logInfo $ show otherKind <> " reward deferred for journey milestone " <> ctx.milestoneId
      pure IJ.AwardSkipped

dispatchDriverStreakEndReward ::
  JourneyRewardDispatchFlow m r =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  DIJ.IncentiveJourney ->
  DTV.VehicleCategory ->
  Maybe DCommon.ServiceTierType ->
  Text ->
  IJ.RewardSpec ->
  m IJ.AwardResult
dispatchDriverStreakEndReward driverId merchantId merchantOpCityId transporterConfig journey vehCategory mbServiceTier campaignKey spec =
  case spec.rewardKind of
    IJ.Coins ->
      case spec.rewardValue of
        Nothing -> do
          logInfo $ "Streak-end Coins reward missing rewardValue journey=" <> journey.id.getId
          pure IJ.AwardSkipped
        Just coinsToAward | coinsToAward <= 0 -> do
          logInfo $ "Streak-end non-positive rewardValue journey=" <> journey.id.getId
          pure IJ.AwardSkipped
        Just coinsToAward -> do
          let entityId = Just campaignKey
          awarded <-
            Coins.awardJourneyMilestoneCoins
              driverId
              merchantId
              merchantOpCityId
              journey.name
              spec.rewardExpirationAt
              coinsToAward
              entityId
              vehCategory
              mbServiceTier
          when (awarded > 0) $
            Coins.updateDriverCoins driverId awarded transporterConfig.timeDiffFromUtc
          logInfo $
            "Awarded "
              <> show awarded
              <> " streak-end coins driver "
              <> driverId.getId
              <> " journey "
              <> journey.name
              <> " campaignKey="
              <> campaignKey
          if awarded > 0 then pure (IJ.Awarded awarded) else pure IJ.AwardSkipped
    IJ.Cash ->
      case spec.rewardValue of
        Nothing -> do
          logInfo $ "Streak-end Cash reward missing rewardValue journey=" <> journey.id.getId
          pure IJ.AwardSkipped
        Just amount | amount <= 0 -> do
          logInfo $ "Streak-end Cash reward non-positive journey=" <> journey.id.getId
          pure IJ.AwardSkipped
        Just amount ->
          awardJourneyCashPayout
            driverId
            merchantId
            merchantOpCityId
            transporterConfig
            (streakCashReferenceId driverId campaignKey)
            amount
    IJ.SubscriptionWaiveOff ->
      awardJourneySubscriptionWaiveOff driverId ("streak-end " <> journey.name <> " campaignKey=" <> campaignKey) spec
    IJ.NoReward -> pure (IJ.Awarded 0)
    otherKind -> do
      logInfo $ show otherKind <> " streak-end reward deferred journey=" <> journey.id.getId
      pure IJ.AwardSkipped

-- | Create one payout order for this reference. The order id is the reference itself,
-- so a retry or the verified-VPA backfill cannot create a second order.
-- Awarded means a payout order exists. AwardSkipped means it does not (no verified UPI, payout off, partner error).
awardJourneyCashPayout ::
  JourneyCashPayoutFlow m r =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Text ->
  Int ->
  m IJ.AwardResult
awardJourneyCashPayout driverId merchantId merchantOpCityId transporterConfig referenceId amountRupees = do
  let lockKey = "IncentiveJourneyCashLock:" <> referenceId
  locked <- Redis.tryLockRedis lockKey 120
  if not locked
    then do
      mbOrder <- QPayoutOrder.findByOrderId referenceId
      pure $
        if isJust mbOrder
          then IJ.Awarded amountRupees
          else IJ.AwardSkipped
    else do
      outcome <- withTryCatch "incentiveJourneyCashPayout" $ awardJourneyCashPayoutLocked driverId merchantId merchantOpCityId transporterConfig referenceId amountRupees
      void $ Redis.unlockRedis lockKey
      case outcome of
        Right result -> pure result
        Left err -> do
          mbOrder <- QPayoutOrder.findByOrderId referenceId
          case mbOrder of
            Just _ -> do
              logInfo $ "Incentive journey cash payout already exists referenceId=" <> referenceId <> " driver=" <> driverId.getId
              pure (IJ.Awarded amountRupees)
            Nothing -> do
              logError $ "Incentive journey cash payout failed referenceId=" <> referenceId <> " driver=" <> driverId.getId <> " error=" <> show err
              pure IJ.AwardSkipped

awardJourneyCashPayoutLocked ::
  JourneyCashPayoutFlow m r =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Text ->
  Int ->
  m IJ.AwardResult
awardJourneyCashPayoutLocked driverId merchantId merchantOpCityId transporterConfig referenceId amountRupees = do
  mbExisting <- QPayoutOrder.findByOrderId referenceId
  case mbExisting of
    Just _ -> do
      logInfo $ "Incentive journey cash payout already exists referenceId=" <> referenceId <> " driver=" <> driverId.getId
      pure (IJ.Awarded amountRupees)
    Nothing -> do
      person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      mbVehicle <- QVeh.findById driverId
      let vehicleCategory = fromMaybe DTV.AUTO_CATEGORY ((.category) =<< mbVehicle)
      payoutConfig <-
        getOneConfig (PayoutConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, vehicleCategory = Just vehicleCategory, isPayoutEnabled = Nothing}) Nothing
          >>= fromMaybeM (PayoutConfigNotFound (show vehicleCategory) merchantOpCityId.getId)
      unless payoutConfig.isPayoutEnabled $
        throwError $ InvalidRequest "Incentive journey cash payout is not enabled"
      when (amountRupees <= 0) $
        throwError $ InvalidRequest "Incentive journey cash amount is not positive"
      driverInfo <- QDriverInfo.findById (cast driverId) >>= fromMaybeM DriverInfoNotFound
      (payoutServiceFlow, payoutServiceName, mbPersonBankAccount) <-
        Payout.getCreatePayoutServiceFlow Payout.MerchantServiceUsageConfigOption DEMSC.PayoutService person.clientSdkVersion merchantOpCityId person.id
      customerVpa <- case payoutServiceFlow of
        IPayout.JuspayFlow ->
          case (driverInfo.payoutVpaStatus, driverInfo.payoutVpa) of
            (Just DI.VIA_WEBHOOK, Just vpa) -> pure (Just vpa)
            (Just DI.VERIFIED_BY_USER, Just vpa) -> pure (Just vpa)
            _ -> throwError $ InvalidRequest "Incentive journey cash payout needs a verified UPI id"
        IPayout.StripeFlow -> pure Nothing
        IPayout.BulkFlow -> pure Nothing
      phoneNo <- mapM decrypt person.mobileNumber
      merchantOperatingCity <- CQMOC.findById (cast merchantOpCityId) >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
      let amount = fromIntegral amountRupees :: HighPrecMoney
          createPayoutOrderReq =
            DPayment.mkCreatePayoutServiceReq
              referenceId
              amount
              transporterConfig.currency
              phoneNo
              person.email
              driverId.getId
              payoutConfig.remark
              (Just person.firstName)
              customerVpa
              payoutConfig.orderType
              payoutServiceFlow
              Nothing
              Nothing
          entityName = DPayment.INCENTIVE_JOURNEY_CASHBACK
          createPayoutOrderCall = Payout.createPayoutOrder payoutServiceName merchantOpCityId person.id mbPersonBankAccount
      void $
        DPayment.createPayoutService
          (cast merchantId)
          (Just $ cast merchantOpCityId)
          (cast driverId)
          (Just [referenceId])
          (Just entityName)
          (show merchantOperatingCity.city)
          createPayoutOrderReq
          createPayoutOrderCall
          Nothing
          afterPayoutOrderCreated
      logInfo $
        "Created incentive journey cash payout referenceId="
          <> referenceId
          <> " amount="
          <> show amountRupees
          <> " driver="
          <> driverId.getId
      pure (IJ.Awarded amountRupees)

-- | After a verified UPI id is saved, pay cash milestones from the last 7 days
-- whose target was reached and whose payout never started. Status becomes Completed only when the payout order exists.
retryVerifiedVpaJourneyCashPayouts ::
  JourneyCashPayoutFlow m r =>
  Id DP.Person ->
  m ()
retryVerifiedVpaJourneyCashPayouts driverId = do
  outcome <- withTryCatch "IncentiveJourney:retryVerifiedVpaJourneyCashPayouts" $ do
    driverInfo <- QDriverInfo.findById (cast driverId) >>= fromMaybeM DriverInfoNotFound
    let vpaVerified =
          isJust driverInfo.payoutVpa
            && driverInfo.payoutVpaStatus `elem` [Just DI.VIA_WEBHOOK, Just DI.VERIFIED_BY_USER]
    when vpaVerified $ do
      person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      transporterConfig <-
        getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) Nothing
          >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
      now <- getCurrentTime
      let since = addUTCTime (negate $ 7 * 24 * 60 * 60) now
      pending <- QStatsExtra.findAwaitingCashByPersonSince (cast driverId) since
      forM_ pending $ \row -> do
        rowOutcome <-
          withTryCatch "IncentiveJourney:payOneAwaitingCashRow" $
            payOneAwaitingCashRow driverId person.merchantId person.merchantOperatingCityId transporterConfig row
        case rowOutcome of
          Left err ->
            logError $
              "Incentive journey cash backfill row failed driver="
                <> driverId.getId
                <> " statsId="
                <> row.id.getId
                <> " error="
                <> show err
          Right () -> pure ()
  case outcome of
    Left err ->
      logError $
        "Incentive journey cash VPA backfill failed driver="
          <> driverId.getId
          <> " error="
          <> show err
    Right () -> pure ()

payOneAwaitingCashRow ::
  JourneyCashPayoutFlow m r =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  DIJS.IncentiveJourneyStats ->
  m ()
payOneAwaitingCashRow driverId merchantId merchantOpCityId transporterConfig stats = do
  let targetMet =
        evaluateCondition
          (Convert.toLibConditionOperator stats.conditionOperator)
          stats.currentValue
          stats.conditionValue
  case stats.cohortJourneyMappingId of
    Nothing ->
      logInfo $ "Skipping incentive journey cash backfill; cohort journey mapping missing statsId=" <> stats.id.getId
    Just _ | not targetMet || stats.rewardType /= IJCommon.Cash -> pure ()
    Just cjmId -> do
      mbMilestone <- QMilestone.findById stats.milestoneId
      case mbMilestone of
        Just milestone
          | milestone.rewardType == IJCommon.Cash,
            Just amount <- milestone.rewardValue,
            amount > 0 -> do
            let referenceId =
                  "milestone:"
                    <> driverId.getId
                    <> ":"
                    <> cjmId.getId
                    <> ":"
                    <> stats.milestoneId.getId
                    <> ":"
                    <> stats.periodKey
            result <- awardJourneyCashPayout driverId merchantId merchantOpCityId transporterConfig referenceId amount
            case result of
              IJ.Awarded _ -> QStatsExtra.markJourneyCashPaid stats.id amount
              IJ.AwardSkipped -> pure ()
        _ ->
          logInfo $ "Skipping incentive journey cash backfill; milestone is not a positive cash reward statsId=" <> stats.id.getId

milestoneCashReferenceId :: Id DP.Person -> IJ.RewardDispatchCtx -> Text
milestoneCashReferenceId driverId ctx =
  "milestone:"
    <> driverId.getId
    <> ":"
    <> ctx.cohortJourneyMappingId
    <> ":"
    <> ctx.milestoneId
    <> ":"
    <> ctx.periodKey

streakCashReferenceId :: Id DP.Person -> Text -> Text
streakCashReferenceId driverId campaignKey =
  "streak:" <> driverId.getId <> ":" <> campaignKey

-- | Applies dashboard subscription waive config. Missing config keeps the previous
-- hardcoded default (100%, 1 day, YATRI_SUBSCRIPTION, WITHOUT_OFFER).
awardJourneySubscriptionWaiveOff ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  Text ->
  IJ.RewardSpec ->
  m IJ.AwardResult
awardJourneySubscriptionWaiveOff driverId contextLabel spec = do
  let waiveOff = fromMaybe defaultSubscriptionWaiveOff spec.subscriptionWaiveOff
  case (readMaybe (T.unpack waiveOff.serviceName), readMaybe (T.unpack waiveOff.waiveOffMode)) of
    (Nothing, _) -> do
      logInfo $ "SubscriptionWaiveOff skipped (invalid serviceName=" <> waiveOff.serviceName <> ") " <> contextLabel
      pure IJ.AwardSkipped
    (_, Nothing) -> do
      logInfo $ "SubscriptionWaiveOff skipped (invalid waiveOffMode=" <> waiveOff.waiveOffMode <> ") " <> contextLabel
      pure IJ.AwardSkipped
    (Just serviceName, Just waiveOfMode) -> do
      mbDriverPlan <- SQPlan.findByDriverIdAndServiceName driverId serviceName
      case mbDriverPlan of
        Nothing -> do
          logInfo $
            "SubscriptionWaiveOff skipped (no driver_plan service="
              <> waiveOff.serviceName
              <> ") "
              <> contextLabel
              <> " driver="
              <> driverId.getId
          pure IJ.AwardSkipped
        Just _ -> do
          SQPlan.updateWaiveOffPercantageAndType
            DDriverPlan.WaiveOffEntity
              { daysValidFor = fromIntegral waiveOff.daysValidFor,
                driverId = driverId.getId,
                percentage = fromIntegral waiveOff.percentage,
                serviceName = serviceName,
                waiveOfMode = waiveOfMode
              }
          logInfo $
            "SubscriptionWaiveOff applied ("
              <> show waiveOff.percentage
              <> "%, daysValidFor="
              <> show waiveOff.daysValidFor
              <> ", service="
              <> waiveOff.serviceName
              <> ", mode="
              <> waiveOff.waiveOffMode
              <> ") "
              <> contextLabel
              <> " driver="
              <> driverId.getId
          pure (IJ.Awarded waiveOff.percentage)

defaultSubscriptionWaiveOff :: IJ.SubscriptionWaiveOffSpec
defaultSubscriptionWaiveOff =
  IJ.SubscriptionWaiveOffSpec
    { percentage = 100,
      daysValidFor = 1,
      serviceName = "YATRI_SUBSCRIPTION",
      waiveOffMode = "WITHOUT_OFFER"
    }

-- | Dashboard waive-off for one driver milestone period row.
waiveDriverMilestone ::
  JourneyRewardDispatchFlow m r =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  DIJ.IncentiveJourney ->
  Id DCJM.CohortJourneyMapping ->
  Id DIJM.IncentiveJourneyMilestone ->
  Text ->
  Maybe DTV.VehicleCategory ->
  Maybe DCommon.ServiceTierType ->
  m ()
waiveDriverMilestone driverId merchantId merchantOpCityId transporterConfig journey cohortJourneyMappingId milestoneId periodKey mbVehCategory mbServiceTier = do
  let vehCategory = fromMaybe DTV.CAR mbVehCategory
      ijHandle =
        IJ.IncentiveJourneyHandle
          { actor = IJ.DriverActor,
            loadJourneysByIds = \_ -> pure [journey],
            loadMilestones = loadJourneyMilestones,
            dispatchReward = \j ctx spec ->
              dispatchDriverReward driverId merchantId merchantOpCityId transporterConfig j vehCategory mbServiceTier ctx spec,
            dispatchStreakEndReward = \j campaignKey spec ->
              dispatchDriverStreakEndReward driverId merchantId merchantOpCityId transporterConfig j vehCategory mbServiceTier campaignKey spec,
            mbOnMilestoneCompleted = Nothing,
            mbOnMgOptInCleared = Nothing,
            buildMetricContext = MetricBag.buildMetricContext
          }
  didWaive <-
    IJ.waiveMilestoneForPeriod
      ijHandle
      (cast driverId)
      (cast merchantId)
      (cast merchantOpCityId)
      (T.pack . show <$> mbVehCategory)
      journey
      cohortJourneyMappingId
      milestoneId
      periodKey
  when didWaive $ do
    milestones <- loadJourneyMilestones journey.id
    whenJust (find (\m -> m.id == milestoneId) milestones) $ \milestone ->
      void $
        withTryCatch "IncentiveJourney:sendMilestoneWaivedOverlay" $
          sendMilestoneWaivedOverlay merchantOpCityId driverId journey milestone milestones

-- | Dashboard: diagnose streak-end / periodDays-slice reward; optionally trigger if ReadyToAward.
-- Scoped stats only (period keys the award path checks). Idempotent on trigger via campaignKey.
inspectOrTriggerStreakEndReward ::
  JourneyRewardDispatchFlow m r =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  Text ->
  Bool ->
  Maybe DTV.VehicleCategory ->
  Maybe DCommon.ServiceTierType ->
  m IJ.StreakEndInspectResult
inspectOrTriggerStreakEndReward driverId merchantId merchantOpCityId transporterConfig journey cjm periodKey trigger mbVehCategory mbServiceTier = do
  let vehCategory = fromMaybe DTV.CAR mbVehCategory
  milestones <- loadJourneyMilestones journey.id
  let dispatchStreak j =
        dispatchDriverStreakEndReward
          driverId
          merchantId
          merchantOpCityId
          transporterConfig
          j
          vehCategory
          mbServiceTier
  IJ.inspectAndMaybeTriggerStreakEnd
    dispatchStreak
    (cast driverId)
    journey
    cjm
    milestones
    periodKey
    trigger
    transporterConfig.timeDiffFromUtc

toLibDeltas :: IncentiveMetrics.RideIncentiveDeltas -> IJ.RideDeltas
toLibDeltas deltas =
  IJ.RideDeltas
    { ridesDelta = deltas.ridesDelta,
      earningsDelta = deltas.earningsDelta,
      netEarningsDelta = deltas.netEarningsDelta,
      distanceMetersDelta = deltas.distanceMetersDelta,
      rideTimeSecondsDelta = deltas.rideTimeSecondsDelta
    }
