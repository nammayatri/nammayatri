{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.Domain.Action.Evaluate
  ( EvaluateInput (..),
    BehaviorEvaluateInput (..),
    IncentiveJourneyHandle (..),
    evaluateIncentiveJourneys,
    evaluateBehaviorJourneys,
    syncBehaviorMetricAndSettle,
    settleClosedPeriodIfNeeded,
    isJourneyWindowActiveFor,
    mkJourneyPeriodKeyFor,
    waiveMilestoneForPeriod,
    tryAwardStreakEnd,
  )
where

import Data.List (nubBy, partition, sortOn)
import qualified Data.Map.Strict as Map
import Data.Time (Day, UTCTime (UTCTime))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error (GenericError (InternalError, InvalidRequest))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney.BehaviorMetric as BM
import qualified Lib.IncentiveJourney.Convert as Convert
import qualified Lib.IncentiveJourney.Deferred as Deferred
import qualified Lib.IncentiveJourney.Domain.Action.StatsSync as StatsSync
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats as DIJS
import qualified Lib.IncentiveJourney.Eval as Eval
import qualified Lib.IncentiveJourney.Evaluation as Evaluation
import qualified Lib.IncentiveJourney.Period as Period
import qualified Lib.IncentiveJourney.RewardMetadata as RM
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.CachedQueries.Assignment as CQAssignment
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourneyStats as CQStats
import qualified Lib.IncentiveJourney.Storage.Queries.CohortJourneyMappingExtra as QCJMExtra
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyStatsExtra as QStats
import qualified Lib.IncentiveJourney.Storage.Queries.UserCohortMapping as QUCM
import qualified Lib.IncentiveJourney.Streak as Streak
import qualified Lib.IncentiveJourney.StreakEnd as StreakEnd
import qualified Lib.IncentiveJourney.Types as IJ
import Lib.IncentiveJourney.Types.Actor (JourneyActor)
import qualified Lib.IncentiveJourney.Window as Window

data EvaluateInput = EvaluateInput
  { personId :: Id Common.Person,
    merchantId :: Id Common.Merchant,
    merchantOperatingCityId :: Id Common.MerchantOperatingCity,
    timeBoundReferenceUtc :: UTCTime,
    timeDiffFromUtc :: Seconds,
    rideDeltas :: IJ.RideDeltas,
    mbPickupSpecialLocationId :: Maybe Text,
    mbDropSpecialLocationId :: Maybe Text,
    mbRideVehicleCategory :: Maybe Text,
    mbRideServiceTierType :: Maybe Text
  }

-- | Kafka BehaviourMetricTick / any non-ride event: no ride deltas; full bag from handle.
data BehaviorEvaluateInput = BehaviorEvaluateInput
  { personId :: Id Common.Person,
    merchantId :: Id Common.Merchant,
    merchantOperatingCityId :: Id Common.MerchantOperatingCity,
    timeBoundReferenceUtc :: UTCTime,
    timeDiffFromUtc :: Seconds,
    mbRideVehicleCategory :: Maybe Text
  }

-- | Adapters. Heavy metric load (Redis + period ride hash) must run in Kafka callers only.
data IncentiveJourneyHandle m = IncentiveJourneyHandle
  { actor :: JourneyActor,
    loadJourneysByIds :: [Id DIJ.IncentiveJourney] -> m [DIJ.IncentiveJourney],
    loadMilestones :: Id DIJ.IncentiveJourney -> m [DIJM.IncentiveJourneyMilestone],
    dispatchReward :: DIJ.IncentiveJourney -> IJ.RewardDispatchCtx -> IJ.RewardSpec -> m IJ.AwardResult,
    dispatchStreakEndReward :: DIJ.IncentiveJourney -> Text -> IJ.RewardSpec -> m IJ.AwardResult,
    mbOnMilestoneCompleted :: Maybe (DIJ.IncentiveJourney -> DIJM.IncentiveJourneyMilestone -> [DIJM.IncentiveJourneyMilestone] -> Int -> m ()),
    -- | Driver-app: clear IJCampaign tag for this CJM after OptIn period clear.
    mbOnMgOptInCleared :: Maybe (Id Common.Person -> Id DCJM.CohortJourneyMapping -> m ()),
    -- | Full bag: behaviour Redis + persisted period ride counters.
    -- When mbDeltas is Just, persist/apply those ride deltas first then return the bag.
    buildMetricContext ::
      Id Common.Person ->
      Id DCJM.CohortJourneyMapping ->
      Text ->
      Maybe IJ.RideDeltas ->
      m BM.MetricContext
  }

isJourneyWindowActiveFor :: UTCTime -> DIJ.IncentiveJourney -> DCJM.CohortJourneyMapping -> Bool
isJourneyWindowActiveFor localTime journey cjm =
  let endDate =
        Streak.computeStreakEndDate
          cjm.startDate
          cjm.streakRange
          journey.journeyType
   in Window.isJourneyWindowActive localTime cjm.startDate endDate

-- | Period key for this campaign. Pass CJM startDate so weekly/monthly mid-week
-- / mid-month starts use anchored keys.
mkJourneyPeriodKeyFor :: UTCTime -> DIJ.IncentiveJourney -> UTCTime -> Text
mkJourneyPeriodKeyFor localTime journey =
  Period.mkJourneyPeriodKeyWithStart
    localTime
    (Convert.toLibPeriodType journey.journeyType)

evaluateIncentiveJourneys ::
  (BeamFlow m r) =>
  IncentiveJourneyHandle m ->
  EvaluateInput ->
  m ()
evaluateIncentiveJourneys ijHandle input = do
  assignments <- CQAssignment.findAssignmentsForDriver ijHandle.actor input.personId input.merchantId input.merchantOperatingCityId input.mbRideVehicleCategory
  let uniqueAssignments = nubBy (\assignment other -> assignment.cohortJourneyMapping.id == other.cohortJourneyMapping.id) assignments
  when (null uniqueAssignments) $
    logInfo $
      "evaluateIncentiveJourneys called with no assignments for person " <> input.personId.getId
  unless (null uniqueAssignments) $ do
    let localTime = addUTCTime (secondsToNominalDiffTime input.timeDiffFromUtc) input.timeBoundReferenceUtc
        mappedJourneyIds = map ((.journeyId) . (.cohortJourneyMapping)) uniqueAssignments
    journeys <- ijHandle.loadJourneysByIds mappedJourneyIds
    -- All assigned journeys: legacy simple columns + JsonLogic (full bag) on Kafka ride-end.
    let matching = journeys
        journeysWithAssignment =
          [ (journey, assignment)
            | journey <- matching,
              assignment <- uniqueAssignments,
              let cjm = assignment.cohortJourneyMapping,
              cjm.journeyId == journey.id,
              cjm.enabled
          ]
        activeMatching =
          filter
            ( \(journey, assignment) ->
                isJourneyWindowActiveFor localTime journey assignment.cohortJourneyMapping
            )
            journeysWithAssignment
    case activeMatching of
      [] ->
        if null matching
          then
            logInfo $
              "No matching IncentiveJourney for person "
                <> input.personId.getId
                <> " mappedJourneyIds="
                <> show (map (.getId) mappedJourneyIds)
          else
            logInfo $
              "IncentiveJourney(s) mapped for person "
                <> input.personId.getId
                <> " but outside active streak window; skipping evaluation"
      journeysToEvaluate ->
        forM_ journeysToEvaluate $ \(journey, assignment) ->
          evaluateSingleJourney ijHandle input localTime journey assignment.cohortJourneyMapping

-- | Kafka BehaviourMetricTick: same milestone rules; bag without new ride deltas.
evaluateBehaviorJourneys ::
  (BeamFlow m r) =>
  IncentiveJourneyHandle m ->
  BehaviorEvaluateInput ->
  m ()
evaluateBehaviorJourneys ijHandle input = do
  assignments <- CQAssignment.findAssignmentsForDriver ijHandle.actor input.personId input.merchantId input.merchantOperatingCityId input.mbRideVehicleCategory
  let uniqueAssignments = nubBy (\assignment other -> assignment.cohortJourneyMapping.id == other.cohortJourneyMapping.id) assignments
  when (null uniqueAssignments) $
    logInfo $
      "evaluateBehaviorJourneys called with no assignments for person " <> input.personId.getId
  unless (null uniqueAssignments) $ do
    let localTime = addUTCTime (secondsToNominalDiffTime input.timeDiffFromUtc) input.timeBoundReferenceUtc
        mappedJourneyIds = map ((.journeyId) . (.cohortJourneyMapping)) uniqueAssignments
    journeys <- ijHandle.loadJourneysByIds mappedJourneyIds
    let journeysWithAssignment =
          [ (journey, assignment)
            | journey <- journeys,
              assignment <- uniqueAssignments,
              let cjm = assignment.cohortJourneyMapping,
              cjm.journeyId == journey.id,
              cjm.enabled
          ]
        activeMatching =
          filter
            ( \(journey, assignment) ->
                isJourneyWindowActiveFor localTime journey assignment.cohortJourneyMapping
            )
            journeysWithAssignment
    forM_ activeMatching $ \(journey, assignment) ->
      evaluateSingleJourneyOnTick ijHandle input localTime journey assignment.cohortJourneyMapping

evaluateSingleJourney ::
  (BeamFlow m r) =>
  IncentiveJourneyHandle m ->
  EvaluateInput ->
  UTCTime ->
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  m ()
evaluateSingleJourney ijHandle input localTime journey cjm = do
  let periodKey = mkJourneyPeriodKeyFor localTime journey cjm.startDate
  milestones <- sortOn (.order) <$> ijHandle.loadMilestones journey.id
  logInfo $
    "Evaluating IncentiveJourney "
      <> journey.id.getId
      <> " journeyType="
      <> show journey.journeyType
      <> " periodKey="
      <> periodKey
      <> " cohortJourneyMappingId="
      <> cjm.id.getId
      <> " streakEndConfigured="
      <> show (StreakEnd.hasStreakEndRewardConfigured cjm)
      <> " milestones="
      <> show (length milestones)
  let dispatchCtx =
        IJ.RewardDispatchCtx
          { personId = input.personId.getId,
            journeyName = journey.name,
            cohortJourneyMappingId = cjm.id.getId,
            milestoneId = "",
            periodKey = periodKey
          }
      (legacyMilestones, bagEvalMilestones) = partition (not . needsBagEval) milestones
      store = mkMilestoneStore ijHandle input.personId input.merchantId input.merchantOperatingCityId journey cjm periodKey milestones
  -- Bag-eval campaigns: accumulate net earnings into currentMetrics before milestone eval.
  unless (null bagEvalMilestones) $
    accumulateCurrentMetricsEarnings
      ijHandle
      input.personId
      input.merchantId
      input.merchantOperatingCityId
      journey
      cjm
      periodKey
      localTime
      bagEvalMilestones
      input.rideDeltas.netEarningsDelta
  -- Legacy: no evaluation JSON — existing mid-ride sequential delta eval (unchanged).
  unless (null legacyMilestones) $
    Eval.evaluateMilestonesInOrderWith
      localTime
      (ijHandle.dispatchReward journey)
      dispatchCtx
      input.rideDeltas
      input.mbPickupSpecialLocationId
      input.mbDropSpecialLocationId
      input.mbRideVehicleCategory
      input.mbRideServiceTierType
      store
      (map Convert.toEvalMilestone legacyMilestones)
  -- Bag eval: evaluation.logic against full Redis + period bag. False → no-op (no disqualify).
  unless (null bagEvalMilestones) $ do
    metrics <-
      ijHandle.buildMetricContext
        input.personId
        cjm.id
        periodKey
        (Just input.rideDeltas)
    void $
      evaluateBagMilestones
        ijHandle
        input.personId
        input.merchantId
        input.merchantOperatingCityId
        localTime
        journey
        cjm
        periodKey
        milestones
        metrics
        dispatchCtx
        bagEvalMilestones
  -- Closed-period deferred settle is list-API only (not ride-end / behaviour sync).
  when (StreakEnd.hasStreakEndRewardConfigured cjm) $
    tryAwardStreakEnd ijHandle input.personId periodKey journey cjm milestones

evaluateSingleJourneyOnTick ::
  (BeamFlow m r) =>
  IncentiveJourneyHandle m ->
  BehaviorEvaluateInput ->
  UTCTime ->
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  m ()
evaluateSingleJourneyOnTick ijHandle input localTime journey cjm = do
  let periodKey = mkJourneyPeriodKeyFor localTime journey cjm.startDate
  milestones <- sortOn (.order) <$> ijHandle.loadMilestones journey.id
  -- Bag-eval milestones (evaluation JSON / simple behaviour columns).
  -- Ride-only legacy milestones stay on the ride-end path.
  let tickMilestones = filter isBehaviorTickMilestone milestones
      dispatchCtx =
        IJ.RewardDispatchCtx
          { personId = input.personId.getId,
            journeyName = journey.name,
            cohortJourneyMappingId = cjm.id.getId,
            milestoneId = "",
            periodKey = periodKey
          }
  when (null tickMilestones) $
    logDebug $
      "Behavior tick: no behaviour milestones for journey=" <> journey.id.getId
  unless (null tickMilestones) $ do
    metrics <-
      ijHandle.buildMetricContext
        input.personId
        cjm.id
        periodKey
        Nothing
    logInfo $
      "Behavior tick evaluating journey="
        <> journey.id.getId
        <> " periodKey="
        <> periodKey
        <> " tickMilestones="
        <> show (length tickMilestones)
    void $
      evaluateBagMilestones
        ijHandle
        input.personId
        input.merchantId
        input.merchantOperatingCityId
        localTime
        journey
        cjm
        periodKey
        milestones
        metrics
        dispatchCtx
        tickMilestones
    when (StreakEnd.hasStreakEndRewardConfigured cjm) $
      tryAwardStreakEnd ijHandle input.personId periodKey journey cjm milestones

-- | Needs full metric bag (not mid-ride delta-only legacy path).
-- Behaviour conditionTypes without evaluation.logic must not use ride deltas
-- (delta is always 0 → LTE with conditionValue ≥ 0 would falsely complete).
needsBagEval :: DIJM.IncentiveJourneyMilestone -> Bool
needsBagEval milestone =
  isJust milestone.evaluation
    || case milestone.conditionType of
      DIJM.CancellationRate -> True
      DIJM.AcceptanceRate -> True
      DIJM.OnlineMinutes -> True
      DIJM.JSONLogic -> True
      _ -> False

-- | Milestones re-checked after cancel / accept / online BT bumps.
isBehaviorTickMilestone :: DIJM.IncentiveJourneyMilestone -> Bool
isBehaviorTickMilestone = needsBagEval

-- | Evaluate bag milestones in order. Always returns False (disqualify removed).
evaluateBagMilestones ::
  (BeamFlow m r) =>
  IncentiveJourneyHandle m ->
  Id Common.Person ->
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  UTCTime ->
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  Text ->
  [DIJM.IncentiveJourneyMilestone] ->
  BM.MetricContext ->
  IJ.RewardDispatchCtx ->
  [DIJM.IncentiveJourneyMilestone] ->
  m Bool
evaluateBagMilestones _ _ _ _ _ _ _ _ _ _ _ [] = pure False
evaluateBagMilestones ijHandle personId merchantId merchantOpCityId localTime journey cjm periodKey milestones metrics dispatchCtx (milestone : rest) = do
  void $
    evaluateJsonLogicMilestone
      ijHandle
      personId
      merchantId
      merchantOpCityId
      localTime
      journey
      cjm
      periodKey
      milestones
      metrics
      dispatchCtx
      milestone
  evaluateBagMilestones
    ijHandle
    personId
    merchantId
    merchantOpCityId
    localTime
    journey
    cjm
    periodKey
    milestones
    metrics
    dispatchCtx
    rest

-- | Bag-eval: sync metrics always; immediate award only when not deferred.
-- Deferred (behavior condition / NetOff) → InProgress only; settle after period closes.
evaluateJsonLogicMilestone ::
  (BeamFlow m r) =>
  IncentiveJourneyHandle m ->
  Id Common.Person ->
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  UTCTime ->
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  Text ->
  [DIJM.IncentiveJourneyMilestone] ->
  BM.MetricContext ->
  IJ.RewardDispatchCtx ->
  DIJM.IncentiveJourneyMilestone ->
  m Bool
evaluateJsonLogicMilestone ijHandle personId merchantId merchantOpCityId localTime journey cjm periodKey milestones metrics dispatchCtx milestone = do
  mbExisting <-
    QStats.findStatsByPersonCjmAndMilestonePeriod personId cjm.id milestone.id periodKey
  if maybe False (IJ.isTerminalStatus . Convert.toLibStatus . (.status)) mbExisting
    then pure False
    else do
      let evalMilestone = Convert.toEvalMilestone milestone
          mbLogic = Evaluation.evaluationLogic milestone.evaluation
          deferred = Deferred.isDeferredMilestone milestone
      if not (Window.isWithinTimeBound localTime milestone.timeBounds) && not deferred
        then pure False
        else do
          -- Stats is progressive SoT; Redis may be empty after eviction — never under-count.
          let statsCtx = BM.metricContextFromCurrentMetrics (mbExisting >>= (.currentMetrics))
              metrics' = BM.progressiveMergeMetricContext statsCtx metrics
              currentValue = BM.metricValueForCondition evalMilestone.conditionType metrics'
              baseMetrics =
                Evaluation.mergeCurrentMetrics
                  (mbExisting >>= (.currentMetrics))
                  metrics'.vars
              meta = RM.parseMilestoneRewardMeta milestone.rewardMetadata
              pct = Deferred.resolvePayoutPercent meta baseMetrics
              withRuleMetrics = Evaluation.setSlabPercent baseMetrics pct
          -- Always refresh metrics snapshot while in progress.
          upsertJsonLogicStats ijHandle personId merchantId merchantOpCityId journey cjm periodKey milestone currentValue withRuleMetrics DIJS.InProgress Nothing
          case BM.evaluateBehaviorCondition mbLogic evalMilestone.conditionType evalMilestone.conditionOperator milestone.conditionValue metrics' of
            Left err -> do
              logError $
                "JsonLogic eval failed milestone="
                  <> milestone.id.getId
                  <> " err="
                  <> err
              pure False
            Right False -> pure False
            Right True
              | deferred ->
                -- Wait for closed-period settlement (NetOff pays even if false later via settle).
                pure False
              | milestone.rewardType == Common.NoReward -> pure False
              | otherwise -> do
                let rewardCtx =
                      IJ.RewardDispatchCtx
                        { personId = dispatchCtx.personId,
                          journeyName = dispatchCtx.journeyName,
                          cohortJourneyMappingId = dispatchCtx.cohortJourneyMappingId,
                          milestoneId = milestone.id.getId,
                          periodKey = dispatchCtx.periodKey
                        }
                awardedResult <-
                  ijHandle.dispatchReward
                    journey
                    rewardCtx
                    evalMilestone.rewardSpec
                case awardedResult of
                  IJ.AwardSkipped ->
                    upsertJsonLogicStats ijHandle personId merchantId merchantOpCityId journey cjm periodKey milestone currentValue withRuleMetrics DIJS.RewardPending Nothing
                  IJ.Awarded awarded -> do
                    upsertJsonLogicStats ijHandle personId merchantId merchantOpCityId journey cjm periodKey milestone currentValue withRuleMetrics DIJS.Completed (Just awarded)
                    case ijHandle.mbOnMilestoneCompleted of
                      Just onCompleted -> onCompleted journey milestone milestones awarded
                      Nothing -> pure ()
                pure False

-- | Bag-eval: add this ride's net earnings into currentMetrics.
-- Inside milestone timeBounds → earningInsideTimebound; else earningOutsideTimebound.
-- Does not skip terminal rows (period ledger for NetOffEarnings payout).
accumulateCurrentMetricsEarnings ::
  (BeamFlow m r) =>
  IncentiveJourneyHandle m ->
  Id Common.Person ->
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  Text ->
  UTCTime ->
  [DIJM.IncentiveJourneyMilestone] ->
  Int ->
  m ()
accumulateCurrentMetricsEarnings _ _ _ _ _ _ _ _ _ 0 = pure ()
accumulateCurrentMetricsEarnings ijHandle personId merchantId merchantOpCityId journey cjm periodKey localTime milestones netDelta = do
  let deltaMoney = toHighPrecMoney netDelta
  forM_ milestones $ \milestone ->
    StatsSync.withStatsRowLock personId cjm.id milestone.id periodKey $ do
      mbExisting <-
        QStats.findStatsByPersonCjmAndMilestonePeriod personId cjm.id milestone.id periodKey
      let within = Window.isWithinTimeBound localTime milestone.timeBounds
          newMetrics =
            Evaluation.bumpEarningsInCurrentMetrics
              (mbExisting >>= (.currentMetrics))
              within
              deltaMoney
      now <- getCurrentTime
      statsId <- maybe generateGUID (pure . (.id)) mbExisting
      campaignUserLinkType <- resolveStatsCampaignUserLinkType mbExisting cjm
      void $
        CQStats.upsertJourneyStats
          ijHandle.actor
          DIJS.IncentiveJourneyStats
            { id = statsId,
              personId = personId,
              merchantId = merchantId,
              merchantOperatingCityId = merchantOpCityId,
              cohortId = cjm.cohortId,
              campaignUserLinkType = campaignUserLinkType,
              cohortJourneyMappingId = Just cjm.id,
              journeyId = journey.id,
              milestoneId = milestone.id,
              periodKey = periodKey,
              conditionType = milestone.conditionType,
              conditionOperator = milestone.conditionOperator,
              conditionValue = milestone.conditionValue,
              currentValue = maybe 0 (.currentValue) mbExisting,
              currentMetrics = newMetrics,
              status = maybe DIJS.InProgress (.status) mbExisting,
              rewardType = milestone.rewardType,
              rewardValue = mbExisting >>= (.rewardValue),
              createdAt = maybe now (.createdAt) mbExisting,
              updatedAt = now
            }

upsertJsonLogicStats ::
  (BeamFlow m r) =>
  IncentiveJourneyHandle m ->
  Id Common.Person ->
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  Text ->
  DIJM.IncentiveJourneyMilestone ->
  Int ->
  Maybe Value ->
  DIJS.JourneyMilestoneStatus ->
  Maybe Int ->
  m ()
upsertJsonLogicStats ijHandle personId merchantId merchantOpCityId journey cjm periodKey milestone currentValue currentMetrics status mbAwarded =
  StatsSync.withStatsRowLock personId cjm.id milestone.id periodKey $ do
    mbExisting <-
      QStats.findStatsByPersonCjmAndMilestonePeriod personId cjm.id milestone.id periodKey
    now <- getCurrentTime
    statsId <- maybe generateGUID (pure . (.id)) mbExisting
    campaignUserLinkType <- resolveStatsCampaignUserLinkType mbExisting cjm
    -- Max-merge counters/earnings under the lock; rates recomputed inside maxMerge.
    -- Refresh slabPercent from merged metrics (do not keep a stale max-merged %).
    -- Terminal check outside the lock (evaluateJsonLogicMilestone) can race another writer
    -- that completes the row; re-read here and keep terminal status/reward (avoids InProgress
    -- regression + double dispatch). Settle may still move RewardPending → Completed / refresh Pending.
    let mergedCounts =
          Evaluation.maxMergeCurrentMetrics
            currentMetrics
            (mbExisting >>= (.currentMetrics))
        meta = RM.parseMilestoneRewardMeta milestone.rewardMetadata
        pct = Deferred.resolvePayoutPercent meta mergedCounts
        mergedMetrics = Evaluation.setSlabPercent mergedCounts pct
        mbExistingStatus = (.status) <$> mbExisting
        existingTerminal =
          maybe False (IJ.isTerminalStatus . Convert.toLibStatus) mbExistingStatus
        allowRewardPendingTransition =
          mbExistingStatus == Just DIJS.RewardPending
            && status `elem` [DIJS.Completed, DIJS.RewardPending]
        finalStatus =
          if existingTerminal && not allowRewardPendingTransition
            then fromMaybe status mbExistingStatus
            else status
        finalReward =
          if existingTerminal && not allowRewardPendingTransition
            then mbExisting >>= (.rewardValue)
            else mbAwarded
    void $
      CQStats.upsertJourneyStats
        ijHandle.actor
        DIJS.IncentiveJourneyStats
          { id = statsId,
            personId = personId,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOpCityId,
            cohortId = cjm.cohortId,
            campaignUserLinkType = campaignUserLinkType,
            cohortJourneyMappingId = Just cjm.id,
            journeyId = journey.id,
            milestoneId = milestone.id,
            periodKey = periodKey,
            conditionType = milestone.conditionType,
            conditionOperator = milestone.conditionOperator,
            conditionValue = milestone.conditionValue,
            currentValue = currentValue,
            currentMetrics = mergedMetrics,
            status = finalStatus,
            rewardType = milestone.rewardType,
            rewardValue = finalReward,
            createdAt = maybe now (.createdAt) mbExisting,
            updatedAt = now
          }

-- | Prefer existing snapshot; else CJM.campaignUserLinkType (NULL → Assigned).
resolveStatsCampaignUserLinkType ::
  Monad m =>
  Maybe DIJS.IncentiveJourneyStats ->
  DCJM.CohortJourneyMapping ->
  m (Maybe DCJM.CampaignUserLinkType)
resolveStatsCampaignUserLinkType mbExisting cjm =
  pure $
    case mbExisting >>= (.campaignUserLinkType) of
      Just ct -> Just ct
      Nothing -> Just $ fromMaybe DCJM.Assigned cjm.campaignUserLinkType

-- | Dashboard waive-off: mark one (person, campaign, milestone, periodKey) as WaivedOff,
-- then attempt streak-end payout if configured.
-- Returns True when a new WaivedOff row was written (False if already terminal / skipped).
waiveMilestoneForPeriod ::
  (BeamFlow m r) =>
  IncentiveJourneyHandle m ->
  Id Common.Person ->
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  Maybe Text ->
  DIJ.IncentiveJourney ->
  Id DCJM.CohortJourneyMapping ->
  Id DIJM.IncentiveJourneyMilestone ->
  Text ->
  m Bool
waiveMilestoneForPeriod ijHandle personId merchantId merchantOpCityId mbVehicleCategory journey cohortJourneyMappingId milestoneId periodKey = do
  milestones <- ijHandle.loadMilestones journey.id
  milestone <- case find (\m -> m.id == milestoneId) milestones of
    Just m -> pure m
    Nothing -> throwError (InternalError $ "Milestone not found for waive-off: " <> milestoneId.getId)
  cjm <-
    QCJMExtra.findCohortJourneyMappingById cohortJourneyMappingId
      >>= fromMaybeM (InvalidRequest $ "Cohort journey mapping not found: " <> cohortJourneyMappingId.getId)
  unless (cjm.journeyId == journey.id && cjm.merchantId == merchantId && cjm.merchantOperatingCityId == merchantOpCityId) $
    throwError (InvalidRequest "Cohort journey mapping does not match this journey and city")
  let streakPeriodKeys = Streak.listPeriodKeysInStreak journey.journeyType cjm.startDate cjm.streakRange
  unless (periodKey `elem` streakPeriodKeys) $
    throwError $
      InvalidRequest $
        "periodKey does not belong to this cohort journey mapping window: "
          <> periodKey
  assignments <-
    CQAssignment.findAssignmentsForDriver
      ijHandle.actor
      personId
      merchantId
      merchantOpCityId
      mbVehicleCategory
  mbUcm <- QUCM.findByUserIdAndCohortMappingId personId cjm.id
  let assignedNow =
        any
          ( \a ->
              a.cohortJourneyMapping.id == cjm.id
                && periodKey
                  `elem` Streak.listPeriodKeysInStreak
                    journey.journeyType
                    a.cohortJourneyMapping.startDate
                    a.cohortJourneyMapping.streakRange
          )
          assignments
  unless (assignedNow || isJust mbUcm) $
    throwError $
      InvalidRequest $
        "No cohort_journey_mapping assignment for person="
          <> personId.getId
          <> " cohortJourneyMappingId="
          <> cjm.id.getId
  waived <-
    StatsSync.withStatsRowLock personId cjm.id milestoneId periodKey $ do
      mbExisting <- QStats.findStatsByPersonCjmAndMilestonePeriod personId cjm.id milestoneId periodKey
      now <- getCurrentTime
      case mbExisting of
        Just existing
          | IJ.isTerminalStatus (Convert.toLibStatus existing.status) -> do
            logInfo $
              "waiveMilestoneForPeriod skipped; already terminal status="
                <> show existing.status
                <> " person="
                <> personId.getId
                <> " periodKey="
                <> periodKey
            pure False
        _ -> do
          whenJust cjm.maxWaiveOffCount $ \maxCount -> do
            periodStats <- QStats.findStatsByPersonCjmAndPeriod personId cjm.id periodKey
            let waivedCount = length $ filter ((== DIJS.WaivedOff) . (.status)) periodStats
            when (waivedCount >= maxCount) $
              throwError $
                InvalidRequest $
                  "Waive-off limit reached for this journey/period (max="
                    <> show maxCount
                    <> ", alreadyWaived="
                    <> show waivedCount
                    <> ")"
          statsId <- maybe generateGUID (pure . (.id)) mbExisting
          campaignUserLinkType <- resolveStatsCampaignUserLinkType mbExisting cjm
          void $
            CQStats.upsertJourneyStats
              ijHandle.actor
              DIJS.IncentiveJourneyStats
                { id = statsId,
                  personId = personId,
                  merchantId = merchantId,
                  merchantOperatingCityId = merchantOpCityId,
                  cohortId = cjm.cohortId,
                  campaignUserLinkType = campaignUserLinkType,
                  cohortJourneyMappingId = Just cjm.id,
                  journeyId = journey.id,
                  milestoneId = milestone.id,
                  periodKey = periodKey,
                  conditionType = milestone.conditionType,
                  conditionOperator = milestone.conditionOperator,
                  conditionValue = milestone.conditionValue,
                  currentValue = maybe milestone.conditionValue (\s -> max s.currentValue milestone.conditionValue) mbExisting,
                  currentMetrics = mbExisting >>= (.currentMetrics),
                  status = DIJS.WaivedOff,
                  rewardType = milestone.rewardType,
                  rewardValue = Nothing,
                  createdAt = maybe now (.createdAt) mbExisting,
                  updatedAt = now
                }
          logInfo $
            "WaivedOff milestone "
              <> milestoneId.getId
              <> " periodKey="
              <> periodKey
              <> " person="
              <> personId.getId
          pure True
  when waived $
    if StreakEnd.hasStreakEndRewardConfigured cjm
      then tryAwardStreakEnd ijHandle personId periodKey journey cjm milestones
      else
        logInfo $
          "streak-end skipped after waive; no streakEndRewardType on cohort_journey_mapping journey="
            <> journey.id.getId
  pure waived

-- | Behaviour tick / stats consumer: sync metric into stats only.
-- Closed-period settle runs from ride-end eval and list-API fork (not the hot path).
syncBehaviorMetricAndSettle ::
  (BeamFlow m r) =>
  IncentiveJourneyHandle m ->
  Id Common.Person ->
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  Seconds ->
  UTCTime -> -- occurredAt (UTC)
  [Text] -> -- cjmIds (empty → all assignments)
  Text -> -- metric base or actionType
  Int ->
  Maybe Text -> -- mbRideVehicleCategory
  m ()
syncBehaviorMetricAndSettle ijHandle personId merchantId merchantOpCityId timeDiffFromUtc occurredAt cjmIds metricOrAction delta mbVehCat = do
  case StatsSync.eventNameToMetricBase metricOrAction of
    Nothing ->
      logDebug $ "StatsSync: unknown metric/action=" <> metricOrAction
    Just metricBase -> do
      let localTime = addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) occurredAt
      assignments <- CQAssignment.findAssignmentsForDriver ijHandle.actor personId merchantId merchantOpCityId mbVehCat
      let uniqueAssignments = nubBy (\a b -> a.cohortJourneyMapping.id == b.cohortJourneyMapping.id) assignments
          filtered =
            if null cjmIds
              then uniqueAssignments
              else filter (\a -> a.cohortJourneyMapping.id.getId `elem` cjmIds) uniqueAssignments
      forM_ filtered $ \assignment -> do
        let cjm = assignment.cohortJourneyMapping
        when cjm.enabled $ do
          journeyList <- ijHandle.loadJourneysByIds [cjm.journeyId]
          forM_ journeyList $ \journey -> do
            when (isJourneyWindowActiveFor localTime journey cjm) $ do
              milestones <- sortOn (.order) <$> ijHandle.loadMilestones journey.id
              StatsSync.syncBehaviorMetricToStats
                ijHandle.actor
                personId
                merchantId
                merchantOpCityId
                journey
                cjm
                milestones
                localTime
                metricBase
                delta

-- | Idempotent closed-period settlement for deferred milestones (+ periodized NetOff streak).
-- List-API fork only. Catches up pending closed periods after the done-watermark.
-- Hot path (already caught up): 1 Redis GET → return (no lock, no DB).
-- Catch-up: one SETNX for the whole batch, then one stats read per pending period.
settleClosedPeriodIfNeeded ::
  (BeamFlow m r) =>
  IncentiveJourneyHandle m ->
  Id Common.Person ->
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  UTCTime ->
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  [DIJM.IncentiveJourneyMilestone] ->
  m ()
settleClosedPeriodIfNeeded ijHandle personId merchantId merchantOpCityId localTime journey cjm milestones = do
  let doneKey = Deferred.settleDoneKey personId.getId cjm.id.getId
  mbDonePeriod <- Hedis.withCrossAppRedis $ Hedis.get doneKey
  let keysToSettle =
        Deferred.pendingClosedPeriodKeysAfterDone localTime journey cjm mbDonePeriod
  -- Steady state: watermark already at last closed period → nothing to do.
  unless (null keysToSettle) $ do
    let lockKey = Deferred.settleLockKey personId.getId cjm.id.getId "catchup"
    acquired <-
      Hedis.withCrossAppRedis $
        Hedis.setNxExpire lockKey Deferred.settleLockTTLSeconds ("1" :: Text)
    -- Advance done-watermark only across a contiguous quiet prefix. Later quiet
    -- periods still settle (idempotent), but must not jump the watermark past a
    -- RewardPending day — that would skip retries until settleDone TTL expires.
    when acquired $
      void $
        foldM
          ( \contiguousQuiet closedPeriodKey -> do
              settleQuiet <-
                settleDeferredMilestonesForPeriod
                  ijHandle
                  personId
                  merchantId
                  merchantOpCityId
                  journey
                  cjm
                  milestones
                  closedPeriodKey
              if contiguousQuiet && settleQuiet
                then do
                  void $
                    Hedis.withCrossAppRedis $
                      Hedis.setExp
                        doneKey
                        closedPeriodKey
                        (Deferred.settleDoneTTLSeconds journey.journeyType)
                  pure True
                else pure False
          )
          True
          keysToSettle

-- | Returns True when this closed period needs no further settle retries:
-- no deferred RewardPending, and no unpaid periodized/classic streak payout
-- (AwardSkipped with positive expected amount keeps the period non-quiet).
settleDeferredMilestonesForPeriod ::
  (BeamFlow m r) =>
  IncentiveJourneyHandle m ->
  Id Common.Person ->
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  [DIJM.IncentiveJourneyMilestone] ->
  Text ->
  m Bool
settleDeferredMilestonesForPeriod ijHandle personId merchantId merchantOpCityId journey cjm milestones closedPeriodKey = do
  let deferredMs = filter Deferred.isDeferredMilestone milestones
      -- True = watermark may advance. AwardSkipped with positive expected streak payout → False.
      runStreakAward =
        if StreakEnd.hasStreakEndRewardConfigured cjm
          then tryAwardPeriodizedStreakReward ijHandle personId journey cjm milestones closedPeriodKey
          else pure True
  -- No deferred milestones: periodized / classic streak may still need a shot.
  if null deferredMs
    then runStreakAward
    else do
      -- One keyed fetch for this person+cjm+closed period (not a history scan).
      periodRows <- QStats.findStatsByPersonCjmAndPeriod personId cjm.id closedPeriodKey
      let rowByMilestone = Map.fromList [(row.milestoneId, row) | row <- periodRows]
          -- Already quiet: every deferred row is final. Skip metric/reward work.
          alreadyQuiet =
            all
              ( \m ->
                  maybe False (IJ.isDeferredSettleDoneStatus . Convert.toLibStatus . (.status)) (Map.lookup m.id rowByMilestone)
              )
              deferredMs
      if alreadyQuiet
        then runStreakAward
        else do
          forM_ deferredMs $ \milestone -> do
            let mbExisting = Map.lookup milestone.id rowByMilestone
            case mbExisting of
              Just row | IJ.isDeferredSettleDoneStatus (Convert.toLibStatus row.status) -> pure ()
              _ -> do
                redisMetrics <-
                  ijHandle.buildMetricContext personId cjm.id closedPeriodKey Nothing
                -- Prefer stats.currentMetrics over wiped Redis zeros for condition + payout.
                let statsCtx = BM.metricContextFromCurrentMetrics (mbExisting >>= (.currentMetrics))
                    metrics = BM.progressiveMergeMetricContext statsCtx redisMetrics
                    currentValue = BM.metricValueForCondition (Convert.toLibConditionType milestone.conditionType) metrics
                    baseMetrics =
                      Evaluation.mergeCurrentMetrics
                        (mbExisting >>= (.currentMetrics))
                        metrics.vars
                    meta = RM.parseMilestoneRewardMeta milestone.rewardMetadata
                    pct = Deferred.resolvePayoutPercent meta baseMetrics
                    withMetrics = Evaluation.setSlabPercent baseMetrics pct
                    mbLogic = Evaluation.evaluationLogic milestone.evaluation
                    evalMilestone = Convert.toEvalMilestone milestone
                    -- RewardPending already passed condition once; retry dispatch without re-gating.
                    isPendingRetry = maybe False ((== DIJS.RewardPending) . (.status)) mbExisting
                condOk <-
                  if isPendingRetry
                    then pure True
                    else case BM.evaluateBehaviorCondition mbLogic evalMilestone.conditionType evalMilestone.conditionOperator milestone.conditionValue metrics of
                      Left err -> do
                        logError $ "Settle JsonLogic failed milestone=" <> milestone.id.getId <> " err=" <> err
                        pure False
                      Right b -> pure b
                if milestone.rewardType == Common.NetOffEarnings
                  then do
                    let payout =
                          Deferred.computeNetOffMilestonePayout
                            milestone.rewardMetadata
                            milestone.rewardValue
                            withMetrics
                    if payout <= 0
                      then -- Idle / no top-up owed: never create a pending row; close InProgress/RewardPending ledger.
                      case mbExisting of
                        Just row
                          | row.status `elem` [DIJS.InProgress, DIJS.RewardPending] ->
                            upsertJsonLogicStats ijHandle personId merchantId merchantOpCityId journey cjm closedPeriodKey milestone currentValue withMetrics DIJS.Completed (Just 0)
                        _ -> pure ()
                      else do
                        let rewardCtx =
                              IJ.RewardDispatchCtx
                                { personId = personId.getId,
                                  journeyName = journey.name,
                                  cohortJourneyMappingId = cjm.id.getId,
                                  milestoneId = milestone.id.getId,
                                  periodKey = closedPeriodKey
                                }
                            cashSpec =
                              evalMilestone.rewardSpec
                                { IJ.rewardKind = IJ.Cash,
                                  IJ.rewardValue = Just payout
                                }
                        awardedResult <- ijHandle.dispatchReward journey rewardCtx cashSpec
                        case awardedResult of
                          -- payout > 0 but skipped (e.g. no VPA) → keep RewardPending for retry.
                          IJ.AwardSkipped ->
                            upsertJsonLogicStats ijHandle personId merchantId merchantOpCityId journey cjm closedPeriodKey milestone currentValue withMetrics DIJS.RewardPending (Just payout)
                          IJ.Awarded awarded -> do
                            upsertJsonLogicStats ijHandle personId merchantId merchantOpCityId journey cjm closedPeriodKey milestone currentValue withMetrics DIJS.Completed (Just awarded)
                            case ijHandle.mbOnMilestoneCompleted of
                              Just onCompleted -> onCompleted journey milestone milestones awarded
                              Nothing -> pure ()
                  else when condOk $ do
                    let rewardCtx =
                          IJ.RewardDispatchCtx
                            { personId = personId.getId,
                              journeyName = journey.name,
                              cohortJourneyMappingId = cjm.id.getId,
                              milestoneId = milestone.id.getId,
                              periodKey = closedPeriodKey
                            }
                    awardedResult <- ijHandle.dispatchReward journey rewardCtx evalMilestone.rewardSpec
                    case awardedResult of
                      IJ.AwardSkipped ->
                        upsertJsonLogicStats ijHandle personId merchantId merchantOpCityId journey cjm closedPeriodKey milestone currentValue withMetrics DIJS.RewardPending Nothing
                      IJ.Awarded awarded -> do
                        upsertJsonLogicStats ijHandle personId merchantId merchantOpCityId journey cjm closedPeriodKey milestone currentValue withMetrics DIJS.Completed (Just awarded)
                        case ijHandle.mbOnMilestoneCompleted of
                          Just onCompleted -> onCompleted journey milestone milestones awarded
                          Nothing -> pure ()
          -- Periodized streak (Coins/Cash/NetOff). Idempotent via campaignKey / cash referenceId.
          streakQuiet <- runStreakAward
          -- Quiet unless a deferred row is still awaiting payout retry, or streak AwardSkipped
          -- with a positive expected payout (do not advance watermark past that slice).
          finalRows <- QStats.findStatsByPersonCjmAndPeriod personId cjm.id closedPeriodKey
          let pending =
                any
                  ( \row ->
                      row.status == DIJS.RewardPending
                        && any (\m -> m.id == row.milestoneId) deferredMs
                  )
                  finalRows
          pure (not pending && streakQuiet)

-- | Interval streak reward when streakEndRewardMetadata.periodDays is set.
-- Coins/Cash/SWO: fixed streakEndRewardValue once every milestone is terminal for every
-- period key in the slice. NetOff: same gate, then payout from summed row deltas.
-- No periodDays → classic full-streak tryAwardStreakEndClassic.
-- Returns True when settle may advance the done-watermark (quiet). AwardSkipped with a
-- positive expected payout returns False so list settle retries the slice.
tryAwardPeriodizedStreakReward ::
  (BeamFlow m r) =>
  IncentiveJourneyHandle m ->
  Id Common.Person ->
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  [DIJM.IncentiveJourneyMilestone] ->
  Text ->
  m Bool
tryAwardPeriodizedStreakReward ijHandle personId journey cjm milestones closedPeriodKey =
  if not (StreakEnd.hasStreakEndRewardConfigured cjm)
    then pure True
    else do
      let streakMeta = RM.parseStreakRewardMeta cjm.streakEndRewardMetadata
          lenDays = Deferred.campaignLengthDays journey cjm
      case streakMeta.periodDays of
        Just pd
          | pd > 0,
            lenDays > 0,
            lenDays `mod` pd == 0 -> do
            let slices = Deferred.periodDaySlices cjm.startDate lenDays pd
                -- Only when closedPeriodKey is the slice's last day (not mid-slice).
                mbSlice = findSliceForPeriodKey journey cjm closedPeriodKey slices
            case mbSlice of
              Nothing -> pure True
              Just (_idx, sliceStart, sliceEnd) -> do
                let periodKeys =
                      filter
                        (periodKeyInDayRange journey cjm sliceStart sliceEnd)
                        (Streak.listPeriodKeysInStreak journey.journeyType cjm.startDate cjm.streakRange)
                allStats <- QStats.findStatsByPersonCjmAndPeriodKeys personId cjm.id periodKeys
                let eligible = StreakEnd.isSliceStreakEligible milestones periodKeys allStats
                    campaignKey = StreakEnd.mkStreakSliceCampaignKey journey cjm sliceStart
                if not eligible
                  then do
                    logInfo $
                      "Periodized streak skipped (slice incomplete) campaignKey="
                        <> campaignKey
                        <> " closedPeriodKey="
                        <> closedPeriodKey
                    pure True
                  else do
                    let (mbPayout, rewardSpec) =
                          StreakEnd.streakEndDispatchSpecAndPayout
                            cjm
                            streakMeta
                            [(row.periodKey, row.currentMetrics) | row <- allStats]
                    case cjm.streakEndRewardType of
                      Just Common.NetOffEarnings
                        | maybe True (<= 0) mbPayout -> pure True
                      _ -> do
                        awardedResult <- ijHandle.dispatchStreakEndReward journey campaignKey rewardSpec
                        case awardedResult of
                          IJ.Awarded n
                            | n > 0 -> do
                              logInfo $
                                "Periodized streak awarded "
                                  <> show n
                                  <> " campaignKey="
                                  <> campaignKey
                              pure True
                          IJ.Awarded _ -> pure True
                          IJ.AwardSkipped -> do
                            logInfo $
                              "Periodized streak skipped campaignKey="
                                <> campaignKey
                                <> " expectedPayout="
                                <> show mbPayout
                            -- Positive expected payout must stay non-quiet for watermark retry.
                            pure $ maybe True (<= 0) mbPayout
        _ ->
          -- No periodDays → classic all-periods-satisfied streak end (NetOff → Cash).
          tryAwardStreakEndClassic ijHandle personId closedPeriodKey journey cjm milestones

-- | Match only when closedPeriodKey is the last day of the slice (exclusive end − 1).
-- Mid-slice closed days must not pay the periodized streak (cash reference locks amount).
findSliceForPeriodKey ::
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  Text ->
  [(Int, Day, Day)] ->
  Maybe (Int, Day, Day)
findSliceForPeriodKey journey cjm periodKey =
  find
    ( \(_, _s, e) ->
        let lastDay = pred e
            lastKey =
              Period.mkJourneyPeriodKeyWithStart
                (UTCTime lastDay 0)
                (Convert.toLibPeriodType journey.journeyType)
                cjm.startDate
         in periodKey == lastKey
    )

periodKeyInDayRange ::
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  Day ->
  Day ->
  Text ->
  Bool
periodKeyInDayRange journey cjm startDay endDay periodKey =
  let days = [startDay .. pred endDay]
      keys =
        map
          ( \d ->
              Period.mkJourneyPeriodKeyWithStart
                (UTCTime d 0)
                (Convert.toLibPeriodType journey.journeyType)
                cjm.startDate
          )
          days
   in periodKey `elem` keys

tryAwardStreakEnd ::
  (BeamFlow m r) =>
  IncentiveJourneyHandle m ->
  Id Common.Person ->
  Text ->
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  [DIJM.IncentiveJourneyMilestone] ->
  m ()
tryAwardStreakEnd ijHandle personId periodKey journey cjm milestones = do
  -- periodDays campaigns pay per slice (not a single end-of-streak payout).
  let streakMeta = RM.parseStreakRewardMeta cjm.streakEndRewardMetadata
      lenDays = Deferred.campaignLengthDays journey cjm
  case streakMeta.periodDays of
    Just pd
      | pd > 0,
        lenDays > 0,
        lenDays `mod` pd == 0 ->
        void $ tryAwardPeriodizedStreakReward ijHandle personId journey cjm milestones periodKey
    _ ->
      void $ tryAwardStreakEndClassic ijHandle personId periodKey journey cjm milestones

-- | Classic streak-end: one payout when every period in the full streak is satisfied.
-- Returns True when settle may advance the done-watermark (quiet).
tryAwardStreakEndClassic ::
  (BeamFlow m r) =>
  IncentiveJourneyHandle m ->
  Id Common.Person ->
  Text ->
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  [DIJM.IncentiveJourneyMilestone] ->
  m Bool
tryAwardStreakEndClassic ijHandle personId periodKey journey cjm milestones = do
  currentPeriodStats <- QStats.findStatsByPersonCjmAndPeriod personId cjm.id periodKey
  if not (StreakEnd.shouldAttemptStreakEnd journey cjm periodKey milestones currentPeriodStats)
    then do
      logInfo $
        "Streak-end award skipped (not last period or last milestone incomplete) journey="
          <> journey.id.getId
          <> " person="
          <> personId.getId
          <> " periodKey="
          <> periodKey
      pure True
    else do
      let neededPeriodKeys = StreakEnd.periodKeysForStreakEndEligibility journey cjm
      scopedStats <- QStats.findStatsByPersonCjmAndPeriodKeys personId cjm.id neededPeriodKeys
      let eligible = StreakEnd.isStreakEndEligible journey cjm periodKey milestones scopedStats
      if not eligible
        then do
          logInfo $
            "Streak-end award not eligible yet (prior periods incomplete) journey="
              <> journey.id.getId
              <> " person="
              <> personId.getId
          pure True
        else do
          let campaignKey = StreakEnd.mkStreakCampaignKey journey cjm
              streakMeta = RM.parseStreakRewardMeta cjm.streakEndRewardMetadata
              (mbPayout, rewardSpec) =
                StreakEnd.streakEndDispatchSpecAndPayout
                  cjm
                  streakMeta
                  [(row.periodKey, row.currentMetrics) | row <- scopedStats]
          case cjm.streakEndRewardType of
            Just Common.NetOffEarnings
              | maybe True (<= 0) mbPayout -> do
                logInfo $
                  "Classic NetOff streak-end payout zero journey="
                    <> journey.id.getId
                    <> " campaignKey="
                    <> campaignKey
                    <> " person="
                    <> personId.getId
                pure True
            _ -> do
              awardedResult <- ijHandle.dispatchStreakEndReward journey campaignKey rewardSpec
              case awardedResult of
                IJ.Awarded n
                  | n > 0 -> do
                    logInfo $
                      "Streak-end awarded "
                        <> show n
                        <> " journey="
                        <> journey.id.getId
                        <> " campaignKey="
                        <> campaignKey
                        <> " person="
                        <> personId.getId
                    pure True
                IJ.Awarded _ -> pure True
                IJ.AwardSkipped -> do
                  logInfo $
                    "Streak-end award skipped journey="
                      <> journey.id.getId
                      <> " campaignKey="
                      <> campaignKey
                      <> " expectedPayout="
                      <> show mbPayout
                  pure $ maybe True (<= 0) mbPayout

mkMilestoneStore ::
  (BeamFlow m r) =>
  IncentiveJourneyHandle m ->
  Id Common.Person ->
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  Text ->
  [DIJM.IncentiveJourneyMilestone] ->
  Eval.MilestoneStore m
mkMilestoneStore ijHandle personId merchantId merchantOpCityId journey cjm periodKey milestones =
  Eval.MilestoneStore
    { loadStats = \evalMilestone -> do
        case lookupMilestone evalMilestone of
          Nothing -> pure Nothing
          Just milestone -> do
            mbStats <-
              QStats.findStatsByPersonCjmAndMilestonePeriod
                personId
                cjm.id
                milestone.id
                periodKey
            pure $ fmap Convert.toEvalStats mbStats,
      persistProgress = \evalMilestone currentValue ->
        void $ upsertStats evalMilestone currentValue DIJS.InProgress Nothing,
      persistAwaitingReward = \evalMilestone currentValue ->
        void $ upsertStats evalMilestone currentValue DIJS.RewardPending Nothing,
      persistCompleted = \evalMilestone currentValue awarded ->
        void $ upsertStats evalMilestone currentValue DIJS.Completed (Just awarded),
      onCompleted = \evalMilestone awarded ->
        case (ijHandle.mbOnMilestoneCompleted, lookupMilestone evalMilestone) of
          (Just onCompleted, Just milestone) -> onCompleted journey milestone milestones awarded
          _ -> pure ()
    }
  where
    lookupMilestone evalMilestone = find (\m -> m.id.getId == evalMilestone.milestoneId) milestones
    upsertStats evalMilestone currentValue status mbAwarded = do
      case lookupMilestone evalMilestone of
        Nothing -> pure ()
        Just milestone ->
          StatsSync.withStatsRowLock personId cjm.id milestone.id periodKey $ do
            mbExisting <-
              QStats.findStatsByPersonCjmAndMilestonePeriod
                personId
                cjm.id
                milestone.id
                periodKey
            now <- getCurrentTime
            statsId <- maybe generateGUID (pure . (.id)) mbExisting
            campaignUserLinkType <- resolveStatsCampaignUserLinkType mbExisting cjm
            void $
              CQStats.upsertJourneyStats
                ijHandle.actor
                DIJS.IncentiveJourneyStats
                  { id = statsId,
                    personId = personId,
                    merchantId = merchantId,
                    merchantOperatingCityId = merchantOpCityId,
                    cohortId = cjm.cohortId,
                    campaignUserLinkType = campaignUserLinkType,
                    cohortJourneyMappingId = Just cjm.id,
                    journeyId = journey.id,
                    milestoneId = milestone.id,
                    periodKey = periodKey,
                    conditionType = milestone.conditionType,
                    conditionOperator = milestone.conditionOperator,
                    conditionValue = milestone.conditionValue,
                    currentValue = currentValue,
                    currentMetrics = mbExisting >>= (.currentMetrics),
                    status = status,
                    rewardType = milestone.rewardType,
                    rewardValue = mbAwarded,
                    createdAt = maybe now (.createdAt) mbExisting,
                    updatedAt = now
                  }
