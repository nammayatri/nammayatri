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
    isJourneyWindowActiveFor,
    mkJourneyPeriodKeyFor,
    waiveMilestoneForPeriod,
    tryAwardStreakEnd,
  )
where

import Data.List (nubBy, partition, sortOn)
import Kernel.Prelude
import Kernel.Types.Error (GenericError (InternalError, InvalidRequest))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney.BehaviorMetric as BM
import qualified Lib.IncentiveJourney.Convert as Convert
import qualified Lib.IncentiveJourney.Domain.Types.CohortDetails as DCD
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats as DIJS
import qualified Lib.IncentiveJourney.Eval as Eval
import qualified Lib.IncentiveJourney.JsonLogic as JsonLogic
import qualified Lib.IncentiveJourney.Period as Period
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.CachedQueries.Assignment as CQAssignment
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourneyStats as CQStats
import qualified Lib.IncentiveJourney.Storage.Queries.CohortDetailsExtra as QCDExtra
import qualified Lib.IncentiveJourney.Storage.Queries.CohortJourneyMappingExtra as QCJMExtra
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyStatsExtra as QStats
import qualified Lib.IncentiveJourney.Storage.Queries.UserCohortMapping as QUCM
import qualified Lib.IncentiveJourney.Storage.Queries.UserCohortMappingExtra as QUCMExtra
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
    -- | Driver-app: mark mg_campaign.optedIn=False after OptIn disqualify (keep membership marker).
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
mkJourneyPeriodKeyFor localTime journey startDate =
  Period.mkJourneyPeriodKeyWithStart
    localTime
    (Convert.toLibPeriodType journey.journeyType)
    startDate

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
  -- MG: accumulate net earnings inside vs outside milestone timeBounds (assignment already requires opted-in).
  -- Before milestone eval so JsonLogic / legacy upserts preserve these columns. No payout job here.
  when (fromMaybe False cjm.isMg) $
    accumulateMgNetEarnings
      ijHandle
      input.personId
      input.merchantId
      input.merchantOperatingCityId
      journey
      cjm
      periodKey
      localTime
      milestones
      input.rideDeltas.netEarningsDelta
  -- Legacy: no conditionLogic — existing mid-ride sequential delta eval (unchanged).
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
  -- Bag eval: conditionLogic (award or NoReward exit-gate) against full Redis + period bag.
  unless (null bagEvalMilestones) $ do
    metrics <-
      ijHandle.buildMetricContext
        input.personId
        cjm.id
        periodKey
        (Just input.rideDeltas)
    disqualified <-
      evaluateMilestonesUntilDisqualified
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
    -- Streak-end only if still on the journey (Disqualify disables UCM).
    unless disqualified $
      when (StreakEnd.hasStreakEndRewardConfigured cjm) $
        tryAwardStreakEnd ijHandle input.personId periodKey journey cjm milestones
  when (null bagEvalMilestones) $
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
  -- Bag-eval milestones (conditionLogic / simple behaviour columns).
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
    disqualified <-
      evaluateMilestonesUntilDisqualified
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
    unless disqualified $
      when (StreakEnd.hasStreakEndRewardConfigured cjm) $
        tryAwardStreakEnd ijHandle input.personId periodKey journey cjm milestones

-- | Needs full metric bag (not mid-ride delta-only legacy path).
needsBagEval :: DIJM.IncentiveJourneyMilestone -> Bool
needsBagEval milestone = isJust milestone.conditionLogic

-- | When True, a failed condition (JsonLogic/columns) Disqualifies instead of InProgress.
shouldDisqualifyOnConditionFail :: DIJM.IncentiveJourneyMilestone -> Bool
shouldDisqualifyOnConditionFail milestone =
  fromMaybe False milestone.disqualifyIfConditionNotMet

-- | Milestones re-checked after cancel / accept / online BT bumps.
isBehaviorTickMilestone :: DIJM.IncentiveJourneyMilestone -> Bool
isBehaviorTickMilestone milestone =
  needsBagEval milestone
    || case milestone.conditionType of
      DIJM.CancellationRate -> True
      DIJM.AcceptanceRate -> True
      DIJM.OnlineMinutes -> True
      _ -> False

-- | Evaluate in order; stop after Disqualify (UCM disabled — journey membership ends).
evaluateMilestonesUntilDisqualified ::
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
evaluateMilestonesUntilDisqualified _ _ _ _ _ _ _ _ _ _ _ [] = pure False
evaluateMilestonesUntilDisqualified ijHandle personId merchantId merchantOpCityId localTime journey cjm periodKey milestones metrics dispatchCtx (milestone : rest) = do
  disqualified <-
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
  if disqualified
    then pure True
    else
      evaluateMilestonesUntilDisqualified
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

-- | Returns True when this milestone Disqualified the driver.
-- condition True → Complete (or InProgress if NoReward). False → InProgress, or Disqualified when flag set.
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
      if not (Window.isWithinTimeBound localTime milestone.timeBounds)
        then pure False
        else do
          let currentValue = BM.metricValueForCondition evalMilestone.conditionType metrics
              metricSnapshot = JsonLogic.metricSnapshotForRule milestone.conditionLogic metrics.vars
              disqualifyOnFail = shouldDisqualifyOnConditionFail milestone
          case BM.evaluateBehaviorCondition milestone.conditionLogic evalMilestone.conditionType evalMilestone.conditionOperator milestone.conditionValue metrics of
            Left err -> do
              logError $
                "JsonLogic eval failed milestone="
                  <> milestone.id.getId
                  <> " err="
                  <> err
              pure False
            Right False
              | disqualifyOnFail -> do
                applyDisqualify ijHandle personId merchantId merchantOpCityId journey cjm periodKey milestone currentValue metricSnapshot
                pure True
              | otherwise -> do
                upsertJsonLogicStats ijHandle personId merchantId merchantOpCityId journey cjm periodKey milestone currentValue metricSnapshot DIJS.InProgress Nothing
                pure False
            Right True
              | milestone.rewardType == Common.NoReward -> do
                -- Eligibility / stay check only — no award.
                upsertJsonLogicStats ijHandle personId merchantId merchantOpCityId journey cjm periodKey milestone currentValue metricSnapshot DIJS.InProgress Nothing
                pure False
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
                    upsertJsonLogicStats ijHandle personId merchantId merchantOpCityId journey cjm periodKey milestone currentValue metricSnapshot DIJS.RewardPending Nothing
                  IJ.Awarded awarded -> do
                    upsertJsonLogicStats ijHandle personId merchantId merchantOpCityId journey cjm periodKey milestone currentValue metricSnapshot DIJS.Completed (Just awarded)
                    case ijHandle.mbOnMilestoneCompleted of
                      Just onCompleted -> onCompleted journey milestone milestones awarded
                      Nothing -> pure ()
                pure False

applyDisqualify ::
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
  m ()
applyDisqualify ijHandle personId merchantId merchantOpCityId journey cjm periodKey milestone currentValue metricSnapshot = do
  upsertJsonLogicStats ijHandle personId merchantId merchantOpCityId journey cjm periodKey milestone currentValue metricSnapshot DIJS.Disqualified Nothing
  -- OptIn: clear optedInAt only (invite stays enabled → can re-opt next period).
  -- Assigned: disable UCM (no opt-in field to clear).
  mbCohort <- QCDExtra.findCohortDetailsById cjm.cohortId
  let isOptIn = maybe False ((== DCD.OptIn) . fromMaybe DCD.Assigned . (.cohortType)) mbCohort
  if isOptIn
    then do
      QUCMExtra.clearOptInUserCohortMapping personId cjm.id
      whenJust ijHandle.mbOnMgOptInCleared $ \onCleared -> onCleared personId cjm.id
    else QUCMExtra.disableUserCohortMapping personId cjm.id
  CQAssignment.clearCacheByPersonId ijHandle.actor personId
  logInfo $
    "IncentiveJourney Disqualified person="
      <> personId.getId
      <> " journey="
      <> journey.id.getId
      <> " cjm="
      <> cjm.id.getId
      <> " milestone="
      <> milestone.id.getId
      <> " optInClear="
      <> show isOptIn

-- | MG-only: add this ride's net earnings to the period stats row.
-- Inside milestone timeBounds → mgNetEarnings; outside → nonMgNetEarnings.
-- Skips terminal rows. Creates a NotStarted/InProgress shell when no row yet (e.g. first ride outside peak).
accumulateMgNetEarnings ::
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
accumulateMgNetEarnings _ _ _ _ _ _ _ _ _ 0 = pure ()
accumulateMgNetEarnings ijHandle personId merchantId merchantOpCityId journey cjm periodKey localTime milestones netDelta = do
  let deltaMoney = toHighPrecMoney netDelta
  forM_ milestones $ \milestone -> do
    mbExisting <-
      QStats.findStatsByPersonCjmAndMilestonePeriod personId cjm.id milestone.id periodKey
    unless (maybe False (IJ.isTerminalStatus . Convert.toLibStatus . (.status)) mbExisting) $ do
      let within = Window.isWithinTimeBound localTime milestone.timeBounds
          existingMg = fromMaybe 0 (mbExisting >>= (.mgNetEarnings))
          existingNonMg = fromMaybe 0 (mbExisting >>= (.nonMgNetEarnings))
          (newMg, newNonMg) =
            if within
              then (Just (existingMg + deltaMoney), Just existingNonMg)
              else (Just existingMg, Just (existingNonMg + deltaMoney))
      now <- getCurrentTime
      statsId <- maybe generateGUID (pure . (.id)) mbExisting
      cohortType <- resolveStatsCohortType mbExisting cjm.cohortId
      let isMg = resolveStatsIsMg mbExisting cjm
      void $
        CQStats.upsertJourneyStats
          ijHandle.actor
          DIJS.IncentiveJourneyStats
            { id = statsId,
              personId = personId,
              merchantId = merchantId,
              merchantOperatingCityId = merchantOpCityId,
              cohortId = cjm.cohortId,
              cohortType = cohortType,
              isMg = isMg,
              cohortJourneyMappingId = Just cjm.id,
              journeyId = journey.id,
              milestoneId = milestone.id,
              periodKey = periodKey,
              conditionType = milestone.conditionType,
              conditionOperator = milestone.conditionOperator,
              conditionValue = milestone.conditionValue,
              currentValue = maybe 0 (.currentValue) mbExisting,
              metricSnapshot = mbExisting >>= (.metricSnapshot),
              status = maybe DIJS.InProgress (.status) mbExisting,
              rewardType = milestone.rewardType,
              rewardValue = mbExisting >>= (.rewardValue),
              mgNetEarnings = newMg,
              nonMgNetEarnings = newNonMg,
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
upsertJsonLogicStats ijHandle personId merchantId merchantOpCityId journey cjm periodKey milestone currentValue metricSnapshot status mbAwarded = do
  mbExisting <-
    QStats.findStatsByPersonCjmAndMilestonePeriod personId cjm.id milestone.id periodKey
  now <- getCurrentTime
  statsId <- maybe generateGUID (pure . (.id)) mbExisting
  cohortType <- resolveStatsCohortType mbExisting cjm.cohortId
  let isMg = resolveStatsIsMg mbExisting cjm
  void $
    CQStats.upsertJourneyStats
      ijHandle.actor
      DIJS.IncentiveJourneyStats
        { id = statsId,
          personId = personId,
          merchantId = merchantId,
          merchantOperatingCityId = merchantOpCityId,
          cohortId = cjm.cohortId,
          cohortType = cohortType,
          isMg = isMg,
          cohortJourneyMappingId = Just cjm.id,
          journeyId = journey.id,
          milestoneId = milestone.id,
          periodKey = periodKey,
          conditionType = milestone.conditionType,
          conditionOperator = milestone.conditionOperator,
          conditionValue = milestone.conditionValue,
          currentValue = currentValue,
          metricSnapshot = metricSnapshot,
          status = status,
          rewardType = milestone.rewardType,
          rewardValue = mbAwarded,
          -- Preserve MG earnings accumulated earlier in this ride-end (or prior rides).
          mgNetEarnings = mbExisting >>= (.mgNetEarnings),
          nonMgNetEarnings = mbExisting >>= (.nonMgNetEarnings),
          createdAt = maybe now (.createdAt) mbExisting,
          updatedAt = now
        }

-- | Prefer existing snapshot; else load cohort (NULL cohortType → Assigned).
resolveStatsCohortType ::
  BeamFlow m r =>
  Maybe DIJS.IncentiveJourneyStats ->
  Id DCD.CohortDetails ->
  m (Maybe DCD.CohortType)
resolveStatsCohortType mbExisting cohortId =
  case mbExisting >>= (.cohortType) of
    Just ct -> pure (Just ct)
    Nothing -> do
      mbCohort <- QCDExtra.findCohortDetailsById cohortId
      pure $ Just $ fromMaybe DCD.Assigned (mbCohort >>= (.cohortType))

-- | Prefer existing snapshot; else CJM.isMg (NULL → False).
resolveStatsIsMg :: Maybe DIJS.IncentiveJourneyStats -> DCJM.CohortJourneyMapping -> Maybe Bool
resolveStatsIsMg mbExisting cjm =
  case mbExisting >>= (.isMg) of
    Just v -> Just v
    Nothing -> Just (fromMaybe False cjm.isMg)

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
      cohortType <- resolveStatsCohortType mbExisting cjm.cohortId
      let isMg = resolveStatsIsMg mbExisting cjm
      void $
        CQStats.upsertJourneyStats
          ijHandle.actor
          DIJS.IncentiveJourneyStats
            { id = statsId,
              personId = personId,
              merchantId = merchantId,
              merchantOperatingCityId = merchantOpCityId,
              cohortId = cjm.cohortId,
              cohortType = cohortType,
              isMg = isMg,
              cohortJourneyMappingId = Just cjm.id,
              journeyId = journey.id,
              milestoneId = milestone.id,
              periodKey = periodKey,
              conditionType = milestone.conditionType,
              conditionOperator = milestone.conditionOperator,
              conditionValue = milestone.conditionValue,
              currentValue = maybe milestone.conditionValue (\s -> max s.currentValue milestone.conditionValue) mbExisting,
              metricSnapshot = mbExisting >>= (.metricSnapshot),
              status = DIJS.WaivedOff,
              rewardType = milestone.rewardType,
              rewardValue = Nothing,
              mgNetEarnings = mbExisting >>= (.mgNetEarnings),
              nonMgNetEarnings = mbExisting >>= (.nonMgNetEarnings),
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
      if StreakEnd.hasStreakEndRewardConfigured cjm
        then tryAwardStreakEnd ijHandle personId periodKey journey cjm milestones
        else
          logInfo $
            "streak-end skipped after waive; no streakEndRewardType on cohort_journey_mapping journey="
              <> journey.id.getId
      pure True

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
  currentPeriodStats <- QStats.findStatsByPersonCjmAndPeriod personId cjm.id periodKey
  if not (StreakEnd.shouldAttemptStreakEnd journey cjm periodKey milestones currentPeriodStats)
    then
      logInfo $
        "Streak-end award skipped (not last period or last milestone incomplete) journey="
          <> journey.id.getId
          <> " person="
          <> personId.getId
          <> " periodKey="
          <> periodKey
    else do
      let neededPeriodKeys = StreakEnd.periodKeysForStreakEndEligibility journey cjm
      scopedStats <- QStats.findStatsByPersonCjmAndPeriodKeys personId cjm.id neededPeriodKeys
      let eligible = StreakEnd.isStreakEndEligible journey cjm periodKey milestones scopedStats
      if not eligible
        then
          logInfo $
            "Streak-end award not eligible yet (prior periods incomplete) journey="
              <> journey.id.getId
              <> " person="
              <> personId.getId
        else do
          let campaignKey = StreakEnd.mkStreakCampaignKey journey cjm
              rewardSpec = StreakEnd.streakEndRewardSpec cjm
          awardedResult <- ijHandle.dispatchStreakEndReward journey campaignKey rewardSpec
          case awardedResult of
            IJ.Awarded n
              | n > 0 ->
                logInfo $
                  "Streak-end awarded "
                    <> show n
                    <> " journey="
                    <> journey.id.getId
                    <> " campaignKey="
                    <> campaignKey
                    <> " person="
                    <> personId.getId
            _ ->
              logInfo $
                "Streak-end award skipped journey="
                  <> journey.id.getId
                  <> " campaignKey="
                  <> campaignKey

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
        Just milestone -> do
          mbExisting <-
            QStats.findStatsByPersonCjmAndMilestonePeriod
              personId
              cjm.id
              milestone.id
              periodKey
          now <- getCurrentTime
          statsId <- maybe generateGUID (pure . (.id)) mbExisting
          cohortType <- resolveStatsCohortType mbExisting cjm.cohortId
          let isMg = resolveStatsIsMg mbExisting cjm
          void $
            CQStats.upsertJourneyStats
              ijHandle.actor
              DIJS.IncentiveJourneyStats
                { id = statsId,
                  personId = personId,
                  merchantId = merchantId,
                  merchantOperatingCityId = merchantOpCityId,
                  cohortId = cjm.cohortId,
                  cohortType = cohortType,
                  isMg = isMg,
                  cohortJourneyMappingId = Just cjm.id,
                  journeyId = journey.id,
                  milestoneId = milestone.id,
                  periodKey = periodKey,
                  conditionType = milestone.conditionType,
                  conditionOperator = milestone.conditionOperator,
                  conditionValue = milestone.conditionValue,
                  currentValue = currentValue,
                  metricSnapshot = Nothing,
                  status = status,
                  rewardType = milestone.rewardType,
                  rewardValue = mbAwarded,
                  mgNetEarnings = mbExisting >>= (.mgNetEarnings),
                  nonMgNetEarnings = mbExisting >>= (.nonMgNetEarnings),
                  createdAt = maybe now (.createdAt) mbExisting,
                  updatedAt = now
                }
