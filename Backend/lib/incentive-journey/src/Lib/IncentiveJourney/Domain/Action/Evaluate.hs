{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.Domain.Action.Evaluate
  ( EvaluateInput (..),
    IncentiveJourneyHandle (..),
    evaluateIncentiveJourneys,
    isJourneyWindowActiveFor,
    mkJourneyPeriodKeyFor,
    waiveMilestoneForPeriod,
    tryAwardStreakEnd,
  )
where

import Kernel.Prelude
import Kernel.Types.Error (GenericError (InternalError, InvalidRequest))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney.Assignment as Assignment
import qualified Lib.IncentiveJourney.Convert as Convert
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats as DIJS
import qualified Lib.IncentiveJourney.Eval as Eval
import qualified Lib.IncentiveJourney.Period as Period
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourneyStats as CQStats
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyStatsExtra as QStats
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

-- | App-injected adapters. ConfigPilot / Coins / overlays stay outside the library.
data IncentiveJourneyHandle m = IncentiveJourneyHandle
  { actor :: JourneyActor,
    loadEnabledJourneys :: Id Common.Merchant -> Id Common.MerchantOperatingCity -> m [DIJ.IncentiveJourney],
    loadMilestones :: Id Common.MerchantOperatingCity -> Id DIJ.IncentiveJourney -> m [DIJM.IncentiveJourneyMilestone],
    -- | Milestone-level reward (NoReward on milestone = complete with 0).
    dispatchReward :: DIJ.IncentiveJourney -> IJ.RewardDispatchCtx -> IJ.RewardSpec -> m IJ.AwardResult,
    -- | Streak-level reward when CJM has streakEndRewardType and all period buckets satisfied.
    dispatchStreakEndReward :: DIJ.IncentiveJourney -> Text -> IJ.RewardSpec -> m IJ.AwardResult,
    -- | Optional side-effect after milestone completes
    mbOnMilestoneCompleted :: Maybe (DIJ.IncentiveJourney -> DIJM.IncentiveJourneyMilestone -> [DIJM.IncentiveJourneyMilestone] -> Int -> m ())
  }

isJourneyWindowActiveFor :: UTCTime -> DIJ.IncentiveJourney -> DCJM.CohortJourneyMapping -> Bool
isJourneyWindowActiveFor localTime journey cjm =
  let endDate =
        Streak.computeStreakEndDate
          cjm.startDate
          cjm.streakRange
          (Convert.journeyTypeOrDefault journey.journeyType)
   in Window.isJourneyWindowActive localTime cjm.startDate endDate

mkJourneyPeriodKeyFor :: UTCTime -> DIJ.IncentiveJourney -> Text
mkJourneyPeriodKeyFor localTime journey =
  Period.mkJourneyPeriodKey
    localTime
    (Convert.toLibPeriodType $ Convert.journeyTypeOrDefault journey.journeyType)

evaluateIncentiveJourneys ::
  (BeamFlow m r) =>
  IncentiveJourneyHandle m ->
  EvaluateInput ->
  m ()
evaluateIncentiveJourneys ijHandle input = do
  assignments <- Assignment.findAssignmentsByUserId input.personId
  when (null assignments) $
    logInfo $
      "evaluateIncentiveJourneys called with no user_cohort_mapping rows for person " <> input.personId.getId
  unless (null assignments) $ do
    let localTime = addUTCTime (secondsToNominalDiffTime input.timeDiffFromUtc) input.timeBoundReferenceUtc
        mappedJourneyIds = map ((.journeyId) . (.cohortJourneyMapping)) assignments
    enabledJourneys <- ijHandle.loadEnabledJourneys input.merchantId input.merchantOperatingCityId
    let matching =
          filter
            (\j -> j.id `elem` mappedJourneyIds)
            enabledJourneys
        journeysWithAssignment =
          [ (journey, assignment)
            | journey <- matching,
              assignment <- assignments,
              assignment.cohortJourneyMapping.journeyId == journey.id
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

evaluateSingleJourney ::
  (BeamFlow m r) =>
  IncentiveJourneyHandle m ->
  EvaluateInput ->
  UTCTime ->
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  m ()
evaluateSingleJourney ijHandle input localTime journey cjm = do
  let periodKey = mkJourneyPeriodKeyFor localTime journey
  milestones <- ijHandle.loadMilestones input.merchantOperatingCityId journey.id
  logInfo $
    "Evaluating IncentiveJourney "
      <> journey.id.getId
      <> " journeyType="
      <> show (Convert.journeyTypeOrDefault journey.journeyType)
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
            milestoneId = ""
          }
      store = mkMilestoneStore ijHandle input journey periodKey milestones
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
    (map Convert.toEvalMilestone milestones)
  when (StreakEnd.hasStreakEndRewardConfigured cjm) $
    tryAwardStreakEnd ijHandle input.personId periodKey journey cjm milestones

-- | Dashboard waive-off: mark one (person, journey, milestone, periodKey) as WaivedOff,
-- then attempt streak-end payout if configured.
-- Returns True when a new WaivedOff row was written (False if already terminal / skipped).
waiveMilestoneForPeriod ::
  (BeamFlow m r) =>
  IncentiveJourneyHandle m ->
  Id Common.Person ->
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  DIJ.IncentiveJourney ->
  Id DIJM.IncentiveJourneyMilestone ->
  Text ->
  m Bool
waiveMilestoneForPeriod ijHandle personId merchantId merchantOpCityId journey milestoneId periodKey = do
  milestones <- ijHandle.loadMilestones merchantOpCityId journey.id
  milestone <- case find (\m -> m.id == milestoneId) milestones of
    Just m -> pure m
    Nothing -> throwError (InternalError $ "Milestone not found for waive-off: " <> milestoneId.getId)
  mbExisting <- QStats.findStatsByPersonAndMilestonePeriod personId journey.id milestoneId periodKey
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
      whenJust journey.maxWaiveOffCount $ \maxCount -> do
        periodStats <- QStats.findStatsByPersonJourneyAndPeriod personId journey.id periodKey
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
      void $
        CQStats.upsertJourneyStats
          ijHandle.actor
          DIJS.IncentiveJourneyStats
            { id = statsId,
              personId = personId,
              journeyId = journey.id,
              milestoneId = milestone.id,
              periodKey = periodKey,
              conditionType = milestone.conditionType,
              conditionOperator = milestone.conditionOperator,
              conditionValue = milestone.conditionValue,
              currentValue = maybe milestone.conditionValue (\s -> max s.currentValue milestone.conditionValue) mbExisting,
              status = DIJS.WaivedOff,
              rewardType = milestone.rewardType,
              rewardValue = Nothing,
              createdAt = maybe now (.createdAt) mbExisting,
              updatedAt = now,
              merchantId = Just merchantId,
              merchantOperatingCityId = Just merchantOpCityId
            }
      logInfo $
        "WaivedOff milestone "
          <> milestoneId.getId
          <> " periodKey="
          <> periodKey
          <> " person="
          <> personId.getId
      assignments <- Assignment.findAssignmentsByUserId personId
      let mbCjm =
            (.cohortJourneyMapping)
              <$> find
                (\a -> a.cohortJourneyMapping.journeyId == journey.id)
                assignments
      case mbCjm of
        Nothing ->
          logInfo $
            "streak-end skipped after waive; no user_cohort_mapping person="
              <> personId.getId
              <> " journey="
              <> journey.id.getId
        Just cjm
          | StreakEnd.hasStreakEndRewardConfigured cjm ->
            tryAwardStreakEnd ijHandle personId periodKey journey cjm milestones
          | otherwise ->
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
  currentPeriodStats <- QStats.findStatsByPersonJourneyAndPeriod personId journey.id periodKey
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
      scopedStats <- QStats.findStatsByPersonJourneyAndPeriodKeys personId journey.id neededPeriodKeys
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
  EvaluateInput ->
  DIJ.IncentiveJourney ->
  Text ->
  [DIJM.IncentiveJourneyMilestone] ->
  Eval.MilestoneStore m
mkMilestoneStore ijHandle input journey periodKey milestones =
  Eval.MilestoneStore
    { loadStats = \evalMilestone -> do
        case lookupMilestone evalMilestone of
          Nothing -> pure Nothing
          Just milestone -> do
            mbStats <-
              QStats.findStatsByPersonAndMilestonePeriod
                input.personId
                journey.id
                milestone.id
                periodKey
            pure $ fmap Convert.toEvalStats mbStats,
      persistProgress = \evalMilestone currentValue ->
        void $ upsertStats evalMilestone currentValue DIJS.InProgress Nothing,
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
            QStats.findStatsByPersonAndMilestonePeriod
              input.personId
              journey.id
              milestone.id
              periodKey
          now <- getCurrentTime
          statsId <- maybe generateGUID (pure . (.id)) mbExisting
          void $
            CQStats.upsertJourneyStats
              ijHandle.actor
              DIJS.IncentiveJourneyStats
                { id = statsId,
                  personId = input.personId,
                  journeyId = journey.id,
                  milestoneId = milestone.id,
                  periodKey = periodKey,
                  conditionType = milestone.conditionType,
                  conditionOperator = milestone.conditionOperator,
                  conditionValue = milestone.conditionValue,
                  currentValue = currentValue,
                  status = status,
                  rewardType = milestone.rewardType,
                  rewardValue = mbAwarded,
                  createdAt = maybe now (.createdAt) mbExisting,
                  updatedAt = now,
                  merchantId = Just input.merchantId,
                  merchantOperatingCityId = Just input.merchantOperatingCityId
                }
