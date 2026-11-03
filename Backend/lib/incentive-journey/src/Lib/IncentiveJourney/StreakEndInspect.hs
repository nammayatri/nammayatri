{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Dashboard / ops: diagnose (and optionally trigger) streak-end or periodDays-slice rewards.
-- Loads only the period keys the award path would check — no full history scan.
module Lib.IncentiveJourney.StreakEndInspect
  ( StreakEndMode (..),
    StreakEndVerdict (..),
    MilestonePeriodStatus (..),
    StreakEndInspectResult (..),
    inspectAndMaybeTriggerStreakEnd,
  )
where

import Control.Applicative ((<|>))
import Data.Time (Day, UTCTime (UTCTime), utctDay)
import Kernel.Prelude
import Kernel.Types.Common (Seconds)
import Kernel.Types.Id
import Kernel.Utils.Common (getLocalCurrentTime)
import qualified Lib.IncentiveJourney.Convert as Convert
import qualified Lib.IncentiveJourney.Deferred as Deferred
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats as DIJS
import qualified Lib.IncentiveJourney.Period as Period
import qualified Lib.IncentiveJourney.RewardMetadata as RM
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyStatsExtra as QStats
import qualified Lib.IncentiveJourney.Streak as Streak
import qualified Lib.IncentiveJourney.StreakEnd as StreakEnd
import qualified Lib.IncentiveJourney.Types as IJ

data StreakEndMode
  = ClassicFullStreak
  | PeriodizedSlice
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Why award should / should not run (ops-facing).
data StreakEndVerdict
  = ReadyToAward
  | NotConfigured
  | NotSliceEndDay
  | SliceIncomplete
  | SliceNotClosed
  | NotLastPeriodOfStreak
  | LastMilestoneIncomplete
  | PriorPeriodsIncomplete
  | NetOffPayoutZero
  | AwardDispatched
  | AwardSkippedByDispatcher
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data MilestonePeriodStatus = MilestonePeriodStatus
  { milestoneId :: Text,
    milestoneOrder :: Int,
    periodKey :: Text,
    status :: Maybe Text -- Nothing = missing row
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data StreakEndInspectResult = StreakEndInspectResult
  { mode :: StreakEndMode,
    campaignKey :: Text,
    periodKey :: Text,
    periodDays :: Maybe Int,
    sliceStart :: Maybe Day,
    sliceEndExclusive :: Maybe Day,
    periodKeysChecked :: [Text],
    streakEndRewardType :: Maybe Text,
    streakEndRewardValue :: Maybe Int,
    expectedPayout :: Maybe Int, -- NetOff computed; Coins/Cash = configured value when eligible
    milestonePeriodStatuses :: [MilestonePeriodStatus],
    eligible :: Bool,
    verdict :: StreakEndVerdict,
    triggered :: Bool,
    awardedAmount :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Inspect eligibility for streak-end / slice reward; optionally dispatch when eligible.
-- 'periodKey' = closed period used as the trigger day (slice last day, or last streak period).
inspectAndMaybeTriggerStreakEnd ::
  (BeamFlow m r) =>
  (DIJ.IncentiveJourney -> Text -> IJ.RewardSpec -> m IJ.AwardResult) ->
  Id Common.Person ->
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  [DIJM.IncentiveJourneyMilestone] ->
  Text -> -- periodKey
  Bool -> -- trigger
  Seconds -> -- timeDiffFromUtc (local clock; same as eval / period keys)
  m StreakEndInspectResult
inspectAndMaybeTriggerStreakEnd dispatchStreak personId journey cjm milestones periodKey trigger timeDiffFromUtc = do
  if not (StreakEnd.hasStreakEndRewardConfigured cjm)
    then
      pure $
        emptyResult ClassicFullStreak (StreakEnd.mkStreakCampaignKey journey cjm) NotConfigured
    else do
      let streakMeta = RM.parseStreakRewardMeta cjm.streakEndRewardMetadata
          lenDays = Deferred.campaignLengthDays journey cjm
      case streakMeta.periodDays of
        Just pd
          | pd > 0,
            lenDays > 0,
            lenDays `mod` pd == 0 ->
            inspectPeriodized dispatchStreak personId journey cjm milestones periodKey trigger pd lenDays streakMeta timeDiffFromUtc
        _ ->
          inspectClassic dispatchStreak personId journey cjm milestones periodKey trigger timeDiffFromUtc
  where
    emptyResult mode campaignKey verdict =
      StreakEndInspectResult
        { mode = mode,
          campaignKey = campaignKey,
          periodKey = periodKey,
          periodDays = Nothing,
          sliceStart = Nothing,
          sliceEndExclusive = Nothing,
          periodKeysChecked = [],
          streakEndRewardType = show <$> cjm.streakEndRewardType,
          streakEndRewardValue = cjm.streakEndRewardValue,
          expectedPayout = Nothing,
          milestonePeriodStatuses = [],
          eligible = False,
          verdict = verdict,
          triggered = False,
          awardedAmount = Nothing
        }

inspectPeriodized ::
  (BeamFlow m r) =>
  (DIJ.IncentiveJourney -> Text -> IJ.RewardSpec -> m IJ.AwardResult) ->
  Id Common.Person ->
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  [DIJM.IncentiveJourneyMilestone] ->
  Text ->
  Bool ->
  Int ->
  Int ->
  RM.StreakRewardMeta ->
  Seconds ->
  m StreakEndInspectResult
inspectPeriodized dispatchStreak personId journey cjm milestones periodKey trigger pd lenDays streakMeta timeDiffFromUtc = do
  let slices = Deferred.periodDaySlices cjm.startDate lenDays pd
      mbSlice =
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
          slices
  case mbSlice of
    Nothing ->
      pure
        StreakEndInspectResult
          { mode = PeriodizedSlice,
            campaignKey = StreakEnd.mkStreakCampaignKey journey cjm <> ":slice:?",
            periodKey = periodKey,
            periodDays = Just pd,
            sliceStart = Nothing,
            sliceEndExclusive = Nothing,
            periodKeysChecked = [],
            streakEndRewardType = show <$> cjm.streakEndRewardType,
            streakEndRewardValue = cjm.streakEndRewardValue,
            expectedPayout = Nothing,
            milestonePeriodStatuses = [],
            eligible = False,
            verdict = NotSliceEndDay,
            triggered = False,
            awardedAmount = Nothing
          }
    Just (_idx, sliceStart, sliceEnd) -> do
      let allKeys = Streak.listPeriodKeysInStreak journey.journeyType cjm.startDate cjm.streakRange
          periodKeys =
            filter
              ( \pk ->
                  let days = [sliceStart .. pred sliceEnd]
                      keys =
                        map
                          ( \d ->
                              Period.mkJourneyPeriodKeyWithStart
                                (UTCTime d 0)
                                (Convert.toLibPeriodType journey.journeyType)
                                cjm.startDate
                          )
                          days
                   in pk `elem` keys
              )
              allKeys
          campaignKey = StreakEnd.mkStreakSliceCampaignKey journey cjm sliceStart
      allStats <- QStats.findStatsByPersonCjmAndPeriodKeys personId cjm.id periodKeys
      localNow <- getLocalCurrentTime timeDiffFromUtc
      let statuses = mkStatuses milestones periodKeys allStats
          eligibleStats = StreakEnd.isSliceStreakEligible milestones periodKeys allStats
          (expectedPayout, baseSpec) =
            StreakEnd.streakEndDispatchSpecAndPayout
              cjm
              streakMeta
              [(row.periodKey, row.currentMetrics) | row <- allStats]
          -- Exclusive sliceEnd: closed when local calendar day >= sliceEnd.
          sliceClosed = utctDay localNow >= sliceEnd
          verdict0 =
            if not eligibleStats
              then SliceIncomplete
              else case cjm.streakEndRewardType of
                Just Common.NetOffEarnings | maybe True (<= 0) expectedPayout -> NetOffPayoutZero
                _
                  | not sliceClosed -> SliceNotClosed
                  | otherwise -> ReadyToAward
          eligible = eligibleStats && verdict0 == ReadyToAward
      finishInspect dispatchStreak journey campaignKey trigger eligible verdict0 expectedPayout baseSpec $
        StreakEndInspectResult
          { mode = PeriodizedSlice,
            campaignKey = campaignKey,
            periodKey = periodKey,
            periodDays = Just pd,
            sliceStart = Just sliceStart,
            sliceEndExclusive = Just sliceEnd,
            periodKeysChecked = periodKeys,
            streakEndRewardType = show <$> cjm.streakEndRewardType,
            streakEndRewardValue = cjm.streakEndRewardValue,
            expectedPayout = expectedPayout,
            milestonePeriodStatuses = statuses,
            eligible = eligible,
            verdict = verdict0,
            triggered = False,
            awardedAmount = Nothing
          }

inspectClassic ::
  (BeamFlow m r) =>
  (DIJ.IncentiveJourney -> Text -> IJ.RewardSpec -> m IJ.AwardResult) ->
  Id Common.Person ->
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  [DIJM.IncentiveJourneyMilestone] ->
  Text ->
  Bool ->
  Seconds ->
  m StreakEndInspectResult
inspectClassic dispatchStreak personId journey cjm milestones periodKey trigger timeDiffFromUtc = do
  let campaignKey = StreakEnd.mkStreakCampaignKey journey cjm
      periodKeys = StreakEnd.periodKeysForStreakEndEligibility journey cjm
      streakEndExclusive =
        Streak.computeStreakEndDate cjm.startDate cjm.streakRange journey.journeyType
  currentPeriodStats <- QStats.findStatsByPersonCjmAndPeriod personId cjm.id periodKey
  let attempt = StreakEnd.shouldAttemptStreakEnd journey cjm periodKey milestones currentPeriodStats
  if not attempt
    then do
      let lastOk = StreakEnd.isLastPeriodInStreak journey cjm periodKey
          lastMsOk = StreakEnd.isLastMilestoneSatisfied milestones periodKey currentPeriodStats
          verdict
            | not lastOk = NotLastPeriodOfStreak
            | not lastMsOk = LastMilestoneIncomplete
            | otherwise = PriorPeriodsIncomplete
          statuses = mkStatuses milestones [periodKey] currentPeriodStats
      pure
        StreakEndInspectResult
          { mode = ClassicFullStreak,
            campaignKey = campaignKey,
            periodKey = periodKey,
            periodDays = Nothing,
            sliceStart = Nothing,
            sliceEndExclusive = Just (utctDay streakEndExclusive),
            periodKeysChecked = [periodKey],
            streakEndRewardType = show <$> cjm.streakEndRewardType,
            streakEndRewardValue = cjm.streakEndRewardValue,
            expectedPayout = cjm.streakEndRewardValue,
            milestonePeriodStatuses = statuses,
            eligible = False,
            verdict = verdict,
            triggered = False,
            awardedAmount = Nothing
          }
    else do
      scopedStats <- QStats.findStatsByPersonCjmAndPeriodKeys personId cjm.id periodKeys
      localNow <- getLocalCurrentTime timeDiffFromUtc
      let streakMeta = RM.parseStreakRewardMeta cjm.streakEndRewardMetadata
          eligibleStats = StreakEnd.isStreakEndEligible journey cjm periodKey milestones scopedStats
          statuses = mkStatuses milestones periodKeys scopedStats
          (expectedPayout, rewardSpec) =
            StreakEnd.streakEndDispatchSpecAndPayout
              cjm
              streakMeta
              [(row.periodKey, row.currentMetrics) | row <- scopedStats]
          -- Exclusive streak end: closed when local calendar day >= end (same as periodized slices).
          streakClosed = utctDay localNow >= utctDay streakEndExclusive
          verdict0 =
            if not eligibleStats
              then PriorPeriodsIncomplete
              else case cjm.streakEndRewardType of
                Just Common.NetOffEarnings | maybe True (<= 0) expectedPayout -> NetOffPayoutZero
                _
                  | not streakClosed -> SliceNotClosed
                  | otherwise -> ReadyToAward
          eligible = eligibleStats && verdict0 == ReadyToAward
      finishInspect dispatchStreak journey campaignKey trigger eligible verdict0 expectedPayout rewardSpec $
        StreakEndInspectResult
          { mode = ClassicFullStreak,
            campaignKey = campaignKey,
            periodKey = periodKey,
            periodDays = Nothing,
            sliceStart = Nothing,
            sliceEndExclusive = Just (utctDay streakEndExclusive),
            periodKeysChecked = periodKeys,
            streakEndRewardType = show <$> cjm.streakEndRewardType,
            streakEndRewardValue = cjm.streakEndRewardValue,
            expectedPayout = expectedPayout,
            milestonePeriodStatuses = statuses,
            eligible = eligible,
            verdict = verdict0,
            triggered = False,
            awardedAmount = Nothing
          }

finishInspect ::
  (BeamFlow m r) =>
  (DIJ.IncentiveJourney -> Text -> IJ.RewardSpec -> m IJ.AwardResult) ->
  DIJ.IncentiveJourney ->
  Text ->
  Bool ->
  Bool ->
  StreakEndVerdict ->
  Maybe Int ->
  IJ.RewardSpec ->
  StreakEndInspectResult ->
  m StreakEndInspectResult
finishInspect dispatchStreak journey campaignKey trigger eligible verdict expectedPayout rewardSpec base =
  if trigger && eligible && verdict == ReadyToAward
    then do
      awardedResult <- dispatchStreak journey campaignKey rewardSpec
      case awardedResult of
        IJ.Awarded n ->
          pure
            base
              { triggered = True,
                awardedAmount = Just n,
                verdict = AwardDispatched,
                expectedPayout = expectedPayout <|> Just n
              }
        IJ.AwardSkipped ->
          pure
            base
              { triggered = True,
                awardedAmount = Nothing,
                verdict = AwardSkippedByDispatcher
              }
    else pure base

mkStatuses ::
  [DIJM.IncentiveJourneyMilestone] ->
  [Text] ->
  [DIJS.IncentiveJourneyStats] ->
  [MilestonePeriodStatus]
mkStatuses milestones periodKeys allStats =
  [ MilestonePeriodStatus
      { milestoneId = m.id.getId,
        milestoneOrder = m.order,
        periodKey = pk,
        status = show . (.status) <$> find (\s -> s.milestoneId == m.id && s.periodKey == pk) allStats
      }
    | m <- milestones,
      pk <- periodKeys
  ]
