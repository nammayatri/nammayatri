{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Deferred milestone detection + NetOff payout math from rewardMetadata.
module Lib.IncentiveJourney.Deferred
  ( isDeferredMilestone,
    hasBehaviorInCondition,
    settleLockKey,
    settleLockTTLSeconds,
    settleDoneKey,
    settleDoneTTLSeconds,
    statsRowLockKey,
    statsRowLockTTLSeconds,
    statsRowLockRetryDelayMicros,
    computeNetOffMilestonePayout,
    computeNetOffRowDelta,
    computeNetOffStreakSumDelta,
    earningsForNetOff,
    resolveMgAmount,
    resolvePayoutPercent,
    previousPeriodKeyInStreak,
    closedPeriodKeysForSettle,
    pendingClosedPeriodKeysAfterDone,
    periodDaySlices,
    campaignLengthDays,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (Day, addDays, diffDays, utctDay)
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import qualified Lib.IncentiveJourney.Convert as Convert
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Evaluation as Evaluation
import qualified Lib.IncentiveJourney.Period as Period
import qualified Lib.IncentiveJourney.RewardMetadata as RM
import qualified Lib.IncentiveJourney.Streak as Streak

-- | NetOff or any behaviour-influenced condition → settle after period closes.
isDeferredMilestone :: DIJM.IncentiveJourneyMilestone -> Bool
isDeferredMilestone m =
  m.rewardType == Common.NetOffEarnings || hasBehaviorInCondition m

hasBehaviorInCondition :: DIJM.IncentiveJourneyMilestone -> Bool
hasBehaviorInCondition m =
  case m.conditionType of
    DIJM.CancellationRate -> True
    DIJM.AcceptanceRate -> True
    DIJM.OnlineMinutes -> True
    -- JSONLogic: deferred only when logic references behaviour tokens (or always if you prefer).
    DIJM.JSONLogic -> evaluationMentionsBehavior m.evaluation
    _ -> evaluationMentionsBehavior m.evaluation

evaluationMentionsBehavior :: Maybe A.Value -> Bool
evaluationMentionsBehavior = \case
  Nothing -> False
  Just v ->
    let raw = T.toLower (TE.decodeUtf8 (BSL.toStrict (A.encode v)))
     in any (`T.isInfixOf` raw) behaviorTokens

behaviorTokens :: [Text]
behaviorTokens =
  [ "onlineminutes",
    "rrreceived",
    "rraccepted",
    "dxcancel",
    "dafcancel",
    "cancellationrate",
    "acceptancerate"
  ]

settleLockKey :: Text -> Text -> Text -> Text
settleLockKey personId cjmId periodKey =
  "ij:settle:" <> personId <> ":" <> cjmId <> ":" <> periodKey

-- | Short concurrency guard for overlapping list forks. Completed rows are skipped by
-- status; RewardPending retries after TTL. Cash/streak payouts are idempotent by referenceId.
settleLockTTLSeconds :: Int
settleLockTTLSeconds = 5 * 60

-- | One key per person+CJM (value = settled periodKey). Overwritten when the next
-- period settles — no per-day key pile-up. TTL only needs to cover “this key is still
-- the previous period” (+ buffer); after period rolls the value no longer matches.
settleDoneKey :: Text -> Text -> Text
settleDoneKey personId cjmId =
  "ij:settle:done:" <> personId <> ":" <> cjmId

-- | Hygiene TTL while this assignment is live. Sized to journey period so monthly
-- campaigns keep the short-circuit for the whole month the key stays “previous”.
settleDoneTTLSeconds :: DIJ.IncentiveJourneyType -> Int
settleDoneTTLSeconds = \case
  DIJ.Daily -> 3 * 24 * 3600
  DIJ.Weekly -> 14 * 24 * 3600
  DIJ.Monthly -> 40 * 24 * 3600

-- | Serialize read-modify-write of incentive_journey_stats.currentMetrics for one row
-- (behaviour tick vs ride-end earnings vs settle/eval upsert).
statsRowLockKey :: Text -> Text -> Text -> Text -> Text
statsRowLockKey personId cjmId milestoneId periodKey =
  "ij:stats:rmw:" <> personId <> ":" <> cjmId <> ":" <> milestoneId <> ":" <> periodKey

statsRowLockTTLSeconds :: Int
statsRowLockTTLSeconds = 5

-- | Micros between lock retries (withWaitAndLockRedis).
statsRowLockRetryDelayMicros :: Int
statsRowLockRetryDelayMicros = 2000

earningsForNetOff :: Bool -> Maybe A.Value -> HighPrecMoney
earningsForNetOff includeAllDay mbMetrics =
  let inside = fromMaybe 0 (Evaluation.lookupNumber mbMetrics Evaluation.earningInsideKey)
      outside = fromMaybe 0 (Evaluation.lookupNumber mbMetrics Evaluation.earningOutsideKey)
   in if includeAllDay then inside + outside else inside

resolveMgAmount :: RM.MilestoneRewardMeta -> Maybe Int -> Maybe A.Value -> Int
resolveMgAmount meta mbRewardValue mbMetrics =
  if not (null meta.mgAmountSlabs)
    then
      let onlineMins = Evaluation.lookupMetricInt mbMetrics mempty "onlineMinutes"
       in RM.resolveAmountSlab meta.mgAmountSlabs onlineMins
    else fromMaybe 0 mbRewardValue

-- | % from rewardMetadata slabs only (evaluation column is condition JsonLogic, not slabs).
-- Configured slabs (including explicit 0%) win; no percent/violation slabs → 100% (same as streak settle).
resolvePayoutPercent :: RM.MilestoneRewardMeta -> Maybe A.Value -> Int
resolvePayoutPercent meta mbMetrics =
  fromMaybe 100 $
    RM.resolveMinPercent mbMetrics meta.percentSlabs meta.violationPercentSlabs meta.violationMetrics

computeNetOffMilestonePayout ::
  Maybe A.Value -> -- rewardMetadata
  Maybe Int -> -- rewardValue fallback MG
  Maybe A.Value -> -- currentMetrics
  Int
computeNetOffMilestonePayout mbRewardMeta mbRewardValue mbMetrics =
  let meta = RM.parseMilestoneRewardMeta mbRewardMeta
      mg = resolveMgAmount meta mbRewardValue mbMetrics
      earnings = earningsForNetOff meta.includeEarningsAllDay mbMetrics
      pct = resolvePayoutPercent meta mbMetrics
   in Evaluation.netOffPayout mg earnings pct

-- | Per-row streak delta (may be negative).
computeNetOffRowDelta ::
  RM.StreakRewardMeta ->
  Maybe Int ->
  Maybe A.Value ->
  Int
computeNetOffRowDelta streakMeta mbRewardValue mbMetrics =
  let asMilestone =
        RM.MilestoneRewardMeta
          { includeEarningsAllDay = streakMeta.includeEarningsAllDay,
            mgAmountSlabs = streakMeta.mgAmountSlabs,
            percentSlabs = streakMeta.percentSlabs,
            violationPercentSlabs = streakMeta.violationPercentSlabs,
            violationMetrics = streakMeta.violationMetrics
          }
      mg = resolveMgAmount asMilestone mbRewardValue mbMetrics
      earnings = earningsForNetOff streakMeta.includeEarningsAllDay mbMetrics
      -- Row delta before % (streak applies % on the sum).
      earn = round earnings :: Int
   in mg - earn

-- | Periodized NetOff streak: one (MG − earnings) delta per periodKey, then sum.
-- Stats are (periodKey, currentMetrics) — often one row per milestone per day.
-- Rows in the same period are max-merged (not summed) so multiple milestones cannot
-- inflate MG, and a legacy null-metrics sibling cannot add a second full guarantee.
-- Returns (sumDelta, mergedMetricsForPercentSlabs).
computeNetOffStreakSumDelta ::
  RM.StreakRewardMeta ->
  Maybe Int ->
  [(Text, Maybe A.Value)] ->
  (Int, Maybe A.Value)
computeNetOffStreakSumDelta streakMeta mbRewardValue periodMetricRows =
  let periodBags =
        Map.elems $
          foldl'
            ( \acc (periodKey, mbMetrics) ->
                Map.insertWith Evaluation.maxMergeCurrentMetrics periodKey mbMetrics acc
            )
            Map.empty
            periodMetricRows
      deltas = map (computeNetOffRowDelta streakMeta mbRewardValue) periodBags
      forPct = foldl' Evaluation.maxMergeCurrentMetrics Nothing periodBags
   in (sum deltas, forPct)

-- | All streak period keys that are already closed at localTime (chronological).
-- Inside the window: every key before the current period.
-- After campaign end: every key in the streak (final day included).
closedPeriodKeysForSettle ::
  UTCTime ->
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  [Text]
closedPeriodKeysForSettle localTime journey cjm =
  let keys = Streak.listPeriodKeysInStreak journey.journeyType cjm.startDate cjm.streakRange
      current =
        Period.mkJourneyPeriodKeyWithStart
          localTime
          (Convert.toLibPeriodType journey.journeyType)
          cjm.startDate
      endExclusive = Streak.computeStreakEndDate cjm.startDate cjm.streakRange journey.journeyType
   in case break (== current) keys of
        (prior, _ : _) -> prior
        _
          | localTime >= endExclusive -> keys
          | otherwise -> []

previousPeriodKeyInStreak ::
  UTCTime ->
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  Maybe Text
previousPeriodKeyInStreak localTime journey cjm =
  listToMaybe (reverse (closedPeriodKeysForSettle localTime journey cjm))

-- | Closed keys still needing settle, after the Redis done-watermark (exclusive).
-- Empty ⇒ hot path: no DB / no locks.
-- Writers must only advance the watermark to the last *contiguous* quiet period
-- (see settleClosedPeriodIfNeeded); jumping past RewardPending excludes retries.
pendingClosedPeriodKeysAfterDone ::
  UTCTime ->
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  Maybe Text -> -- settle done-key value
  [Text]
pendingClosedPeriodKeysAfterDone localTime journey cjm mbDone =
  let closedKeys = closedPeriodKeysForSettle localTime journey cjm
   in case mbDone of
        Just done
          | done `elem` closedKeys ->
            drop 1 (dropWhile (/= done) closedKeys)
        _ -> closedKeys

-- | Non-overlapping day windows of length periodDays covering [start, start+streakDays).
-- Each slice: (sliceIndex, startDayInclusive, endDayExclusive).
periodDaySlices :: UTCTime -> Int -> Int -> [(Int, Day, Day)]
periodDaySlices startDate streakDays periodDays
  | periodDays <= 0 || streakDays <= 0 = []
  | streakDays `mod` periodDays /= 0 = []
  | otherwise =
    let startDay = utctDay startDate
        n = streakDays `div` periodDays
     in [ (i, addDays (fromIntegral (i * periodDays)) startDay, addDays (fromIntegral ((i + 1) * periodDays)) startDay)
          | i <- [0 .. n - 1]
        ]

-- | Calendar days in campaign window [start, exclusiveEnd).
campaignLengthDays :: DIJ.IncentiveJourney -> DCJM.CohortJourneyMapping -> Int
campaignLengthDays journey cjm =
  let endExclusive = Streak.computeStreakEndDate cjm.startDate cjm.streakRange journey.journeyType
   in fromIntegral (diffDays (utctDay endExclusive) (utctDay cjm.startDate))
