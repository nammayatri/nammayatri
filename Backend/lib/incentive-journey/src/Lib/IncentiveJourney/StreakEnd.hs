{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.StreakEnd
  ( mkStreakCampaignKey,
    periodKeysForStreakEndEligibility,
    isLastPeriodInStreak,
    isLastMilestoneSatisfied,
    shouldAttemptStreakEnd,
    isStreakEndEligible,
    streakEndRewardSpec,
    hasStreakEndRewardConfigured,
  )
where

import Data.Ord (comparing)
import Data.Time (utctDay)
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.IncentiveJourney.Convert as Convert
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats as DIJS
import qualified Lib.IncentiveJourney.Streak as Streak
import qualified Lib.IncentiveJourney.Types as IJ

-- | Streak-end is configured when cohort_journey_mapping has a streakEndRewardType.
hasStreakEndRewardConfigured :: DCJM.CohortJourneyMapping -> Bool
hasStreakEndRewardConfigured cjm = isJust cjm.streakEndRewardType

journeyTypeOrDaily :: DIJ.IncentiveJourney -> DIJ.IncentiveJourneyType
journeyTypeOrDaily journey = fromMaybe DIJ.Daily journey.journeyType

streakEndDateFor :: DIJ.IncentiveJourney -> DCJM.CohortJourneyMapping -> UTCTime
streakEndDateFor journey cjm =
  Streak.computeStreakEndDate cjm.startDate cjm.streakRange (journeyTypeOrDaily journey)

mkStreakCampaignKey :: DIJ.IncentiveJourney -> DCJM.CohortJourneyMapping -> Text
mkStreakCampaignKey journey cjm =
  let endDate = streakEndDateFor journey cjm
   in "campaign:"
        <> show (utctDay cjm.startDate)
        <> ":"
        <> show (utctDay endDate)
        <> ":"
        <> cjm.id.getId

periodKeysForStreakEndEligibility :: DIJ.IncentiveJourney -> DCJM.CohortJourneyMapping -> [Text]
periodKeysForStreakEndEligibility journey cjm
  | not (hasStreakEndRewardConfigured cjm) = []
  | otherwise =
    Streak.listPeriodKeysInStreak (journeyTypeOrDaily journey) cjm.startDate cjm.streakRange

-- | True when currentPeriodKey is the Nth (last) period in the streak window.
isLastPeriodInStreak :: DIJ.IncentiveJourney -> DCJM.CohortJourneyMapping -> Text -> Bool
isLastPeriodInStreak journey cjm currentPeriodKey =
  case reverse (periodKeysForStreakEndEligibility journey cjm) of
    (lastKey : _) -> lastKey == currentPeriodKey
    [] -> False

satisfiedStatus :: DIJS.JourneyMilestoneStatus -> Bool
satisfiedStatus DIJS.Completed = True
satisfiedStatus DIJS.Rewarded = True
satisfiedStatus DIJS.WaivedOff = True
satisfiedStatus _ = False

hasSatisfied ::
  Id DIJM.IncentiveJourneyMilestone ->
  Text ->
  [DIJS.IncentiveJourneyStats] ->
  Bool
hasSatisfied milestoneId periodKey allStats =
  any
    ( \s ->
        s.milestoneId == milestoneId
          && s.periodKey == periodKey
          && satisfiedStatus s.status
    )
    allStats

-- | Highest-order milestone is Completed / Rewarded / WaivedOff for the given period.
isLastMilestoneSatisfied ::
  [DIJM.IncentiveJourneyMilestone] ->
  Text ->
  [DIJS.IncentiveJourneyStats] ->
  Bool
isLastMilestoneSatisfied milestones periodKey allStats =
  case milestones of
    [] -> False
    _ ->
      let lastMilestone = maximumBy (comparing (.order)) milestones
       in hasSatisfied lastMilestone.id periodKey allStats

-- | Cheap gate before loading multi-period stats (ride-end path):
-- last period + last milestone done. Waive of any in-between milestone still
-- reaches full eligibility once every period key's milestones are satisfied.
shouldAttemptStreakEnd ::
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  Text ->
  [DIJM.IncentiveJourneyMilestone] ->
  [DIJS.IncentiveJourneyStats] ->
  Bool
shouldAttemptStreakEnd journey cjm currentPeriodKey milestones currentPeriodStats
  | not (hasStreakEndRewardConfigured cjm) = False
  | not (isLastPeriodInStreak journey cjm currentPeriodKey) = False
  | otherwise = isLastMilestoneSatisfied milestones currentPeriodKey currentPeriodStats

-- | Full eligibility: every milestone satisfied for every period key in the streak.
isStreakEndEligible ::
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  Text ->
  [DIJM.IncentiveJourneyMilestone] ->
  [DIJS.IncentiveJourneyStats] ->
  Bool
isStreakEndEligible journey cjm currentPeriodKey milestones allStats
  | not (shouldAttemptStreakEnd journey cjm currentPeriodKey milestones allStats) = False
  | otherwise =
    let periodKeys = periodKeysForStreakEndEligibility journey cjm
     in not (null milestones)
          && not (null periodKeys)
          && all
            ( \periodKey ->
                all (\m -> hasSatisfied m.id periodKey allStats) milestones
            )
            periodKeys

streakEndRewardSpec :: DCJM.CohortJourneyMapping -> IJ.RewardSpec
streakEndRewardSpec cjm =
  IJ.RewardSpec
    { rewardKind = maybe IJ.Coins Convert.toLibRewardKind cjm.streakEndRewardType,
      rewardValue = cjm.streakEndRewardValue,
      rewardExpirationAt = cjm.streakEndRewardExpirationAt
    }
