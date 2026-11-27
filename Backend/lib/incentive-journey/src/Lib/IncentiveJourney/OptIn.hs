{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.OptIn
  ( isOptInActiveForLocalTime,
    isOptInPeriodActive,
    optInPeriodExclusiveEnd,
    hasStaleOptIn,
    resolveCampaignOptInPeriod,
    listOptInPeriod,
  )
where

import Data.Time (Day, UTCTime (UTCTime), addDays, diffDays, utctDay)
import Kernel.Prelude
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.UserCohortMapping as DUCM
import Lib.IncentiveJourney.Period (mkDailyPeriodKey, mkJourneyPeriodKeyWithStart)
import Lib.IncentiveJourney.Types (JourneyPeriodType (Weekly))

-- | Cadence for marking opt-in: CJM config wins, else request, else Daily.
resolveCampaignOptInPeriod :: DCJM.CohortJourneyMapping -> Maybe DCJM.OptInPeriod -> DCJM.OptInPeriod
resolveCampaignOptInPeriod cjm mbReq =
  case cjm.optInPeriod of
    Just period -> period
    Nothing -> fromMaybe DCJM.Daily mbReq

-- | What list/dashboard expose for OptIn campaigns. Assigned → Nothing.
listOptInPeriod :: DCJM.CohortJourneyMapping -> Maybe DCJM.OptInPeriod
listOptInPeriod cjm =
  case fromMaybe DCJM.Assigned cjm.campaignUserLinkType of
    DCJM.Assigned -> Nothing
    DCJM.OptIn -> Just (fromMaybe DCJM.Daily cjm.optInPeriod)

-- | Whether this UCM's OptIn is still live for the current city-local wall-clock.
-- Missing optInPeriod defaults to Daily (same as POST /optIn).
-- Weekly buckets match journey weeks: 7-day windows anchored at campaign startDate
-- (any weekday — not Monday-only). When startDate is missing, fall back to a rolling
-- 7-day window from optedInAt.
-- Callers must pass city-local now and optedInAt stored as local wall-clock.
isOptInActiveForLocalTime :: UTCTime -> Maybe UTCTime -> DUCM.UserCohortMapping -> Bool
isOptInActiveForLocalTime localNow mbCampaignStartDate ucm =
  case ucm.optedInAt of
    Nothing -> False
    Just optedInAt ->
      isOptInPeriodActive localNow (fromMaybe DCJM.Daily ucm.optInPeriod) mbCampaignStartDate optedInAt

isOptInPeriodActive :: UTCTime -> DCJM.OptInPeriod -> Maybe UTCTime -> UTCTime -> Bool
isOptInPeriodActive localNow period mbCampaignStartDate optedInAt =
  case period of
    DCJM.Daily -> mkDailyPeriodKey localNow == mkDailyPeriodKey optedInAt
    DCJM.Weekly ->
      case mbCampaignStartDate of
        Just startDate ->
          mkJourneyPeriodKeyWithStart localNow Weekly startDate
            == mkJourneyPeriodKeyWithStart optedInAt Weekly startDate
        Nothing ->
          -- Rolling week from opt-in day (any start weekday).
          let optedInDay = utctDay optedInAt
              localDay = utctDay localNow
           in localDay >= optedInDay && diffDays localDay optedInDay < 7

-- | Exclusive end of the opt-in period containing localNow (city-local wall-clock as UTCTime).
-- Daily → next local midnight.
-- Weekly → startDate-anchored week end (start + (idx+1)*7); without startDate → localDay+7.
optInPeriodExclusiveEnd :: UTCTime -> DCJM.OptInPeriod -> Maybe UTCTime -> UTCTime
optInPeriodExclusiveEnd localNow period mbCampaignStartDate =
  case period of
    DCJM.Daily -> UTCTime (addDays 1 (utctDay localNow)) 0
    DCJM.Weekly -> UTCTime (weeklyExclusiveEndDay localNow mbCampaignStartDate) 0

weeklyExclusiveEndDay :: UTCTime -> Maybe UTCTime -> Day
weeklyExclusiveEndDay localNow mbCampaignStartDate =
  case mbCampaignStartDate of
    Just startDate ->
      let startDay = utctDay startDate
          localDay = utctDay localNow
          idx = max 0 (diffDays localDay startDay) `div` 7
       in addDays ((idx + 1) * 7) startDay
    Nothing -> addDays 7 (utctDay localNow)

-- | Had an opt-in stamp but the period has rolled — list should clear to Pending.
hasStaleOptIn :: UTCTime -> Maybe UTCTime -> DUCM.UserCohortMapping -> Bool
hasStaleOptIn localNow mbCampaignStartDate ucm =
  isJust ucm.optedInAt && not (isOptInActiveForLocalTime localNow mbCampaignStartDate ucm)
