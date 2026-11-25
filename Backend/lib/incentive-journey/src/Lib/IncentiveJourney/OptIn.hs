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
  )
where

import Data.Time (Day, UTCTime (UTCTime), addDays, diffDays, utctDay)
import Kernel.Prelude
import qualified Lib.IncentiveJourney.Domain.Types.UserCohortMapping as DUCM
import Lib.IncentiveJourney.Period (mkDailyPeriodKey, mkJourneyPeriodKeyWithStart)
import Lib.IncentiveJourney.Types (JourneyPeriodType (Weekly))

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
      isOptInPeriodActive localNow (fromMaybe DUCM.Daily ucm.optInPeriod) mbCampaignStartDate optedInAt

isOptInPeriodActive :: UTCTime -> DUCM.OptInPeriod -> Maybe UTCTime -> UTCTime -> Bool
isOptInPeriodActive localNow period mbCampaignStartDate optedInAt =
  case period of
    DUCM.Daily -> mkDailyPeriodKey localNow == mkDailyPeriodKey optedInAt
    DUCM.Weekly ->
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
optInPeriodExclusiveEnd :: UTCTime -> DUCM.OptInPeriod -> Maybe UTCTime -> UTCTime
optInPeriodExclusiveEnd localNow period mbCampaignStartDate =
  case period of
    DUCM.Daily -> UTCTime (addDays 1 (utctDay localNow)) 0
    DUCM.Weekly -> UTCTime (weeklyExclusiveEndDay localNow mbCampaignStartDate) 0

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
