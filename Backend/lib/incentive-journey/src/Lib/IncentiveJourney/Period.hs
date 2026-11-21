{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.Period
  ( mkJourneyPeriodKeyWithStart,
    mkDailyPeriodKey,
  )
where

import qualified Data.Text as T
import Data.Time (Day, addDays, addGregorianMonthsClip, diffDays, utctDay)
import Kernel.Prelude
import Lib.IncentiveJourney.Types

-- | Period key for eval / streak / UI.
-- Daily: calendar day.
-- Weekly / Monthly: always anchored to campaign startDate (7-day / month-step buckets).
-- Format: Day:YYYY-MM-DD | Week:YYYY-MM-DD | Month:YYYY-MM-DD.
mkJourneyPeriodKeyWithStart :: UTCTime -> JourneyPeriodType -> UTCTime -> Text
mkJourneyPeriodKeyWithStart localTime periodType startDate =
  case periodType of
    Daily -> mkDailyPeriodKey localTime
    Weekly -> mkWeeklyPeriodKeyAnchored startDate localTime
    Monthly -> mkMonthlyPeriodKeyAnchored startDate localTime

-- | Calendar day only — peak windows are filtered at milestone eval, not in the key.
mkDailyPeriodKey :: UTCTime -> Text
mkDailyPeriodKey localTime =
  "Day:" <> T.pack (show (utctDay localTime))

-- | 7-day buckets from startDate. Format: Week:YYYY-MM-DD (anchor day).
mkWeeklyPeriodKeyAnchored :: UTCTime -> UTCTime -> Text
mkWeeklyPeriodKeyAnchored startDate localTime =
  let startDay = utctDay startDate
      localDay = utctDay localTime
      idx = max 0 (diffDays localDay startDay) `div` 7
      anchor = addDays (idx * 7) startDay
   in "Week:" <> T.pack (show anchor)

-- | Month-length buckets from startDate. Format: Month:YYYY-MM-DD (anchor day).
mkMonthlyPeriodKeyAnchored :: UTCTime -> UTCTime -> Text
mkMonthlyPeriodKeyAnchored startDate localTime =
  let startDay = utctDay startDate
      localDay = utctDay localTime
      idx = monthPeriodIndex startDay localDay
      anchor = addGregorianMonthsClip idx startDay
   in "Month:" <> T.pack (show anchor)

-- | Largest i >= 0 with addGregorianMonthsClip i startDay <= localDay.
monthPeriodIndex :: Day -> Day -> Integer
monthPeriodIndex startDay localDay
  | localDay < startDay = 0
  | otherwise = go 0
  where
    go i =
      let next = addGregorianMonthsClip (i + 1) startDay
       in if localDay < next then i else go (i + 1)
