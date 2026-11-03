{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.Period
  ( mkJourneyPeriodKeyWithStart,
    mkDailyPeriodKey,
    periodExclusiveEndFromKey,
    periodExclusiveEndFromKeyForCampaign,
  )
where

import qualified Data.Text as T
import Data.Time (Day, UTCTime (UTCTime), addDays, addGregorianMonthsClip, diffDays, fromGregorian, utctDay)
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

-- | Exclusive end when campaign startDate is known (matches settlement month buckets).
-- Month periods are steps of addGregorianMonthsClip from startDate — not anchor+1month.
-- Example: start Jan 31 → Month:2026-02-28 ends at Mar 31 (not Mar 28).
periodExclusiveEndFromKeyForCampaign :: UTCTime -> Text -> Maybe UTCTime
periodExclusiveEndFromKeyForCampaign startDate key =
  case T.splitOn ":" key of
    ["Day", dayText] -> dayEndExclusive dayText (\d -> addDays 1 d)
    ["Week", dayText] -> dayEndExclusive dayText (\d -> addDays 7 d)
    ["Month", dayText] -> do
      anchor <- parseIsoDay dayText
      idx <- monthIndexForAnchor (utctDay startDate) anchor
      pure $ UTCTime (addGregorianMonthsClip (idx + 1) (utctDay startDate)) 0
    _ -> Nothing

-- | Fallback when startDate is unavailable. Day/Week are exact; Month uses anchor+1 clip
-- which disagrees with settlement for 29–31 start days after the first period — prefer
-- 'periodExclusiveEndFromKeyForCampaign' whenever CJM.startDate is known.
periodExclusiveEndFromKey :: Text -> Maybe UTCTime
periodExclusiveEndFromKey key =
  case T.splitOn ":" key of
    ["Day", dayText] -> dayEndExclusive dayText (\d -> addDays 1 d)
    ["Week", dayText] -> dayEndExclusive dayText (\d -> addDays 7 d)
    ["Month", dayText] -> dayEndExclusive dayText (\d -> addGregorianMonthsClip 1 d)
    _ -> Nothing

-- | idx where addGregorianMonthsClip idx startDay == anchor (same as key construction).
monthIndexForAnchor :: Day -> Day -> Maybe Integer
monthIndexForAnchor startDay anchor
  | anchor < startDay = Nothing
  | otherwise = go 0
  where
    go i
      | i > 2400 = Nothing
      | otherwise =
        let a = addGregorianMonthsClip i startDay
         in if a == anchor
              then Just i
              else
                if a > anchor
                  then Nothing
                  else go (i + 1)

dayEndExclusive :: Text -> (Day -> Day) -> Maybe UTCTime
dayEndExclusive dayText nextDay = do
  day <- parseIsoDay dayText
  pure $ UTCTime (nextDay day) 0

parseIsoDay :: Text -> Maybe Day
parseIsoDay t =
  case T.splitOn "-" t of
    [y, m, d] -> do
      year <- readMaybe (T.unpack y)
      month <- readMaybe (T.unpack m)
      dom <- readMaybe (T.unpack d)
      pure $ fromGregorian year month dom
    _ -> Nothing
