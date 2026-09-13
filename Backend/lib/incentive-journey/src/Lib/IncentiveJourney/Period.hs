{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.Period
  ( mkJourneyPeriodKey,
    mkDailyPeriodKey,
    mkWeeklyPeriodKey,
    mkMonthlyPeriodKey,
  )
where

import qualified Data.Text as T
import Data.Time (utctDay)
import Data.Time.Calendar (toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Kernel.Prelude
import Lib.IncentiveJourney.Types

mkJourneyPeriodKey :: UTCTime -> JourneyPeriodType -> Text
mkJourneyPeriodKey localTime periodType =
  case periodType of
    Daily -> mkDailyPeriodKey localTime
    Weekly -> mkWeeklyPeriodKey localTime
    Monthly -> mkMonthlyPeriodKey localTime

-- | Calendar day only — peak windows are filtered at milestone eval, not in the key.
mkDailyPeriodKey :: UTCTime -> Text
mkDailyPeriodKey localTime =
  "Day:" <> T.pack (show (utctDay localTime))

mkWeeklyPeriodKey :: UTCTime -> Text
mkWeeklyPeriodKey localTime =
  let (year, week, _) = toWeekDate (utctDay localTime)
   in "Week:" <> T.pack (show year) <> "-W" <> T.pack (pad2 week)

-- | Calendar month in the already-localized timestamp.
mkMonthlyPeriodKey :: UTCTime -> Text
mkMonthlyPeriodKey localTime =
  let (year, month, _) = toGregorian (utctDay localTime)
   in "Month:" <> T.pack (show year) <> "-" <> T.pack (pad2 month)

pad2 :: Int -> String
pad2 n
  | n < 10 = '0' : show n
  | otherwise = show n
