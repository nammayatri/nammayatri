{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.Streak
  ( computeStreakEndDate,
    displayStreakEndDate,
    listPeriodKeysInStreak,
  )
where

import Data.Time (UTCTime (UTCTime), addDays, addGregorianMonthsClip, utctDay, utctDayTime)
import Kernel.Prelude
import qualified Lib.IncentiveJourney.Convert as Convert
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Period as Period

-- | Exclusive end of the streak window: startDate + streakRange units.
-- Active while startDate <= localTime < endDate.
-- Example weekly start Wed Oct 22, streakRange=2 -> exclusive end Wed Nov 5
-- (last eligible calendar day is Tue Nov 4 — use 'displayStreakEndDate' for UI).
computeStreakEndDate :: UTCTime -> Int -> DIJ.IncentiveJourneyType -> UTCTime
computeStreakEndDate startDate streakRange journeyType =
  let startDay = utctDay startDate
      endDay =
        case journeyType of
          DIJ.Daily -> addDays (fromIntegral streakRange) startDay
          DIJ.Weekly -> addDays (fromIntegral (streakRange * 7)) startDay
          DIJ.Monthly -> addGregorianMonthsClip (fromIntegral streakRange) startDay
   in UTCTime endDay (utctDayTime startDate)

-- | Inclusive last day/time for dashboard + app UI (one calendar day before exclusive end).
-- Backend window checks must keep using 'computeStreakEndDate'.
displayStreakEndDate :: UTCTime -> Int -> DIJ.IncentiveJourneyType -> UTCTime
displayStreakEndDate startDate streakRange journeyType =
  let exclusive = computeStreakEndDate startDate streakRange journeyType
   in UTCTime (addDays (-1) (utctDay exclusive)) (utctDayTime exclusive)

-- | Exactly streakRange period keys, stepping one period at a time from startDate.
-- Weekly/monthly always use startDate-anchored keys (Monday/1st and mid starts alike).
listPeriodKeysInStreak :: DIJ.IncentiveJourneyType -> UTCTime -> Int -> [Text]
listPeriodKeysInStreak journeyType startDate streakRange
  | streakRange <= 0 = []
  | otherwise =
    let startDay = utctDay startDate
        offsets = [0 .. streakRange - 1]
        periodType = Convert.toLibPeriodType journeyType
     in case journeyType of
          DIJ.Daily ->
            map (\i -> Period.mkDailyPeriodKey (UTCTime (addDays (fromIntegral i) startDay) 0)) offsets
          DIJ.Weekly ->
            map
              ( \i ->
                  let bucketStart = UTCTime (addDays (fromIntegral (i * 7)) startDay) 0
                   in Period.mkJourneyPeriodKeyWithStart bucketStart periodType startDate
              )
              offsets
          DIJ.Monthly ->
            map
              ( \i ->
                  let bucketStart = UTCTime (addGregorianMonthsClip (fromIntegral i) startDay) 0
                   in Period.mkJourneyPeriodKeyWithStart bucketStart periodType startDate
              )
              offsets
