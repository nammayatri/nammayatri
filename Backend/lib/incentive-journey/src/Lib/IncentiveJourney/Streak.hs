{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.Streak
  ( computeStreakEndDate,
    listPeriodKeysInStreak,
    validateMappingStartDate,
  )
where

import Data.Time (UTCTime (UTCTime), addDays, addGregorianMonthsClip, toGregorian, utctDay, utctDayTime)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Kernel.Prelude
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Period as Period

-- | Exclusive end of the streak window: startDate + streakRange units.
-- Active while startDate <= localTime < endDate.
-- Example monthly start Jan 1, streakRange=2 -> endDate = Mar 1 (window through last Feb day).
computeStreakEndDate :: UTCTime -> Int -> DIJ.IncentiveJourneyType -> UTCTime
computeStreakEndDate startDate streakRange journeyType =
  let startDay = utctDay startDate
      endDay =
        case journeyType of
          DIJ.Daily -> addDays (fromIntegral streakRange) startDay
          DIJ.Weekly -> addDays (fromIntegral (streakRange * 7)) startDay
          DIJ.Monthly -> addGregorianMonthsClip (fromIntegral streakRange) startDay
   in UTCTime endDay (utctDayTime startDate)

-- | Exactly streakRange period keys, stepping one period at a time from startDate.
-- Daily streakRange=2  -> [Day:start, Day:start+1]
-- Weekly streakRange=2 -> [Week:start, Week:start+7d]
-- Monthly streakRange=2 -> [Month:start, Month:start+1m]
listPeriodKeysInStreak :: DIJ.IncentiveJourneyType -> UTCTime -> Int -> [Text]
listPeriodKeysInStreak journeyType startDate streakRange
  | streakRange <= 0 = []
  | otherwise =
    let startDay = utctDay startDate
        offsets = [0 .. streakRange - 1]
     in case journeyType of
          DIJ.Daily ->
            map (\i -> Period.mkDailyPeriodKey (UTCTime (addDays (fromIntegral i) startDay) 0)) offsets
          DIJ.Weekly ->
            map (\i -> Period.mkWeeklyPeriodKey (UTCTime (addDays (fromIntegral (i * 7)) startDay) 0)) offsets
          DIJ.Monthly ->
            map (\i -> Period.mkMonthlyPeriodKey (UTCTime (addGregorianMonthsClip (fromIntegral i) startDay) 0)) offsets

-- | Weekly start must be Monday; monthly start must be the 1st. Daily has no restriction.
validateMappingStartDate :: DIJ.IncentiveJourneyType -> UTCTime -> Either Text ()
validateMappingStartDate journeyType startDate =
  let day = utctDay startDate
   in case journeyType of
        DIJ.Daily -> Right ()
        DIJ.Weekly ->
          let (_, _, dayOfWeek) = toWeekDate day
           in if dayOfWeek == 1
                then Right ()
                else Left "Weekly journey startDate must be a Monday"
        DIJ.Monthly ->
          let (_, _, dayOfMonth) = toGregorian day
           in if dayOfMonth == 1
                then Right ()
                else Left "Monthly journey startDate must be the 1st of the month"
