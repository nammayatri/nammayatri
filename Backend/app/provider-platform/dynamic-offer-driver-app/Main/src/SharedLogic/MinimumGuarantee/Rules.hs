{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Pure rules of Minimum Guarantee (MG) campaigns: window arithmetic, slab lookup, metric
-- counters and the top-up formula. No IO; everything stateful lives in the sibling modules.
module SharedLogic.MinimumGuarantee.Rules
  ( MgWindowSpan (..),
    pageBounds,
    lastMaybe,
    localDayOf,
    lastDueDay,
    windowSpansOnDay,
    insideAnySpan,
    spansOverlap,
    overlapSeconds,
    slabPercent,
    metricValue,
    isLimitMetric,
    payoutPercentFor,
    limitRuleExhausted,
    closeOpenBreak,
    addCountedRide,
    topUpAmount,
  )
where

import Data.List (sortOn)
import Data.Time (Day, UTCTime (..), addDays, addUTCTime, diffUTCTime, timeOfDayToTime, timeToTimeOfDay, utctDay)
import qualified Domain.Types.Extra.MgEnrollment as DEME
import qualified Domain.Types.MgCampaignRule as DMR
import qualified Domain.Types.MgCampaignWindow as DMW
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney (..))

-- | Absolute start (inclusive) and end (exclusive) of one window on one day.
data MgWindowSpan = MgWindowSpan
  { spanStart :: UTCTime,
    spanEnd :: UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | The campaign's time-of-day windows applied to one local day, as absolute UTC spans, earliest
-- first. Windows are validated on save to lie within a single day and not to overlap.
windowSpansOnDay :: NominalDiffTime -> Day -> [DMW.MgCampaignWindow] -> [MgWindowSpan]
windowSpansOnDay timeDiffFromUtc day windows = sortOn (.spanStart) (map toSpan windows)
  where
    toSpan w = MgWindowSpan {spanStart = toUtc w.startTime, spanEnd = toUtc w.endTime}
    toUtc tod = addUTCTime (negate timeDiffFromUtc) (UTCTime day (timeOfDayToTime tod))

-- | The last element of a list in one pass, without reversing it.
lastMaybe :: [a] -> Maybe a
lastMaybe = foldl' (\_ x -> Just x) Nothing

-- | Limit and offset of a page, applied by the database queries: the limit defaults to 20 and is
-- capped at 50 (the other list endpoints use 10 to 50), the offset defaults to 0.
pageBounds :: Maybe Int -> Maybe Int -> (Int, Int)
pageBounds mbLimit mbOffset = (min 50 (max 1 (fromMaybe 20 mbLimit)), max 0 (fromMaybe 0 mbOffset))

-- | The city-local calendar day of an instant.
localDayOf :: NominalDiffTime -> UTCTime -> Day
localDayOf timeDiffFromUtc = utctDay . addUTCTime timeDiffFromUtc

-- | The latest local day whose settlement time (last window end + grace) has passed at `now`.
-- Every enrollment up to and including that day is ready to settle.
lastDueDay :: NominalDiffTime -> Int -> TimeOfDay -> UTCTime -> Day
lastDueDay timeDiffFromUtc graceMinutes lastWindowEnd now =
  let shifted = addUTCTime (timeDiffFromUtc - fromIntegral (graceMinutes * 60)) now
      day = utctDay shifted
   in if timeToTimeOfDay (utctDayTime shifted) >= lastWindowEnd then day else addDays (-1) day

insideAnySpan :: UTCTime -> [MgWindowSpan] -> Bool
insideAnySpan t = any (\s -> t >= s.spanStart && t < s.spanEnd)

-- | True when any span of one set overlaps any span of the other.
spansOverlap :: [MgWindowSpan] -> [MgWindowSpan] -> Bool
spansOverlap xs ys = or [x.spanStart < y.spanEnd && y.spanStart < x.spanEnd | x <- xs, y <- ys]

-- | Whole seconds of [from, to) that fall inside the spans. A partial second is not counted.
overlapSeconds :: UTCTime -> UTCTime -> [MgWindowSpan] -> Int
overlapSeconds from to spans = sum (map overlap spans)
  where
    overlap s =
      let lo = max from s.spanStart
          hi = min to s.spanEnd
       in if hi > lo then floor (diffUTCTime hi lo) else 0

-- | Payout percent of the slab the value falls in: the slab with the highest start at or below the
-- value, since each slab runs until the next one starts and the last is open-ended. Slab sets are
-- validated on save to start at 0, so a value below every slab (0% here) is only a safe default.
slabPercent :: [DMR.MgSlab] -> Int -> Int
slabPercent slabs v =
  maybe 0 (.percent) . listToMaybe . reverse . sortOn (.fromValue) $ filter (\s -> s.fromValue <= v) slabs

-- | The value of a metric for the day. Break time counts whole minutes only.
metricValue :: DMR.MgMetric -> DEME.MgMetrics -> Int
metricValue metric m = case metric of
  DMR.CANCELLATIONS -> m.cancellations
  DMR.BREAK_MINUTES -> m.breakSeconds `div` 60
  DMR.RIDES_COMPLETED -> m.rides
  DMR.DISTANCE_KM -> m.distanceMeters `div` 1000

-- | Limit metrics only grow and higher is worse; a limit rule at 0% can never recover during the
-- day. Target metrics (rides, distance) can still grow and are judged at settlement.
isLimitMetric :: DMR.MgMetric -> Bool
isLimitMetric metric = metric `elem` [DMR.CANCELLATIONS, DMR.BREAK_MINUTES]

-- | The day's payout percent: the lowest percent across all rules (100 when there are none).
payoutPercentFor :: [DMR.MgCampaignRule] -> DEME.MgMetrics -> Int
payoutPercentFor rules m = foldl' min 100 [slabPercent r.slabs (metricValue r.metric m) | r <- rules]

-- | True when some limit rule is already at 0%, so the day is lost.
limitRuleExhausted :: [DMR.MgCampaignRule] -> DEME.MgMetrics -> Bool
limitRuleExhausted rules m =
  any (\r -> isLimitMetric r.metric && slabPercent r.slabs (metricValue r.metric m) == 0) rules

-- | Adds the part of an open break that falls inside the windows and closes it. The break is
-- closed at `now`; pass the last window end to close a break that outlived the day.
closeOpenBreak :: UTCTime -> [MgWindowSpan] -> DEME.MgMetrics -> DEME.MgMetrics
closeOpenBreak now spans m = case m.breakStartedAt of
  Nothing -> m
  Just start ->
    m
      { DEME.breakSeconds = m.breakSeconds + overlapSeconds start now spans,
        DEME.breakStartedAt = Nothing
      }

-- | Counts one ride that started inside a window. Earned always grows; the ride count and the
-- distance only grow when the ride meets the params of the RIDES_COMPLETED / DISTANCE_KM rule
-- (minimum distance / fare), so very short rides cannot be used to reach a target. A ride id
-- already counted is a no-op, which makes a retried ride-ended event harmless.
addCountedRide :: [DMR.MgCampaignRule] -> Text -> HighPrecMoney -> Int -> DEME.MgMetrics -> DEME.MgMetrics
addCountedRide rules rideId fare distanceMeters m
  | rideId `elem` m.countedRideIds = m
  | otherwise =
    m
      { DEME.rides = m.rides + (if passes DMR.RIDES_COMPLETED then 1 else 0),
        DEME.distanceMeters = m.distanceMeters + (if passes DMR.DISTANCE_KM then distanceMeters else 0),
        DEME.earned = m.earned + fare,
        DEME.countedRideIds = take maxCountedRideIds (rideId : m.countedRideIds)
      }
  where
    passes metric = ridePasses (paramsOf metric)
    paramsOf metric = join (listToMaybe [r.params | r <- rules, r.metric == metric])
    ridePasses Nothing = True
    ridePasses (Just p) =
      maybe True (distanceMeters >=) p.minRideDistanceMeters
        && maybe True (fare >=) p.minRideFare

-- | A day has a handful of rides; the cap only bounds the row if something goes wrong.
maxCountedRideIds :: Int
maxCountedRideIds = 200

-- | max(0, guarantee - earned) x percent / 100, rounded to a whole currency unit.
topUpAmount :: HighPrecMoney -> HighPrecMoney -> Int -> HighPrecMoney
topUpAmount guarantee earned percent =
  let shortfall = max 0 (getHighPrecMoney guarantee - getHighPrecMoney earned)
      raw = shortfall * toRational percent / 100
   in HighPrecMoney (toRational (round raw :: Integer))
