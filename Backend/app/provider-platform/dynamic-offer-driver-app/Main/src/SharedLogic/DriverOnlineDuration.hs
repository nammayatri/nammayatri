module SharedLogic.DriverOnlineDuration
  ( OnlineDurationResult (..),
    foldOnlineIntervals,
  )
where

import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Time (Day, UTCTime (..), addDays, addUTCTime, diffUTCTime)
import Kernel.Prelude
import Kernel.Utils.Common (Seconds (..), secondsToNominalDiffTime)

-- | Per-day online totals for one driver, reconstructed from the state changelog.
data OnlineDurationResult = OnlineDurationResult
  { -- | One entry per day in the requested range, including days with no activity.
    perDay :: [(Day, Seconds)],
    -- | False when the state at the start of the window could not be determined
    -- because no changelog row exists before it. The totals are then a lower bound.
    dataComplete :: Bool
  }
  deriving (Show, Eq)

-- | Reconstruct online intervals from a changelog and total them per merchant-local day.
--
-- The caller maps its domain enum to 'Bool' before calling, so only the arithmetic lives
-- here. 'Nothing' for the initial state means no row exists before the window: the state
-- the driver entered the window in is unknown, so it is assumed offline and
-- 'dataComplete' is False.
--
-- Rows that repeat the current state are ignored. This matters because the drainer emits
-- a row for any column change, not only for mode changes, so most rows are repeats.
foldOnlineIntervals ::
  -- | merchant offset from UTC
  Seconds ->
  -- | first merchant-local day, inclusive
  Day ->
  -- | last merchant-local day, inclusive
  Day ->
  -- | now, so a still-open interval is never clamped to a future time
  UTCTime ->
  -- | online at window start; Nothing when no earlier row exists
  Maybe Bool ->
  -- | (online, changedAt) pairs within the window
  [(Bool, UTCTime)] ->
  OnlineDurationResult
foldOnlineIntervals timeDiffFromUtc fromDay toDay now mbInitialOnline changes =
  OnlineDurationResult
    { perDay = map (\d -> (d, M.findWithDefault (Seconds 0) d totals)) allDays,
      dataComplete = isJust mbInitialOnline
    }
  where
    offset = secondsToNominalDiffTime timeDiffFromUtc
    localToUtc = addUTCTime (negate offset)
    utcToLocal = addUTCTime offset

    windowStart = localToUtc (UTCTime fromDay 0)
    windowEnd = min (localToUtc (UTCTime (addDays 1 toDay) 0)) now

    allDays = [fromDay .. toDay]
    initialOnline = fromMaybe False mbInitialOnline

    inWindow (_, t) = t >= windowStart && t < windowEnd
    transitions = dropRepeats initialOnline (sortOn snd (filter inWindow changes))

    intervals = collect initialOnline windowStart transitions

    -- After dropRepeats the states strictly alternate, so the flag on each remaining
    -- change is implied by the state we are currently in.
    collect True openedAt [] = [(openedAt, windowEnd)]
    collect False _ [] = []
    collect True openedAt ((_, t) : rest) = (openedAt, t) : collect False t rest
    collect False _ ((_, t) : rest) = collect True t rest

    totals = M.fromListWith (+) (concatMap (splitByLocalDay utcToLocal) intervals)

dropRepeats :: Bool -> [(Bool, UTCTime)] -> [(Bool, UTCTime)]
dropRepeats _ [] = []
dropRepeats current ((online, changedAt) : rest)
  | online == current = dropRepeats current rest
  | otherwise = (online, changedAt) : dropRepeats online rest

-- | Split one UTC interval at merchant-local midnights, attributing each part to its
-- local day. A session from 23:00 to 01:00 credits both days.
splitByLocalDay :: (UTCTime -> UTCTime) -> (UTCTime, UTCTime) -> [(Day, Seconds)]
splitByLocalDay utcToLocal (startUtc, endUtc) = go (utcToLocal startUtc) (utcToLocal endUtc)
  where
    go start end
      | start >= end = []
      | utctDay start == utctDay end = [(utctDay start, diffSeconds end start)]
      | otherwise =
        let nextMidnight = UTCTime (addDays 1 (utctDay start)) 0
         in (utctDay start, diffSeconds nextMidnight start) : go nextMidnight end

-- | Each endpoint is floored to a whole second before subtracting, so repeated splitting
-- at midnight cannot accumulate sub-second rounding error. Mirrors diffUTCTimeInSeconds
-- in Domain/Action/Internal/ProcessingChangeOnline.hs.
diffSeconds :: UTCTime -> UTCTime -> Seconds
diffSeconds to' from' = Seconds (round (diffUTCTime (floorToSecond to') (floorToSecond from')))

floorToSecond :: UTCTime -> UTCTime
floorToSecond (UTCTime day dayTime) = UTCTime day (fromIntegral (floor dayTime :: Integer))
