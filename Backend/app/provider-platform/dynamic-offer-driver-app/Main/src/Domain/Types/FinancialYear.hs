module Domain.Types.FinancialYear where

import Data.Aeson
import Data.Time.Calendar
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

-- | An FY-relative reporting period.
--
-- Every member maps to a contiguous run of months measured from the start of
-- the financial year, so quarters, halves and the full year share one
-- implementation. Adding a monthly or custom period is a change to
-- 'fyPeriodMonths' alone.
data FyPeriod
  = FullYear
  | H1
  | H2
  | Q1
  | Q2
  | Q3
  | Q4
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''FyPeriod)

-- | Half-open @[from, to)@ month offsets from the start of the financial year.
fyPeriodMonths :: FyPeriod -> (Integer, Integer)
fyPeriodMonths = \case
  FullYear -> (0, 12)
  H1 -> (0, 6)
  H2 -> (6, 12)
  Q1 -> (0, 3)
  Q2 -> (3, 6)
  Q3 -> (6, 9)
  Q4 -> (9, 12)

-- | Inclusive date range for a period within a financial year.
--
-- @fyStartMonth@ is the calendar month the FY begins (4 = April in India).
-- @fy@ is the FY's /starting/ calendar year, so 2025 means 2025-04-01 .. 2026-03-31.
--
-- Offsets are always applied to the first of a month, so clip-vs-rollover
-- never differ.
fyPeriodRange :: Int -> Int -> FyPeriod -> (Day, Day)
fyPeriodRange fyStartMonth fy period =
  let (mFrom, mTo) = fyPeriodMonths period
      fyStart = fromGregorian (fromIntegral fy) fyStartMonth 1
      from = addGregorianMonthsClip mFrom fyStart
      to = addDays (-1) (addGregorianMonthsClip mTo fyStart)
   in (from, to)

-- | The financial year a local date falls in, named by its starting calendar
-- year. A date in Jan-Mar belongs to the FY that started the previous calendar
-- year.
financialYearOf :: Int -> Day -> Int
financialYearOf fyStartMonth day =
  let (y, m, _) = toGregorian day
   in fromIntegral (if m >= fyStartMonth then y else y - 1)

-- | The quarter (1..4) a local date falls in, relative to the financial year.
quarterOf :: Int -> Day -> Int
quarterOf fyStartMonth day =
  let (_, m, _) = toGregorian day
      monthsSinceFyStart = (12 + m - fyStartMonth) `mod` 12
   in (monthsSinceFyStart `div` 3) + 1

-- | The financial year and quarter a local date falls in — the bucket a ride
-- accumulates into. Always derived from the ride's own local date, never from
-- @now@, so a late or replayed ride lands in the period it belongs to.
fyAndQuarterOf :: Int -> Day -> (Int, Int)
fyAndQuarterOf fyStartMonth day = (financialYearOf fyStartMonth day, quarterOf fyStartMonth day)
