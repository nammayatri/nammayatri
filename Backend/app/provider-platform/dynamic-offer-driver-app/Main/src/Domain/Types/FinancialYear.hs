module Domain.Types.FinancialYear where

import Data.Time.Calendar
import Kernel.Prelude

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
