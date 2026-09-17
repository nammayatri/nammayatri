module Lib.Payment.Offer.Frequency
  ( Window (..),
    allOfferFrequencies,
    windowOf,
  )
where

import qualified Data.Time as Time
import qualified Data.Time.Calendar.WeekDate as Week
import Kernel.Prelude
import Kernel.Types.Common (Seconds (..))
import Lib.Payment.Domain.Types.Offer (OfferFrequency (..))

data Window = Window
  { windowStart :: UTCTime,
    windowEnd :: UTCTime
  }
  deriving (Eq, Show)

allOfferFrequencies :: [OfferFrequency]
allOfferFrequencies = [HOURLY, DAILY, WEEKLY, MONTHLY]

-- | The clock hour, calendar day, ISO week or calendar month containing @now@, in the city's local time.
windowOf :: OfferFrequency -> Seconds -> UTCTime -> Window
windowOf frequency timeDiffFromUtc now =
  let offset = fromIntegral (getSeconds timeDiffFromUtc) :: NominalDiffTime
      localNow = Time.addUTCTime offset now
      localDay = Time.utctDay localNow
      dayStart d = Time.UTCTime d 0
      (startLocal, endLocal) = case frequency of
        HOURLY ->
          let hour = floor (Time.utctDayTime localNow / 3600) :: Integer
              start = Time.UTCTime localDay (fromInteger (hour * 3600))
           in (start, Time.addUTCTime 3600 start)
        DAILY -> (dayStart localDay, dayStart (Time.addDays 1 localDay))
        WEEKLY ->
          let (_, _, weekday) = Week.toWeekDate localDay -- Monday = 1 .. Sunday = 7
              monday = Time.addDays (negate (toInteger (weekday - 1))) localDay
           in (dayStart monday, dayStart (Time.addDays 7 monday))
        MONTHLY ->
          let (year, month, _) = Time.toGregorian localDay
              firstOfMonth = Time.fromGregorian year month 1
           in (dayStart firstOfMonth, dayStart (Time.addGregorianMonthsClip 1 firstOfMonth))
      toUtc = Time.addUTCTime (negate offset)
   in Window (toUtc startLocal) (toUtc endLocal)
