module SharedLogic.Allocator.Jobs.UpdateRecurringBookingTimetable where

import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime (..))
import Data.Time.LocalTime (timeToTimeOfDay)
import qualified Domain.Types.RecurringBooking as DRecurringBooking
import qualified Domain.Types.Timetable as DTimetable
import Kernel.Prelude
import Kernel.Storage.Esqueleto (runTransaction)
import Kernel.Utils.Common
import qualified Storage.Queries.RecurringBooking as QRecurringBooking
import qualified Storage.Queries.Timetable as QTimetable

timetablesToGenerate :: Int
timetablesToGenerate = 5

updateRecurringBookingTimetable :: EsqDBFlow m r => m ()
updateRecurringBookingTimetable = do
  now <- getCurrentTime
  bookings <- QRecurringBooking.findAllActiveOnDate (utctDay now)
  bookingTimetables <- traverse (generateNextTimetables timetablesToGenerate now) bookings
  runTransaction $ QTimetable.insertTimetables $ concat bookingTimetables

generateNextTimetables :: MonadFlow m => Int -> UTCTime -> DRecurringBooking.RecurringBooking -> m [DTimetable.Timetable]
generateNextTimetables count now booking =
  DRecurringBooking.scheduledDates booking
    & dropWhile isPastRide
    & take count
    & traverse (makeTimetableForDay booking)
  where
    isPastRide day =
      day < utctDay now
        || ( day == utctDay now
               && booking.pickupTime > timeToTimeOfDay (utctDayTime now)
           )

makeTimetableForDay :: MonadFlow m => DRecurringBooking.RecurringBooking -> Day -> m DTimetable.Timetable
makeTimetableForDay booking day = do
  timetableId <- generateGUID
  pure $
    DTimetable.Timetable
      { id = timetableId,
        recurringBookingId = booking.id,
        pickupDate = day,
        pickupTime = booking.pickupTime,
        status = DTimetable.Active
      }
