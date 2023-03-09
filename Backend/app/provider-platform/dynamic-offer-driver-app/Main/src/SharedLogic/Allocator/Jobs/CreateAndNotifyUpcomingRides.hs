module SharedLogic.Allocator.Jobs.CreateAndNotifyUpcomingRides where

import Data.Time.LocalTime
import qualified Domain.Types.Booking as Booking
import qualified Domain.Types.TimeRange as TimeRange
import qualified Domain.Types.Timetable as Timetable
import qualified Domain.Types.Vehicle as Vehicle
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FareCalculator (calculateFareParameters)
import Storage.Queries.Timetable as QTimetable

getCurrentLocalTime :: MonadTime m => TimeZone -> m LocalTime
getCurrentLocalTime tz = do
  now <- getCurrentTime
  pure $ utcToLocalTime tz now

handle :: EsqDBFlow m r => m ()
handle = do
  timeZone <- liftIO getCurrentTimeZone
  now <- getCurrentLocalTime timeZone
  let oneHour = 60 * 60
      timeRange = TimeRange.fromTimeAndDuration now oneHour
  Esq.runTransaction $ do
    timetables <- QTimetable.findAllUnscheduledAndActiveDuring timeRange
    bookings <- traverse (makeScheduledBooking timeZone) timetables
    Esq.createMany bookings
    pure ()

makeScheduledBooking ::
  MonadTime m =>
  MonadGuid m =>
  TimeZone ->
  Timetable.UpcomingBooking ->
  m Booking.SimpleBooking
makeScheduledBooking timeZone timetable = do
  bookingId <- generateGUID
  now <- getCurrentTime
  let estimatedDistance = error "TODO: call google to calculate trip"
      estimatedDuration = error "TODO: call google to calculate trip"
      variant = timetable.farePolicy.vehicleVariant
  fareParams <- calculateFareParameters timetable.farePolicy estimatedDistance estimatedDuration Nothing
  let estimatedFare = error "calculate estimated fare"
  pure $
    Booking.SimpleBooking
      { id = bookingId,
        status = Booking.NEW,
        quoteId = error "TODO: What do for quote",
        providerId = Id timetable.providerId,
        bapId = timetable.bapId,
        bapUri = timetable.bapUri,
        startTime = localTimeToUTC timeZone timetable.pickupTime,
        riderId = Nothing,
        fromLocation = timetable.fromLocation,
        toLocation = timetable.toLocation,
        vehicleVariant = variant,
        estimatedDistance = estimatedDistance,
        estimatedDuration = estimatedDuration,
        estimatedFare = estimatedFare,
        fareParams = fareParams.id,
        riderName = Nothing,
        createdAt = now,
        updatedAt = now
      }
