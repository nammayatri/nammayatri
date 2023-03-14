module Domain.Types.Timetable where

import Data.Time.Calendar (Day)
import Data.Time.LocalTime (LocalTime)
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Booking.BookingLocation as DLoc
import qualified Domain.Types.FarePolicy as DFarePolicy
import qualified Domain.Types.RecurringBooking as DRBooking
import Kernel.Prelude
import Kernel.Types.Id

data Status
  = Active
  | Cancelled
  deriving (Generic, Show, Read)

data Timetable = Timetable
  { id :: Id Timetable,
    recurringBookingId :: Id DRBooking.RecurringBooking,
    bookingId :: Maybe (Id DBooking.Booking),
    pickupTime :: LocalTime,
    status :: Status
  }
  deriving (Generic, Show)

data UpcomingBooking = UpcomingBooking
  { id :: Id Timetable,
    pickupTime :: LocalTime,
    recurringBookingId :: Id DRBooking.RecurringBooking,
    bookingId :: Maybe (Id DBooking.Booking),
    providerId :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    farePolicy :: DFarePolicy.FarePolicy,
    fromLocation :: DLoc.BookingLocation,
    toLocation :: DLoc.BookingLocation
  }
